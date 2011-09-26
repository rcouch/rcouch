// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <jsapi.h>
#include "utf8.h"
#include "util.h"


#define SETUP_REQUEST(cx) \
    JS_SetContextThread(cx); \
    JS_BeginRequest(cx);
#define FINISH_REQUEST(cx) \
    JS_EndRequest(cx); \
    JS_ClearContextThread(cx);


static JSClass global_class = {
    "GlobalClass",
    JSCLASS_GLOBAL_FLAGS,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_StrictPropertyStub,
    JS_EnumerateStub,
    JS_ResolveStub,
    JS_ConvertStub,
    JS_FinalizeStub,
    JSCLASS_NO_OPTIONAL_MEMBERS
};

static JSBool
evalcx(JSContext *cx, uintN argc, jsval* vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    JSString* str;
    JSObject* sandbox;
    JSObject* global;
    JSContext* subcx;
    JSCrossCompartmentCall* call = NULL;
    const jschar* src;
    size_t srclen;
    jsval rval;
    JSBool ret = JS_FALSE;

    sandbox = NULL;
    if(!JS_ConvertArguments(cx, argc, argv, "S / o", &str, &sandbox)) {
        return JS_FALSE;
    }

    subcx = JS_NewContext(JS_GetRuntime(cx), 8L * 1024L);
    if(!subcx) {
        JS_ReportOutOfMemory(cx);
        return JS_FALSE;
    }

    SETUP_REQUEST(subcx);

    src = JS_GetStringCharsAndLength(cx, str, &srclen);

    // Re-use the compartment associated with the main context,
    // rather than creating a new compartment */
    global = JS_GetGlobalObject(cx);
    if(global == NULL) goto done;
    call = JS_EnterCrossCompartmentCall(subcx, global);

    if(!sandbox) {
        sandbox = JS_NewGlobalObject(subcx, &global_class);
        if(!sandbox || !JS_InitStandardClasses(subcx, sandbox)) {
            goto done;
        }
    }

    if(srclen == 0) {
        JS_SET_RVAL(cx, vp, OBJECT_TO_JSVAL(sandbox));
    } else {
        JS_EvaluateUCScript(subcx, sandbox, src, srclen, NULL, 0, &rval);
        JS_SET_RVAL(cx, vp, rval);
    }
    
    ret = JS_TRUE;

done:
    JS_LeaveCrossCompartmentCall(call);
    FINISH_REQUEST(subcx);
    JS_DestroyContext(subcx);
    return ret;
}


static JSBool
gc(JSContext* cx, uintN argc, jsval* vp)
{
    JS_GC(cx);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}


static JSBool
print(JSContext* cx, uintN argc, jsval* vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    couch_print(cx, argc, argv);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}


static JSBool
quit(JSContext* cx, uintN argc, jsval* vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int exit_code = 0;
    JS_ConvertArguments(cx, argc, argv, "/i", &exit_code);
    exit(exit_code);
}


static JSBool
readline(JSContext* cx, uintN argc, jsval* vp)
{
    JSString* line;

    /* GC Occasionally */
    JS_MaybeGC(cx);

    line = couch_readline(cx, stdin);
    if(line == NULL) return JS_FALSE;

    JS_SET_RVAL(cx, vp, STRING_TO_JSVAL(line));
    return JS_TRUE;
}


static JSBool
seal(JSContext* cx, uintN argc, jsval* vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    JSObject *target;
    JSBool deep = JS_FALSE;
    JSBool ret;

    if(!JS_ConvertArguments(cx, argc, argv, "o/b", &target, &deep))
        return JS_FALSE;

    if(!target) {
        JS_SET_RVAL(cx, vp, JSVAL_VOID);
        return JS_TRUE;
    }

    
    ret = deep ? JS_DeepFreezeObject(cx, target) : JS_FreezeObject(cx, target);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return ret;
}

static JSFunctionSpec global_functions[] = {
    JS_FS("evalcx", evalcx, 0, 0),
    JS_FS("gc", gc, 0, 0),
    JS_FS("print", print, 0, 0),
    JS_FS("quit", quit, 0, 0),
    JS_FS("readline", readline, 0, 0),
    JS_FS("seal", seal, 0, 0),
    JS_FS_END
};


int
main(int argc, const char* argv[])
{
    JSRuntime* rt = NULL;
    JSContext* cx = NULL;
    JSObject* global = NULL;
    JSCrossCompartmentCall *call = NULL;
    JSObject* klass = NULL;
    JSObject* script;
    JSString* scriptsrc;
    const jschar* schars;
    size_t slen;
    jsval sroot;
    jsval result;

    couch_args* args = couch_parse_args(argc, argv);

    rt = JS_NewRuntime(64L * 1024L * 1024L);
    if(rt == NULL)
        return 1;

    cx = JS_NewContext(rt, args->stack_size);
    if(cx == NULL)
        return 1;

    JS_SetErrorReporter(cx, couch_error);
    JS_ToggleOptions(cx, JSOPTION_XML);
    
    SETUP_REQUEST(cx);

    global = JS_NewCompartmentAndGlobalObject(cx, &global_class, NULL);
    if(global == NULL)
        return 1;

    call = JS_EnterCrossCompartmentCall(cx, global);

    JS_SetGlobalObject(cx, global);
    
    if(!JS_InitStandardClasses(cx, global))
        return 1;

    if(couch_load_funcs(cx, global, global_functions) != JS_TRUE)
        return 1;

    // Convert script source to jschars.
    scriptsrc = dec_string(cx, args->script, strlen(args->script));
    if(!scriptsrc)
        return 1;

    schars = JS_GetStringCharsAndLength(cx, scriptsrc, &slen);
    
    // Root it so GC doesn't collect it.
    sroot = STRING_TO_JSVAL(scriptsrc); 
    if(JS_AddValueRoot(cx, &sroot) != JS_TRUE) {
        fprintf(stderr, "Internal root error.\n");
        return 1;
    }

    // Compile and run
    script = JS_CompileUCScript(cx, global, schars, slen, args->script_name, 1);
    if(!script) {
        fprintf(stderr, "Failed to compile script.\n");
        return 1;
    }
    
    JS_ExecuteScript(cx, global, script, &result);

    // Warning message if we don't remove it.
    JS_RemoveValueRoot(cx, &sroot);

    JS_LeaveCrossCompartmentCall(call);
    FINISH_REQUEST(cx);
    JS_DestroyContext(cx);
    JS_DestroyRuntime(rt);
    JS_ShutDown();

    return 0;
}
