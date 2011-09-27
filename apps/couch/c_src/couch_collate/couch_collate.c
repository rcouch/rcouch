/*

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License. You may obtain a copy of the
License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied. See the License for the
specific language governing permissions and limitations under the License.

*/

#ifdef DARWIN
#define U_HIDE_DRAFT_API 1
#define U_DISABLE_RENAMING 1
#endif

#include "erl_nif.h"
#include "unicode/ucol.h"
#include "unicode/ucasemap.h"
#include <stdio.h>
#include <assert.h>

static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_NULL;

typedef struct {
    ErlNifEnv* env;
    int error;
    UCollator* coll;
} ctx_t;

typedef struct {
    UCollator** collators;
    int collStackTop;
    int numCollators;
    ErlNifMutex* collMutex;
} priv_data_t;

static ERL_NIF_TERM collate_nif(ErlNifEnv*, int, const ERL_NIF_TERM []);
static int collate_binary(priv_data_t*, ctx_t*, ERL_NIF_TERM, ERL_NIF_TERM, ERL_NIF_TERM);
static int on_load(ErlNifEnv*, void**, ERL_NIF_TERM);
static void on_unload(ErlNifEnv*, void*);
static __inline void reserve_coll(priv_data_t*, ctx_t*);
static __inline void release_coll(priv_data_t*, ctx_t*);

void
reserve_coll(priv_data_t* pData, ctx_t *ctx)
{
    if (ctx->coll == NULL) {
        enif_mutex_lock(pData->collMutex);
        assert(pData->collStackTop < pData->numCollators);
        ctx->coll = pData->collators[pData->collStackTop];
        pData->collStackTop += 1;
        enif_mutex_unlock(pData->collMutex);
    }
}


void
release_coll(priv_data_t* pData, ctx_t *ctx)
{
    if (ctx->coll != NULL) {
        enif_mutex_lock(pData->collMutex);
        pData->collStackTop -= 1;
        assert(pData->collStackTop >= 0);
        enif_mutex_unlock(pData->collMutex);
    }
}

/* ------------------------------------------------------------------------- */

static ERL_NIF_TERM
collate_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM term_a          = argv[0];
    ERL_NIF_TERM term_b          = argv[1];
    ERL_NIF_TERM term_has_nocase = argv[2];

    ctx_t ctx;
    int result;

    ctx.env = env;
    ctx.error = 0;
    ctx.coll = NULL;

    priv_data_t* pData = (priv_data_t*) enif_priv_data(env);

    result = collate_binary(pData, &ctx, term_a, term_b, term_has_nocase);
    release_coll(pData, &ctx);

    return enif_make_int(env, result);
}

int
collate_binary(priv_data_t* pData, ctx_t* ctx, ERL_NIF_TERM term_a, ERL_NIF_TERM term_b, ERL_NIF_TERM term_has_nocase)
{
    ErlNifBinary binA, binB;
    int has_nocase, response;

    if(!enif_get_int(ctx->env, term_has_nocase, &has_nocase)) {
        ctx->error = 1;
        return 0;
    }
    if(!enif_inspect_binary(ctx->env, term_a, &binA)) {
        ctx->error = 1;
        return 0;
    }
    if(!enif_inspect_binary(ctx->env, term_b, &binB)) {
        ctx->error = 1;
        return 0;
    }

    switch(has_nocase) {
    case 0: /* COLLATE */
    case 1: /* COLLATE_NO_CASE: */
        {
        UErrorCode status = U_ZERO_ERROR;
        UCharIterator iterA;
        UCharIterator iterB;

        uiter_setUTF8(&iterA, (const char *) binA.data, (uint32_t) binA.size);
        uiter_setUTF8(&iterB, (const char *) binB.data, (uint32_t) binB.size);

        /* grab a collator */
        reserve_coll(pData, ctx);

        if (has_nocase == 1) /* switching this collator to case insensitive */
          ucol_setAttribute(ctx->coll, UCOL_STRENGTH, UCOL_PRIMARY, &status);

        /* by default, it will collate case sensitive */
        response = ucol_strcollIter(ctx->coll, &iterA, &iterB, &status);

        if (has_nocase == 1) /* puting back this collator to case sensitive */
          ucol_setAttribute(ctx->coll, UCOL_STRENGTH, UCOL_DEFAULT, &status);

        break;
        }

    default:
        response = -1;
    }

    return response;
}

/* ------------------------------------------------------------------------- */

int
on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info)
{
    UErrorCode status = U_ZERO_ERROR;
    priv_data_t* pData = (priv_data_t*)enif_alloc(sizeof(priv_data_t));
    int i, j;

    /* Initialize the structure */
    pData->collators = NULL;
    pData->collStackTop = 0;
    pData->numCollators = 0;
    pData->collMutex = NULL;

    if (!enif_get_int(env, info, &(pData->numCollators) )) {
        enif_free((char*)pData);
        return 1;
    }

    if (pData->numCollators < 1) {
        enif_free((char*)pData);
        return 2;
    }

    pData->collMutex = enif_mutex_create("coll_mutex");

    if (pData->collMutex == NULL) {
        enif_free((char*)pData);
        return 3;
    }

    pData->collators = enif_alloc(sizeof(UCollator*) * pData->numCollators);

    if (pData->collators == NULL) {
        enif_mutex_destroy(pData->collMutex);
        enif_free((char*)pData);
        return 4;
    }

    for (i = 0; i < pData->numCollators; i++) {
        pData->collators[i] = ucol_open("", &status);

        if (U_FAILURE(status)) {
            for (j = 0; j < i; j++) {
                ucol_close(pData->collators[j]);
            }

            enif_free(pData->collators);
            enif_mutex_destroy(pData->collMutex);

            enif_free((char*)pData);

            return 5;
        }
    }

    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");
    ATOM_NULL = enif_make_atom(env, "null");

    *priv_data = pData;

    return 0;
}


void
on_unload(ErlNifEnv* env, void* priv_data)
{
    priv_data_t* pData = (priv_data_t*)priv_data;
    if (pData->collators != NULL) {
        int i;

        for (i = 0; i < pData->numCollators; i++) {
            ucol_close(pData->collators[i]);
        }

        enif_free(pData->collators);
    }

    if (pData->collMutex != NULL) {
        enif_mutex_destroy(pData->collMutex);
    }

    enif_free((char*)pData);
}

int
on_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info)
{
    return 0;
}

int
on_upgrade(ErlNifEnv* env, void** priv_data, void** old_data, ERL_NIF_TERM info)
{
    return 0;
}

/* ------------------------------------------------------------------------- */

static ErlNifFunc
nif_funcs[] =
{
    {"collate_nif", 3, collate_nif}
};

ERL_NIF_INIT(couch_collate, nif_funcs, &on_load, &on_reload, &on_upgrade, &on_unload);
