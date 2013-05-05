/*
 *  Author: Jan Lehnardt <jan@apache.org>
 *  This is Apache 2.0 licensed free software
 */
#import "CouchDBXApplicationController.h"

#import "iniparser.h"

@implementation CouchDBXApplicationController

-(BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)app
{
  return YES;
}

-(void)applicationWillTerminate:(NSNotification *)notification
{
	[self ensureFullCommit];
    
}

- (void)windowWillClose:(NSNotification *)aNotification 
{
    [self stop];
}

-(void)applicationWillFinishLaunching:(NSNotification *)notification
{

}

/* found at http://www.cocoadev.com/index.pl?ApplicationSupportFolder */
- (NSString *)applicationSupportFolder:(NSString*)appName {
    NSString *applicationSupportFolder = nil;
    FSRef foundRef;
    OSErr err = FSFindFolder(kUserDomain, kApplicationSupportFolderType, kDontCreateFolder, &foundRef);
    if (err == noErr) {
        unsigned char path[PATH_MAX];
        OSStatus validPath = FSRefMakePath(&foundRef, path, sizeof(path));
        if (validPath == noErr) {
            applicationSupportFolder = [[NSFileManager defaultManager] stringWithFileSystemRepresentation:(const char *)path
                                                                                                   length:(NSUInteger)strlen((char*)path)];
        }
    }
	applicationSupportFolder = [applicationSupportFolder stringByAppendingPathComponent:appName];
    return applicationSupportFolder;
}

- (NSString *)applicationSupportFolder {
    return [self applicationSupportFolder:@"rcouchx"];
}

-(NSMutableString *)uriFile
{    
    NSMutableString *urifile = [[NSMutableString alloc] init];
    
    [urifile appendString:[self applicationSupportFolder]];
    [urifile appendString:@"/couch.uri"];
    return urifile;
}

-(NSMutableString *) finalConfigPath
{
    NSMutableString *iniFile = [[NSMutableString alloc] init];;
    [iniFile appendString:[[NSBundle mainBundle] resourcePath]];
    [iniFile appendString:@"/rcouch/etc/local.ini"];
    return iniFile;
}

-(void)ensureFullCommit
{    
	// get couch uri
	NSString *uri = [NSString stringWithContentsOfFile:[self uriFile] encoding:NSUTF8StringEncoding error:NULL];
    uri = [uri substringToIndex:[uri length] - 1];

	// TODO: maybe parse out \n

	// get database dir
	NSString *databaseDir = [self applicationSupportFolder];

	// get ensure_full_commit.sh
	NSMutableString *ensure_full_commit_script = [[NSMutableString alloc] init];
	[ensure_full_commit_script appendString: [[NSBundle mainBundle] resourcePath]];
	[ensure_full_commit_script appendString: @"/ensure_full_commit.sh"];

	// exec ensure_full_commit.sh database_dir couch.uri
	NSArray *args = [[NSArray alloc] initWithObjects:databaseDir, uri, nil];
	NSTask *commitTask = [[NSTask alloc] init];
	[commitTask setArguments: args];
	[commitTask launch];
	[commitTask waitUntilExit];

}


-(void)awakeFromNib
{
    [browse setEnabled:NO];
	
	NSLayoutManager *lm;
    
	lm = [outputView layoutManager];
	[lm setDelegate:(id < NSLayoutManagerDelegate >) self];
	
	[webView setUIDelegate:self];
	
	[self launchCouchDB];
}

-(IBAction)start:(id)sender
{
    if([task isRunning]) {
      [self stop];
      return;
    } 
    
    [self launchCouchDB];
}

-(void)deleteUri
{
    [[NSFileManager defaultManager] removeItemAtPath:[self uriFile] error:NULL];
}

-(void)stop
{
    NSFileHandle *writer;
    writer = [in fileHandleForWriting];
    [writer writeData:[@"q().\n" dataUsingEncoding:NSASCIIStringEncoding]];
    [writer closeFile];
  
    [browse setEnabled:NO];
    [start setImage:[NSImage imageNamed:@"start.png"]];
    [start setLabel:@"start"];
    
    [self deleteUri];
    
}

-(void)waitStartup
{
    while(1) {
        if(![[NSFileManager defaultManager] fileExistsAtPath:[self uriFile]]) {
            sleep(1);
        } else {
            break;
        }
    }
}

-(void)setInitParams
{
    
	// determine data dir
	NSString *dataDir = [self applicationSupportFolder];
    
	// create if it doesn't exist
	if(![[NSFileManager defaultManager] fileExistsAtPath:dataDir]) {
		[[NSFileManager defaultManager] createDirectoryAtPath:dataDir withIntermediateDirectories:YES attributes:nil error:NULL];
	}
    
    // delete uri file if needed
    [self deleteUri];
    
    dictionary* iniDict = iniparser_load([[self finalConfigPath] UTF8String]);
    if (iniDict == NULL) {
        iniDict = dictionary_new(0);
        assert(iniDict);
    }
    
    dictionary_set(iniDict, "couchdb", NULL);
    if (iniparser_getstring(iniDict, "couchdb:database_dir", NULL) == NULL) {
        dictionary_set(iniDict, "couchdb:database_dir", [dataDir UTF8String]);
    }
    if (iniparser_getstring(iniDict, "couchdb:view_index_dir", NULL) == NULL) {
        dictionary_set(iniDict, "couchdb:view_index_dir", [dataDir UTF8String]);
    }
    
    if (iniparser_getstring(iniDict, "couchdb:uri_file", NULL) == NULL) {
        dictionary_set(iniDict, "couchdb:uri_file", [[self uriFile] UTF8String]);
    }

    NSString *tmpfile = [NSString stringWithFormat:@"%@.tmp", [self finalConfigPath]];
    FILE *f = fopen([tmpfile UTF8String], "w");
    if (f) {
        iniparser_dump_ini(iniDict, f);
        fclose(f);
        rename([tmpfile UTF8String], [[self finalConfigPath] UTF8String]);
    } else {
        NSLog(@"Can't write to temporary config file:  %@:  %s\n", tmpfile, strerror(errno));
    }
    
    iniparser_freedict(iniDict);
}

-(void)launchCouchDB
{
	[self setInitParams];
    [browse setEnabled:YES];
    [start setImage:[NSImage imageNamed:@"stop.png"]];
    [start setLabel:@"stop"];


	in = [[NSPipe alloc] init];
	out = [[NSPipe alloc] init];
	task = [[NSTask alloc] init];

	NSMutableString *launchPath = [[NSMutableString alloc] init];
	[launchPath appendString:[[NSBundle mainBundle] resourcePath]];
	[launchPath appendString:@"/rcouch"];
	[task setCurrentDirectoryPath:launchPath];

	[launchPath appendString:@"/bin/rcouch"];
	[task setLaunchPath:launchPath];
	NSArray *args = [[NSArray alloc] initWithObjects:@"console", nil];
	[task setArguments:args];
	[task setStandardInput:in];
	[task setStandardOutput:out];

	NSFileHandle *fh = [out fileHandleForReading];
	NSNotificationCenter *nc;
	nc = [NSNotificationCenter defaultCenter];

	[nc addObserver:self
					selector:@selector(dataReady:)
							name:NSFileHandleReadCompletionNotification
						 object:fh];
	
	[nc addObserver:self
					selector:@selector(taskTerminated:)
							name:NSTaskDidTerminateNotification
						object:task];

  	[task launch];
  	[outputView setString:@"Starting rcouch...\n"];
  	[fh readInBackgroundAndNotify];
    
    [self waitStartup];
	[self openFuton];
	
}

-(void)taskTerminated:(NSNotification *)note
{
    [self cleanup];
}

-(void)cleanup
{
    [task release];
    task = nil;
    
    [in release];
    in = nil;
		[out release];
		out = nil;

    [[NSNotificationCenter defaultCenter] removeObserver:self];
    
    [self deleteUri];
}

-(void)openFuton
{
	// get couch uri
	NSString *uri = [NSString stringWithContentsOfFile:[self uriFile] encoding:NSUTF8StringEncoding error:NULL];
    uri = [uri substringToIndex:[uri length] - 1];

    NSMutableString *futonUri = [[NSMutableString alloc] init];
    [futonUri appendString:uri];
    [futonUri appendString:@"_utils/"];
    
	[webView setTextSizeMultiplier:1.3];
	[[webView mainFrame] loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:futonUri]]];
    
}

-(IBAction)browse:(id)sender
{
	[self openFuton];
}

- (void)appendData:(NSData *)d
{
    NSString *s = [[NSString alloc] initWithData: d
                                        encoding: NSUTF8StringEncoding];
    NSTextStorage *ts = [outputView textStorage];
    [ts replaceCharactersInRange:NSMakeRange([ts length], 0) withString:s];
    [s release];
}

- (void)dataReady:(NSNotification *)n
{
    NSData *d;
    d = [[n userInfo] valueForKey:NSFileHandleNotificationDataItem];
    if ([d length]) {
      [self appendData:d];
    }
    if (task)
      [[out fileHandleForReading] readInBackgroundAndNotify];
    
    
}

- (void)layoutManager:(NSLayoutManager *)aLayoutManager didCompleteLayoutForTextContainer:(NSTextContainer *)aTextContainer atEnd:(BOOL)flag
{
	if (flag) {
		NSTextStorage *ts = [outputView textStorage];
		[outputView scrollRangeToVisible:NSMakeRange([ts length], 0)];
	}
}

- (void)webView:(WebView *)sender runOpenPanelForFileButtonWithResultListener:(id < WebOpenPanelResultListener >)resultListener
{
	[self openChooseFileDialogWithListener: resultListener
			allowMultipleFiles: FALSE];
}
- (void)webView:(WebView *)sender runOpenPanelForFileButtonWithResultListener:(id < WebOpenPanelResultListener >)resultListener allowMultipleFiles:(BOOL)allowMultipleFiles
{
	[self openChooseFileDialogWithListener: resultListener
			allowMultipleFiles: allowMultipleFiles];
}

#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 1060 
	#define MULTIPLE_SELECTION_POSSIBLE TRUE
#else
	#define MULTIPLE_SELECTION_POSSIBLE FALSE
#endif
- (void)openChooseFileDialogWithListener: (id < WebOpenPanelResultListener >)resultListener allowMultipleFiles: (BOOL)multipleSelection
{
	NSOpenPanel* openDlg = [NSOpenPanel openPanel];
	[openDlg setCanChooseFiles:YES];
	[openDlg setCanChooseDirectories:NO];
	[openDlg setAllowsMultipleSelection: (multipleSelection && MULTIPLE_SELECTION_POSSIBLE)];
	NSInteger result = [openDlg runModal];
	if (result == NSFileHandlingPanelOKButton) {
		NSArray* files = [openDlg URLs];
#if MULTIPLE_SELECTION_POSSIBLE
		NSInteger filesNumber = [files count];
		if (filesNumber == 1) {
#endif
			NSURL* fileURL = [files objectAtIndex:0];
			NSString* path = [fileURL path];
			[resultListener chooseFilename:path ];
#if MULTIPLE_SELECTION_POSSIBLE			
		} else {
			NSMutableArray* fileNames = [NSMutableArray arrayWithCapacity:filesNumber];
			for (NSURL* fileURL in files) {
				NSString* path = [fileURL path];
				[fileNames addObject:path];
			}
			[resultListener chooseFilenames: fileNames];
		} 
#endif		
	} else {
		[resultListener cancel];
	}
}

- (void)webView:(WebView *)sender runJavaScriptAlertPanelWithMessage:(NSString *)message initiatedByFrame:(WebFrame *)frame
{
	NSRunInformationalAlertPanel(nil, message, nil, nil, nil);
}

@end
