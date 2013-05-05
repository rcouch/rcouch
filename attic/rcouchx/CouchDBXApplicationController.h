/*
    Author: Jan Lehnardt <jan@apache.org>
    This is Apache 2.0 licensed free software
*/
#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

@interface CouchDBXApplicationController : NSObject{
    IBOutlet NSToolbarItem *start;
    IBOutlet NSToolbarItem *browse;
	IBOutlet NSTextView *outputView;
	IBOutlet WebView *webView;
    
    NSTask *task;
    NSPipe *in, *out;
}

-(IBAction)start:(id)sender;
-(IBAction)browse:(id)sender;

-(void)launchCouchDB;
-(void)stop;
-(void)taskTerminated:(NSNotification *)note;
-(void)cleanup;
-(void)openFuton;
-(void)openChooseFileDialogWithListener: (id < WebOpenPanelResultListener >)resultListener allowMultipleFiles: (BOOL)multipleSelection;
-(void)ensureFullCommit;
-(NSString *)applicationSupportFolder;

@end
