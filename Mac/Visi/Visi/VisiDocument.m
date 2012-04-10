//
//  VisiDocument.m
//  Visi
//
//  Created by David Pollak on 2/9/12.
//  Copyright (c) 2012 David Pollak. All rights reserved.
//

#import "VisiDocument.h"
#import "RunCommandOnMainThread.h"

void setText(NSTextView *tv, NSString *txt);

void setText(NSTextView *tv, NSString *txt) {
    NSString *cur =  [[tv textStorage] string];
    [[tv textStorage] replaceCharactersInRange: NSMakeRange(0, [cur length]) 
                                    withString:txt];
}

void freeEvent(visi_event *evt) {
    switch (evt -> cmd) {
        case reportErrorEvent:
            if (evt -> evtInfo.errorText != Nil) free(evt -> evtInfo.errorText);
            break;
            
        case removeSourceEvent:
            if (evt -> evtInfo.sourceSinkName != Nil) free(evt -> evtInfo.sourceSinkName);
            break;
            
        case removeSinkEvent:
            if (evt -> evtInfo.sourceSinkName != Nil) free(evt -> evtInfo.sourceSinkName);
            break;
            
        case addSourceEvent:
            if (evt -> evtInfo.sourceSinkName != Nil) free(evt -> evtInfo.sourceSinkName);
            break;
            
        case addSinkEvent:
            if (evt -> evtInfo.sourceSinkName != Nil) free(evt -> evtInfo.sourceSinkName);
            break;

        case setSinkEvent:
            if (evt -> evtInfo.sourceSinkName != Nil) free(evt -> evtInfo.sourceSinkName);
            if (evt -> eventType == stringVisiType && evt -> evtValue.text != Nil) {
                free(evt -> evtValue.text);
            }
            break;
    }
    free(evt);
}

void sendEvent(const void *theId, visi_event *evt) {
    id target = (__bridge id) theId;
    RunCommandOnMainThread *rc = [[RunCommandOnMainThread alloc] init:target event:evt];
    [rc run];
}

@implementation VisiDocument

@synthesize editor;
@synthesize sourceControls;
@synthesize sinkControls;
@synthesize errorInfo;
@synthesize base;

- (IBAction)runCode:(id) sender {
    NSString *theCode = [[editor textStorage] string];
    visi_command cmd;
    setText(errorInfo, @"Starting Model");
    cmd.cmd = setProgramTextCmd;
    cmd.cmdInfo.text = [theCode UTF8String];
    callIntoVisi(&cmd);
}

/*
- (IBAction)grabBool:(id)sender {
    NamedSwitch *sw = sender;
    BOOL b = sw.on;
    NSString *ns = sw.name;
    setSourceString([ns cStringUsingEncoding:[NSString defaultCStringEncoding]], BoolSourceType, b ? "t" : "0");
}

- (IBAction)grabText:(id)sender {
    NamedText *sw = sender;
    NSString *b = sw.text;
    NSString *ns = sw.name;
    setSourceString([ns cStringUsingEncoding:[NSString defaultCStringEncoding]], [sw delegate] == nil ? StringSourceType : NumberSourceType, 
                    [b cStringUsingEncoding:[NSString defaultCStringEncoding]]);
}
*/

- (IBAction)grabBool:(id)sender {
    /*
    NamedSwitch *sw = sender;
    BOOL b = sw.on;
    NSString *ns = sw.name;
    setSourceString([ns cStringUsingEncoding:[NSString defaultCStringEncoding]], BoolSourceType, b ? "t" : "0");
     */
}

- (IBAction)grabText:(id)sender {
    
}

- (NSTextField *)findSink:(NSString *)name {
    return nil;
}

- (void) layoutControls {
    
}

- (id)init
{
    self = [super init];
    callIntoVisi = Nil;
    if (self) {
        self.base = @"";
        callIntoVisi = startProcess((__bridge void *) self);
    }
    return self;
}

- (void)runEvent:(visi_event *)event {
    switch (event -> cmd) {
        case reportErrorEvent:
        {
            char *str = event -> evtInfo.errorText;
            if (str) {
                setText(errorInfo, [[NSString alloc] initWithUTF8String:str]);
            } else {
                setText(errorInfo, @"");
            }
        }
            break;

        case removeSourceEvent:
            // FIXME remove source
            break;
            
        case removeSinkEvent:
            // FIXME remove sink
            break;
            
        case addSourceEvent:
            // FIXME add source
            break;
            
        case addSinkEvent:
            // FIXME add sink
            break;
            
        case setSinkEvent:
            // FIXME set sink value
            break;
            
        default:
            break;
    }
}


- (NSString *)windowNibName
{
    // Override returning the nib file name of the document
    // If you need to use a subclass of NSWindowController or if your document supports multiple NSWindowControllers, you should remove this method and override -makeWindowControllers instead.
    return @"VisiDocument";
}

- (void)windowControllerDidLoadNib:(NSWindowController *)aController
{
    [super windowControllerDidLoadNib:aController];
    setText(editor, base);
    [editor setAutomaticSpellingCorrectionEnabled:NO];
    [self runCode: self];
    // Add any code here that needs to be executed once the windowController has loaded the document's window.
}

- (void) windowWillClose: (NSNotification *) notification
{
    if (callIntoVisi != Nil) {
    visi_command cmd;
    cmd.cmd = stopRunningCmd;
    callIntoVisi(&cmd);
    freeFunPtr(callIntoVisi);
    callIntoVisi = Nil;
    }
}

- (NSData *)dataOfType:(NSString *)typeName error:(NSError **)outError
{
    NSString *text = [[self.editor textStorage] string];
    const char *utf8 = [text UTF8String];
	NSData *contents = [NSData dataWithBytes:utf8 length:strlen(utf8)];
    return contents;
}

- (BOOL)readFromData:(NSData *)data ofType:(NSString *)typeName error:(NSError **)outError
{
    if ([data length] > 0) {
        NSString *text = [NSString stringWithUTF8String: [data bytes]];
        if (self.editor == nil) {
            self.base = text;
        } else {
            NSString *cur =  [[self.editor textStorage] string];
          [[self.editor textStorage] replaceCharactersInRange: NSMakeRange(0, [cur length]) withString:text];
        }
    }
    return YES;
}

+ (BOOL)autosavesInPlace
{
    return YES;
}

@end
