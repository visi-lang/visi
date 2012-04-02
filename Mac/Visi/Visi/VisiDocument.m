//
//  VisiDocument.m
//  Visi
//
//  Created by David Pollak on 2/9/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import "VisiDocument.h"

#import "include/VisiBridge.h"

@implementation VisiDocument

void runCommand(const void *theId, const visi_command *cmd) {
    id target = (__bridge id) theId;
    // FIXME
}

@synthesize editor;
@synthesize output;
@synthesize errorInfo;
@synthesize base;

- (IBAction)runCode:(id) sender {
    printf("I like eating mice!!!\n");
}

- (void)runCmd:(int) cmd name:(NSString *)name value:(NSString *) value {
    
}

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

/*
 - (void) findCopyOrRemove: (SourceSinkInfo *) ss {
 
 }
 */

- (void) layoutControls {
    
}



- (id)init
{
    self = [super init];
    if (self) {
        self.base = @"";
    }
    return self;
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
    NSString *cur =  [[self.editor textStorage] string];
    [[self.editor textStorage] replaceCharactersInRange: NSMakeRange(0, [cur length]) withString:self.base];
    [editor setAutomaticSpellingCorrectionEnabled:NO];
    // Add any code here that needs to be executed once the windowController has loaded the document's window.
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
