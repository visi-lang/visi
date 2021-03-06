//
//  VisiDocument.m
//  Visi
//
//  Created by David Pollak on 2/9/12.
//  Copyright (c) 2012 David Pollak. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "VisiDocument.h"
#import "RunCommandOnMainThread.h"

void setText(NSTextView *tv, NSString *txt);

void setText(NSTextView *tv, NSString *txt) {
    NSString *cur =  [[tv textStorage] string];
    [[tv textStorage] replaceCharactersInRange: NSMakeRange(0, [cur length]) 
                                    withString:txt];
}

static char fieldNameKey;
static char isNumericField;

void freeEvent(visi_event *evt) {
    switch (evt -> cmd) {
        case reportErrorEvent:
            if (evt -> evtInfo.errorText != Nil) free(evt -> evtInfo.errorText);
            break;
            
        case removeSourceEvent:
        case removeSinkEvent:
            break;
            
        case addSourceEvent:
            if (evt -> evtInfo.sourceSinkName != Nil) free(evt -> evtInfo.sourceSinkName);
            break;
            
        case addSinkEvent:
            if (evt -> evtInfo.sourceSinkName != Nil) free(evt -> evtInfo.sourceSinkName);
            break;

        case setSinkEvent:
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

- (void)textDidChange:(NSNotification *)aNotification {
    id obj = [aNotification object];
    if (obj == editor) {
        [self runCode:obj];
    }
}
-(void) controlTextDidChange:(NSNotification *)aNotification {
    id obj = [aNotification object];
    if (obj == editor) {
        [self runCode:obj];
    } else if (objc_getAssociatedObject(obj,
                                  &isNumericField)) {
        NSString *sv = [obj stringValue];
        NSError *error = NULL;
        NSRegularExpression *regex = [NSRegularExpression         
                                      regularExpressionWithPattern:@"[-+]?[0-9]*\\.?[0-9]*"
                                      options:NSRegularExpressionCaseInsensitive
                                      error:&error];
        NSRange rangeOfFirstMatch = [regex rangeOfFirstMatchInString:sv options:0 range:NSMakeRange(0, [sv length])];
        
        if (!NSEqualRanges(rangeOfFirstMatch, NSMakeRange(NSNotFound, 0))) {
            
            NSString *substringForFirstMatch = [sv substringWithRange:rangeOfFirstMatch];
            [obj setStringValue:substringForFirstMatch];
        } else {
            [obj setStringValue:@"0"];
        }

        [self grabNumber:obj];
    } else {
        [self grabText:obj];
    }
}


- (IBAction)grabText:(id)sender {
    NSInteger tag = [sender tag];
    visi_command cmd;
    cmd.cmd = setSourceCmd;
    cmd.targetHash = tag;
    cmd.cmdType = stringVisiType;
    cmd.cmdInfo.text = [[sender stringValue] UTF8String];
    id fieldName = objc_getAssociatedObject(sender, &fieldNameKey);
    if (fieldName) {
        cmd.cmdTarget = [fieldName UTF8String];
        callIntoVisi(&cmd);
    }
}

- (IBAction)grabNumber:(id)sender {
    NSInteger tag = [sender tag];
    visi_command cmd;
    cmd.cmd = setSourceCmd;
    cmd.targetHash = tag;
    cmd.cmdType = doubleVisiType;
    cmd.cmdInfo.number = [sender doubleValue];
    id fieldName = objc_getAssociatedObject(sender, &fieldNameKey);
    cmd.cmdTarget = [fieldName UTF8String];
    callIntoVisi(&cmd);    
}

- (IBAction)grabBool:(id)sender {
    NSInteger tag = [sender tag];
    visi_command cmd;
    cmd.cmd = setSourceCmd;
    cmd.targetHash = tag;
    cmd.cmdType = boolVisiType;
    if ([sender state] != NSOffState) {cmd.cmdInfo.boolValue = 1;} else {cmd.cmdInfo.boolValue = 0;};
    id fieldName = objc_getAssociatedObject(sender, &fieldNameKey);
    cmd.cmdTarget = [fieldName UTF8String];
    callIntoVisi(&cmd);
}

- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView {
    if (aTableView == sourceControls) {
        return [sourceInfo count];
    }
    return [sinkInfo count];
}

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex {
    return nil;
}

- (NSView *)tableView:(NSTableView *)tableView viewForTableColumn:(NSTableColumn *)tableColumn row:(NSInteger)row {
    NSString *id = tableColumn.identifier;
    if ([id isEqualToString:@"sink_name"]) {
        InfoHolder *info = [sinkInfo objectAtIndex:row];
        NSTextField *tf = [[NSTextField alloc] init];
        [tf setBezeled:NO];
        [tf setBordered:NO];
        [tf setEditable:NO];
        [tf setSelectable:NO];
        [tf setDrawsBackground:NO];
        [tf setStringValue:[info name]];
        return tf;
    }

    if ([id isEqualToString:@"source_name"]) {
        InfoHolder *info = [sourceInfo objectAtIndex:row];
        NSTextField *tf = [[NSTextField alloc] init];
        [tf setBezeled:NO];
        [tf setBordered:NO];
        [tf setEditable:NO];
        [tf setSelectable:NO];
        [tf setDrawsBackground:NO];
        [tf setStringValue:[info name]];
        return tf;
    }    
    
    if ([id isEqualToString:@"sink_value"]) {
        InfoHolder *info = [sinkInfo objectAtIndex:row];
        NSTextField *tf = [[NSTextField alloc] init];
        [tf setBezeled:NO];
        [tf setBordered:NO];
        [tf setEditable:NO];
        [tf setSelectable:NO];
        [tf setDrawsBackground:NO];
        [tf setTag:[info hash]];
        [tf setStringValue:[info value]];
        return tf;
    }

    if ([id isEqualToString:@"source_control"]) {
        InfoHolder *info = [sourceInfo objectAtIndex:row];
        NSString *vt = [info value];
        if ([vt isEqualToString:@"num"]) {
            NSTextField *out = [[NSTextField alloc] init];
            [out setTarget:self];
            [out setAction:@selector(grabNumber:)];
            [out setDelegate:self];
            
            [out setDrawsBackground:NO];
            objc_setAssociatedObject (out,
                                      &fieldNameKey,
                                      [info name],
                                      OBJC_ASSOCIATION_RETAIN);
            [out setTag:[info hash]];
            objc_setAssociatedObject (out,
                                      &isNumericField,
                                      @"true",
                                      OBJC_ASSOCIATION_RETAIN);                                                           

            return out;            
        }
        
        if ([vt isEqualToString:@"str"]) {
            NSTextField *out = [[NSTextField alloc] init];
            [out setTarget:self];
            [out setAction:@selector(grabText:)];
            [out setDelegate:self];

            [out setDrawsBackground:NO];
            objc_setAssociatedObject (out,
                                      &fieldNameKey,
                                      [info name],
                                      OBJC_ASSOCIATION_RETAIN);
            [out setTag:[info hash]];

            return out;
        }
        
        if ([vt isEqualToString:@"bool"]) {
            NSButton * out = [[NSButton alloc] init];
            [out setButtonType:NSSwitchButton];
            [out setTitle:@""];
            [out setTarget:self];
            [out setAction:@selector(grabBool:)];            
            objc_setAssociatedObject (out,
                                      &fieldNameKey,
                                      [info name],
                                      OBJC_ASSOCIATION_RETAIN);
            [out setTag:[info hash]];
            return out;
        }
    }
    
    
    return nil;
}

- (id)findSink:(NSInteger) hash {
    id first = [sourceControls viewWithTag:hash];
    if (first) return first;
    return [sinkControls viewWithTag:hash];
}

- (void) layoutControls {
  [self layoutControls: sourceControls];
  [self layoutControls: sinkControls];
}

- (void) layoutControls:(id)view {
    NSArray *subs = [view subviews];
    for (int x = 0; x < [subs count]; x++) {
        NSRect rect = CGRectMake(0, 30 * x, 300, 30);
        NSView *tv = [subs objectAtIndex:x];
        [tv setFrame:rect];    
        [tv setNeedsDisplay:YES];
    }
    [view setNeedsLayout: YES];
    [view setNeedsDisplay:YES];
}

- (id)init
{
    self = [super init];
    callIntoVisi = Nil;
    if (self) {
        sourceInfo = [[NSArray alloc] init];
        sinkInfo = [[NSArray alloc] init];
        
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
        {
            int pos = [InfoHolder find:sourceInfo withHash:event -> targetHash];
            if (pos >= 0) {
                NSMutableArray *mut = [[NSMutableArray alloc] init];
                [mut addObjectsFromArray: sourceInfo];
                [mut removeObjectAtIndex: pos];
                sourceInfo = [NSArray arrayWithArray:mut];
                [sourceControls removeRowsAtIndexes:[NSIndexSet indexSetWithIndex: pos]
                                      withAnimation:NSTableViewAnimationSlideUp];
            }
        }
            break;
            
        case removeSinkEvent:
        {
            int pos = [InfoHolder find:sinkInfo withHash:event -> targetHash];
            if (pos >= 0) {
                NSMutableArray *mut = [[NSMutableArray alloc] init];
                [mut addObjectsFromArray: sinkInfo];
                [mut removeObjectAtIndex: pos];
                sinkInfo = [NSArray arrayWithArray:mut];
                [sinkControls removeRowsAtIndexes:[NSIndexSet indexSetWithIndex: pos]
                                      withAnimation:NSTableViewAnimationSlideUp];
            }
        }
            break;
            
        case addSourceEvent:
        {
            InfoHolder *ih = [[InfoHolder alloc]
                              initWithName:
                                [NSString stringWithUTF8String:
                                 event -> evtInfo.sourceSinkName]
                                                        value:@""
                              hash:event -> targetHash];
            
            switch (event -> eventType) {
                case doubleVisiType:
                    [ih updateValue:@"num"];
                    break;
                    
                case stringVisiType:
                    [ih updateValue:@"str"];
                    break;
                    
                case boolVisiType:
                    [ih updateValue:@"bool"];
                    break;
            }
            
            NSUInteger sz = [sourceInfo count];
            sourceInfo = [sourceInfo arrayByAddingObject:ih];
            
            [sourceControls insertRowsAtIndexes: [NSIndexSet indexSetWithIndex:sz]
                                withAnimation:NSTableViewAnimationSlideDown];
        }
            break;
            
        case addSinkEvent: 
        {
            InfoHolder *ih = [[InfoHolder alloc] 
                              initWithName:
                                [NSString stringWithUTF8String:
                                 event -> evtInfo.sourceSinkName]
                              value: @""
                              hash: event -> targetHash];
            
            NSUInteger sz = [sinkInfo count];
            sinkInfo = [sinkInfo arrayByAddingObject:ih];

            [sinkControls 
             insertRowsAtIndexes: 
                [NSIndexSet indexSetWithIndex:sz]
                                        withAnimation:NSTableViewAnimationSlideDown];
        }
            
            break;
            
        case setSinkEvent: {
            int i = [InfoHolder find:sinkInfo withHash:event -> targetHash];
            if (i >= 0) {
                InfoHolder *ih = [sinkInfo objectAtIndex:i];
                NSString *str = @"";
                switch (event -> eventType) {
                    case doubleVisiType: {
                        NSTextField *ttf = [[NSTextField alloc] init];
                        [ttf setDoubleValue: event -> evtValue.number];
                        str = [ttf stringValue];
                    }
                        break;
                        
                    case stringVisiType:
                        str = [NSString stringWithUTF8String: event -> evtValue.text];
                        break;
                        
                    case boolVisiType:
                        if (event -> evtValue.boolValue) {
                            str = @"true";
                        } else {
                            str = @"false";
                        }
                        break;
                        
                    default:
                        break;
                }
                [ih updateValue:str];
                
                NSTextField *tf = [sinkControls viewWithTag:event -> targetHash];
                [tf setStringValue:str];
                [tf setNeedsDisplay:YES];
            }
        }
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
