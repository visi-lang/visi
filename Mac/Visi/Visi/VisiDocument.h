//
//  VisiDocument.h
//  Visi
//
//  Created by David Pollak on 2/9/12.
//  Copyright (c) 2012 David Pollak. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <objc/runtime.h>

#import "include/VisiBridge.h"

@interface VisiDocument : NSDocument{
	NSTextView *editor;
	NSScrollView *output;
	NSTextView *errorInfo;
	NSArray *currentControls;
	NSMutableArray *newControls;
    NSString *base;
    cmdFunc callIntoVisi;
}

@property (retain, nonatomic) IBOutlet NSTextView *editor;
@property (retain, nonatomic) IBOutlet NSTextView *errorInfo;
@property (retain, nonatomic) IBOutlet NSString *base;
@property (retain, nonatomic) IBOutlet NSView *sourceControls;
@property (retain, nonatomic) IBOutlet NSView *sinkControls;


- (IBAction)runCode:(id) sender;
- (IBAction)grabBool:(id)sender;
- (IBAction)grabText:(id)sender;
- (IBAction)grabNumber:(id)sender;
- (void) windowWillClose: (NSNotification *) notification;
- (void)runEvent:(visi_event *)event;
-(void) controlTextDidChange:(NSNotification *)aNotification;

// - (void) findCopyOrRemove: (SourceSinkInfo *) ss;
- (void) layoutControls;					
- (void) layoutControls:(id)view;
@end
