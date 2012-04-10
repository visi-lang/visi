//
//  VisiDocument.h
//  Visi
//
//  Created by David Pollak on 2/9/12.
//  Copyright (c) 2012 David Pollak. All rights reserved.
//

#import <Cocoa/Cocoa.h>

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
@property (retain, nonatomic) IBOutlet NSScrollView *sourceControls;
@property (retain, nonatomic) IBOutlet NSScrollView *sinkControls;


- (IBAction)runCode:(id) sender;
- (IBAction)grabBool:(id)sender;
- (IBAction)grabText:(id)sender;
- (void) windowWillClose: (NSNotification *) notification;
- (NSTextField *)findSink:(NSString *)name;
- (void)runEvent:(visi_event *)event;

// - (void) findCopyOrRemove: (SourceSinkInfo *) ss;
- (void) layoutControls;					

@end
