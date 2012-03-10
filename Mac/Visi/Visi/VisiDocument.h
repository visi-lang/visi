//
//  VisiDocument.h
//  Visi
//
//  Created by David Pollak on 2/9/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface VisiDocument : NSDocument{
	NSTextView *editor;
	NSScrollView *output;
	NSTextView *errorInfo;
	NSArray *currentControls;
	NSMutableArray *newControls;
    NSString *base;
}

@property (retain, nonatomic) IBOutlet NSTextView *editor;
@property (retain, nonatomic) IBOutlet NSScrollView *output;
@property (retain, nonatomic) IBOutlet NSTextView *errorInfo;
@property (retain, nonatomic) IBOutlet NSString *base;

- (IBAction)runCode:(id) sender;
- (void)runCmd:(int) cmd name:(NSString *)name value:(NSString *) value;
- (IBAction)grabBool:(id)sender;
- (IBAction)grabText:(id)sender;

- (NSTextField *)findSink:(NSString *)name;

// - (void) findCopyOrRemove: (SourceSinkInfo *) ss;
- (void) layoutControls;					

@end
