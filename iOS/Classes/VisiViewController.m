/* ***** BEGIN LICENSE BLOCK *****
* Version: MPL 1.1
*
* The contents of this file are subject to the Mozilla Public License Version
* 1.1 (the "License"); you may not use this file except in compliance with
* the License. You may obtain a copy of the License at
* http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* The Original Code is Visi.io.
*
* The Initial Developer of the Original Code is
* David Pollak.
* Portions created by the Initial Developer are Copyright (C) 2011
* the Initial Developer. All Rights Reserved.
*
* Contributor(s):
*
* ***** END LICENSE BLOCK ***** */

#import "VisiViewController.h"
#import "RunCommandOnMainThread.h"
#import "NamedSwitch.h"
#import "NamedText.h"
#import "SourceSinkInfo.h"

@implementation VisiViewController

@synthesize editor;
@synthesize output;
@synthesize models;
@synthesize errorInfo;
@synthesize currentControls;
@synthesize newControls;

- (void)viewDidLoad {
    [super viewDidLoad];
    currentControls = [[NSArray array] retain];

    UIFont *font = [UIFont fontWithName:@"Courier" size:12.0];
    errorInfo.font = font;
    errorInfo.text = @"";
    editor.font = [UIFont fontWithName:@"Courier" size:16.0];
}



// Override to allow orientations other than the default portrait orientation.
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    return YES;
}

- (void)didReceiveMemoryWarning {
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];

    // Release any cached data, images, etc that aren't in use.
}

- (void)viewDidUnload {
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}


- (void)dealloc {
    [super dealloc];
}

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

- (NSString *)calcName:(NSString *)text {
	NSString *theName = nil;
	NSArray *lines = [text componentsSeparatedByString:@"\n"];
	NSUInteger theLine = [lines indexOfObjectPassingTest:
						  ^(id obj, NSUInteger idx, BOOL *stop) {
							  return (NSEqualRanges([obj rangeOfString:@"//"], NSMakeRange (0, 2)));
						}];
	printf("The line is: %i\n", theLine);
						
	return @"";
}

- (void)saveIfNeeded:(NSString *)text {
	if (text != nil && [text length] > 0) {
		NSString *newName = [self calcName: text];
	}
}

- (IBAction)runCode:(id) sender {
    errorInfo.text = @"";
    NSString *editorValue = [editor text];

	[self saveIfNeeded:editorValue];
	
    setProgramText(self, [editorValue cStringUsingEncoding:[NSString defaultCStringEncoding]]);
}


- (IBAction)newModel:(id) sender {
    errorInfo.text = @"";
    NSString *editorValue = [editor text];

	[self saveIfNeeded:editorValue];
	
	[curModelName release];
    curModelName = nil;
	[editor setText:@""];
}

- (BOOL)textField:(UITextField *)textField shouldChangeCharactersInRange:(NSRange)range replacementString:(NSString *)string
{

    NSNumberFormatter *numberFormatter = [[NSNumberFormatter alloc] init];
    [numberFormatter setNumberStyle:NSNumberFormatterDecimalStyle];

    NSNumber* candidateNumber;

    NSString* candidateString = [textField.text stringByReplacingCharactersInRange:range withString:string];

    range = NSMakeRange(0, [candidateString length]);

    [numberFormatter getObjectValue:&candidateNumber forString:candidateString range:&range error:nil];

    if (([candidateString length] > 0) && (candidateNumber == nil || range.length < [candidateString length])) {
        return NO;
    } else {
        return YES;
    }
}

- (void) findCopyOrRemove: (SourceSinkInfo *)ss {
    int x = 0;
    for (x = 0; x < [self.currentControls count]; x++) {
        SourceSinkInfo *other = [self.currentControls objectAtIndex:x];
        // we've found a source or sink match... copy the controls
        if (ss.source == other.source && [ss.name isEqualToString: other.name] &&
        [ss.theType isEqualToString: other.theType]) {
            ss.control = other.control;
            other.control = nil;
            ss.container = other.container;
            other.container = nil;
        }
    }
}

- (UITextField *) findSink:(NSString *) sinkName {
    int x;
    for (x = 0; x < [self.currentControls count]; x++) {
        SourceSinkInfo *ss = [self.currentControls objectAtIndex:x];
        if (!ss.source && [sinkName isEqualToString: ss.name]) return (UITextField *) ss.control;
    }
    return (UITextField *) nil;
}

/*
* Given the list of controls, lay them out nicely
*/
- (void) layoutControls {
    int sourceCnt = 0;
    int sinkCnt = 0;

    for (int x = 0; x < [currentControls count]; x++) {
        SourceSinkInfo *ss = (SourceSinkInfo *) [currentControls objectAtIndex:x];
        if (ss.source) {
            if (ss.container == nil) {
                UIView *fr = [[[UIView alloc] initWithFrame:CGRectMake(0, 30 * sourceCnt, 400, 30)] retain];
                ss.container = fr;
                [self.output addSubview:fr];

                UILabel *label = [[[UILabel alloc] initWithFrame:CGRectMake(0, 0, 100, 30)] retain];
                [ss.name retain];
                label.text = ss.name;
                [fr addSubview: label];

                UIView *control = nil;
                CGRect rect = CGRectMake(100, 0, 290, 28);

                if ([ss.theType isEqualToString: BoolSourceStr]) {
                    NamedSwitch *ns = [[[NamedSwitch alloc] initWithFrame: rect] retain];
                    ns.name = ss.name;
                    [ns addTarget: nil action: @selector(grabBool:) forControlEvents: UIControlEventValueChanged ];
                    control = ns;
                } else if ([ss.theType isEqualToString:StringSourceStr]) {
                    NamedText *nt = [[[NamedText alloc] initWithFrame: rect] retain];
                    nt.name = ss.name;
                    nt.text = @"";
                    nt.borderStyle = UITextBorderStyleLine;
                    // [nt setDelegate:self];
                    [nt addTarget: nil action: @selector(grabText:) forControlEvents:  UIControlEventEditingChanged];
                    control = nt;
                } else {
                    NamedText *nt = [[[NamedText alloc] initWithFrame: rect] retain];
                    nt.name = ss.name;
                    nt.text = @"0";
                    nt.borderStyle = UITextBorderStyleLine;
                    [nt setDelegate:self];
                    [nt addTarget: nil action: @selector(grabText:) forControlEvents:  UIControlEventEditingChanged];
                    control = nt;
                }

                [fr addSubview:control];

                ss.control = control;
            }
            ss.container.frame = CGRectMake(0, 30 * sourceCnt, 400, 30);
            sourceCnt++;
        } else {
            if (ss.container == nil) {
                UIView *fr = [[[UIView alloc] initWithFrame:CGRectMake(400, 30 * sourceCnt, 300, 30)] retain];
                ss.container = fr;
                [self.output addSubview:fr];
                UILabel *label = [[[UILabel alloc] initWithFrame:CGRectMake(0, 0, 100, 30)] retain];
                label.text = ss.name;
                [fr addSubview: label];
                UILabel *out = [[[UILabel alloc] initWithFrame:CGRectMake(100, 0, 200, 30)] retain];
                ss.control = out;
                out.text = @"";
                [fr addSubview:out];
            }
            ss.container.frame = CGRectMake(400, 30 * sinkCnt, 300, 30);
            sinkCnt++;
        }
    }

    int max = sourceCnt > sinkCnt ? sourceCnt : sinkCnt;

    output.contentSize = CGSizeMake(400, 30 * (1 + max));
    [output setNeedsDisplay];

    // update the model with the current values
    for (int x = 0; x < [currentControls count]; x++) {
        SourceSinkInfo *ss = [currentControls objectAtIndex:x];
        if (ss.source) {
            UIView *uc = ss.control;
            if ([ss.theType isEqualToString: BoolSourceStr]) {
                [self grabBool:uc];
            } else {
                [self grabText:uc];
            }
        }
    }
}

- (id)doLayoutThing {
    // remove all elements not in new list
    for (int x = 0; x < [self.newControls count]; x++) {
        [self findCopyOrRemove: (SourceSinkInfo *) [self.newControls objectAtIndex:x]];
    }

    // remove all the current elements not in the new list from the UI
    for (int x = 0; x < [self.currentControls count]; x++) {
        SourceSinkInfo *ss = (SourceSinkInfo *) [self.currentControls objectAtIndex:x];
        if (ss.container != nil) {
            [ss.container removeFromSuperview];
            ss.container = nil;
            ss.control = nil;
        }
    }

    [currentControls release];
    currentControls = nil;
    currentControls = newControls;
    newControls = nil;
    [self layoutControls];
    return self;
}	

- (void)runCmd:(int) cmd name:(NSString *)name value:(NSString *) value {
    SourceSinkInfo *ss;
    UITextField *tf;

    switch (cmd) {
        case DisplayErrorCmd:
        errorInfo.text = value;
        break;

        case BeginProgramInfoCmd:
        self.newControls = [NSMutableArray array];
        break;

        case EndProgramInfoCmd:
        [self doLayoutThing];
        break;

        case SourceInfoCmd:
        ss = [[[SourceSinkInfo alloc] init] retain];
        ss.name = name;
        ss.source = YES;
        ss.theType = value;

        [self.newControls addObject:ss];
        break;

        case SinkInfoCmd:
        ss = [[[SourceSinkInfo alloc] init] retain];
        ss.name = name;
        ss.source = NO;
        ss.theType = value;

        [self.newControls addObject:ss];
        break;

        case SetSinkCmd:
        tf = [self findSink:name];
        tf.text = value;
        [tf setNeedsDisplay];
        break;
    }
}

@end
