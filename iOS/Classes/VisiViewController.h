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

#import <UIKit/UIKit.h>
#import "SourceSinkInfo.h"

@interface VisiViewController : UIViewController <UITextFieldDelegate>{
	UITextView *editor;
	UIScrollView *output;
	UITextView *errorInfo;
	NSArray *currentControls;
	NSMutableArray *newControls;
	NSString *curModelName;
}
@property (retain, nonatomic) IBOutlet UITextView *editor;
@property (retain, nonatomic) IBOutlet UIScrollView *output;
@property (retain, nonatomic) IBOutlet UITextView *errorInfo;
@property (retain, nonatomic) IBOutlet UITableView *models;
@property (retain, nonatomic) NSArray *currentControls;
@property (retain, nonatomic) NSMutableArray *newControls;

- (IBAction)runCode:(id) sender;
- (IBAction)newModel:(id) sender;
- (void)runCmd:(int) cmd name:(NSString *)name value:(NSString *) value;
- (IBAction)grabBool:(id)sender;
- (IBAction)grabText:(id)sender;

- (UITextField *)findSink:(NSString *)name;

- (void) findCopyOrRemove: (SourceSinkInfo *) ss;
- (void) layoutControls;							

@end

