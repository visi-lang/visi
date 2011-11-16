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

#import <Foundation/Foundation.h>


@interface RunCommandOnMainThread : NSObject {
  	void (*whatToDo)(void *);
	id obj;
	int cmd;
	NSString *name;
	NSString *value;
}

- (void)setFunc:(void *)func;
- (void)setCmd: (id) obj cmd:(int) cmd name:(NSString *)name value:(NSString *) value;
- (void)run;
- (void)reallyDoIt:(id)ignore;

@end

extern void releaseMe(void *);
extern void setProgramText(id,const char *);
extern void setSourceString(const char *, int, const char *);

// Shared defines between Haskell-land and ObjC-land

#define BoolSourceType 1
#define BoolSourceStr @"BOOL"
#define StringSourceType 2
#define StringSourceStr @"STR"
#define NumberSourceType 3
#define NumberSourceStr @"NUM"

#define DisplayErrorCmd 1
#define BeginProgramInfoCmd 2
#define EndProgramInfoCmd 3
#define SourceInfoCmd 4
#define SinkInfoCmd 5
#define SetSinkCmd 6