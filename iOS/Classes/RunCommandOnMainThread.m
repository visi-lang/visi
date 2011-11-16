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

#import "RunCommandOnMainThread.h"
#import "VisiViewController.h"

void sendInfoBack(id theObj,int cmd,char *name, char *value) {
    RunCommandOnMainThread *runner = [[RunCommandOnMainThread alloc] init];
    NSString *nameStr = [[[NSString alloc] initWithUTF8String:name] retain];
    NSString *valueStr = [[[NSString alloc] initWithUTF8String:value] retain];
    [runner setCmd: theObj cmd: cmd name: nameStr value: valueStr];
    [runner retain];
    [runner run];
}

@implementation RunCommandOnMainThread

- (id)init {
    [super init];
    whatToDo = NULL;
    cmd = -1;
    name = nil;
    value = nil;
    return self;
}

- (void)dealloc {
    if (whatToDo != NULL) releaseMe(whatToDo);
    //[obj release];
    [name release];
    [value release];
    [super dealloc];
}

- (void)setCmd: (id) _obj cmd:(int)theCmd name:(NSString *)_name value:(NSString *) _value {
    obj = _obj;
    //[obj retain];
    cmd = theCmd;
    name = _name;
    [name retain];
    value = _value;
    [value retain];
}

- (void)run {
    [self performSelectorOnMainThread:@selector(reallyDoIt:) withObject:self waitUntilDone:TRUE];
}

- (void)reallyDoIt:(id)ignore {

    switch (cmd) {
        case -1:
        whatToDo(NULL);
        break;
        
        default:
        [obj runCmd: cmd name: name value: value];
        break;
    }

    [self release];
}

- (void)setFunc:(void *)func {
    whatToDo = func;
}

@end
