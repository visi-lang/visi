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
* Portions created by the Initial Developer are Copyright (C) 2011-2012
* the Initial Developer. All Rights Reserved.
*
* Contributor(s):
*
* ***** END LICENSE BLOCK ***** */

#import "RunCommandOnMainThread.h"
#import "VisiDocument.h"

@implementation RunCommandOnMainThread

- (id)init:(id)t event:(visi_event *)e {
    event = e;
    target = t;
    return self;
}

- (void)dealloc {
    if (event != NULL) freeEvent(event);
}


- (void)run {
    [self performSelectorOnMainThread:@selector(reallyDoIt:) withObject:self waitUntilDone:NO];
}

- (void)reallyDoIt:(id)ignore {
    [target runEvent:event];
    freeEvent(event);
    event = Nil;
}
@end
