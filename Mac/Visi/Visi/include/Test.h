/***** BEGIN LICENSE BLOCK *****
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
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK *****/


enum cmds {
 setProgramTextCmd,
 setStringSourceCmd,
 setNumberSourceCmd,
 setBoolSourceCmd} visiCmds;

typedef struct {
	enum cmds cmd;
	const char *target;
	union info {
		const char *text;
		int boolValue;
		double number;
	} theInfo;
} visi_command;



typedef struct 
{
	int cmd;
	const char *text;
	int boolValue;
	double number;
} visi_event;

void runCommand(const void *id, const visi_command *cmd);

