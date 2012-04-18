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

#define HashType long

enum cmds {
 setProgramTextCmd,
 setSourceCmd,
 stopRunningCmd} visiCmds;

typedef struct {
	int cmd;
	// const char *target;
	union {
		const char *text;
		int boolValue;
		double number;
	} cmdInfo;
	HashType targetHash;
	const char *cmdTarget;
	int cmdType;
} visi_command;

enum evts {
 reportErrorEvent,
 removeSourceEvent,
 removeSinkEvent,
 addSourceEvent,
 addSinkEvent,
 setSinkEvent} visiEvents;

enum theTypes {
	doubleVisiType,
	stringVisiType,
	boolVisiType
} visiTypes;

typedef struct 
{
	int cmd;
	union {
		char *errorText;
		char *sourceSinkName;
	} evtInfo;
	HashType targetHash;
	union {
		char *text;
		int boolValue;
		double number;
	} evtValue;
	int eventType;
} visi_event;

/**
 * Send an event into ObjC land and execute the event on the UI thread
 */
void sendEvent(const void *id, visi_event *evt);

// a command callback
typedef void(*cmdFunc)(visi_command *cmd);

cmdFunc startProcess(void *controller);

void freeEvent(visi_event *evt);

void freeFunPtr(cmdFunc cmd);