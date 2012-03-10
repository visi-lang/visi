//
//  main.m
//  Visi
//
//  Created by David Pollak on 2/9/12.
//  Copyright (c) 2012 David Pollak. All rights reserved.
//

#import <Cocoa/Cocoa.h>

extern int haskellMain(int argc, char *argv[]);
extern void hs_init(int *argc, char ***argv);

int main(int argc, char *argv[])
{
	hs_init(&argc, &argv);
    return haskellMain(argc, argv);
}

int afterHaskellmain(int argc, char *argv[])
{
    return NSApplicationMain(argc, (const char **)argv);
}
