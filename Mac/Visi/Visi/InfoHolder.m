//
//  InfoHolder.m
//  Visi
//
//  Created by David Pollak on 4/18/12.
//  Copyright (c) 2012 lift Web Framework. All rights reserved.
//

#import "InfoHolder.h"

@implementation InfoHolder

- (InfoHolder *)initWithName:(NSString *)n value:(NSString *)v hash:(long)h
{
    self = [super init];
    name = n;
    value = v;
    targetHash = h;
    return self;
}

- (void)updateValue:(NSString *)v
{
    value = v;
}
- (long)hash {
    return targetHash;
}

- (NSString *)name {
    return name;
}

- (NSString *)value {
    return value;
}

+ (int)find:(NSArray *)a withHash:(long)h {
    for (int x = 0; x < [a count]; x++) {
        id o = [a objectAtIndex:x];
        if ([o hash] == h) return x;
    }
    return -1;
}

+ (id)findItem:(NSArray *)a withHash:(long)h {
    int i = [InfoHolder find:a withHash:h];
    if (i < 0) return nil;
    return [a objectAtIndex: i];
}

@end
