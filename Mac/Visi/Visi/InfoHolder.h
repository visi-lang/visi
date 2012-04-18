//
//  InfoHolder.h
//  Visi
//
//  Created by David Pollak on 4/18/12.
//  Copyright (c) 2012 lift Web Framework. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface InfoHolder : NSObject {
    NSString *name;
    NSString *value;
    long targetHash;
}

- (InfoHolder *)initWithName:(NSString *)n value:(NSString *)v hash:(long)h;
- (void)updateValue:(NSString *)v;
- (long)hash;
- (NSString *)name;
- (NSString *)value;

+ (int)find:(NSArray *)a withHash:(long)h;
+ (id)findItem:(NSArray *)a withHash:(long)h;

@end
