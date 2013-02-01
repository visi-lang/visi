//
//  CalcIt.h
//  SumItUp
//
//  Created by David Pollak on 1/29/13.
//  Copyright (c) 2013 David Pollak. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface CalcIt : UIViewController
- (IBAction)setTotal:(id)sender;
- (IBAction)setName:(id)sender;
- (IBAction)setTaxrate:(id)sender;

@property (weak, nonatomic) IBOutlet id subtotal;
@property (weak, nonatomic) IBOutlet id tax;
@property (weak, nonatomic) IBOutlet id theTotal;
@property (weak, nonatomic) IBOutlet id webview;

@end
