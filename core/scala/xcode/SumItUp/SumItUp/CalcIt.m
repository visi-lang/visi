//
//  CalcIt.m
//  SumItUp
//
//  Created by David Pollak on 1/29/13.
//  Copyright (c) 2013 David Pollak. All rights reserved.
//

#import "CalcIt.h"
#import "VisiViewController.h"
#import <Foundation/Foundation.h>

@implementation CalcIt

double dTotal;
double dTaxrate;

- (void)viewDidLoad
{
    [super viewDidLoad];
    dTotal = 0;
    dTaxrate = 0;
    
    NSString *prog = @"\n"
    "function Thunk(f, scope) {\n"
    "  this.$_get = getFunc()\n"
    "\n"
    "  function getFunc() {\n"
    "    return function() {\n"
    "      this.$_get = function() {return this.$_old;};\n"
    "      var ret = f(scope);\n"
    "      this.$_get = function() {return ret;}\n"
    "      this.$_old = ret;\n"
    "      return ret;\n"
    "    };\n"
    "  }\n"
    "\n"
    "  this.$_reset = function() {\n"
    "    this.$_get = getFunc()\n"
    "  }\n"
    "\n"
    "  this.$_apply = function(x) {\n"
    "    return this.$_get().$_apply(x);\n"
    "  }\n"
    "\n"
    "  this.$_old = 0\n"
    "}\n"
    "\n"
    "function Value(v) {\n"
    "  this.$_set = function(nv) {v = nv;};\n"
    "  this.$_get = function() {return v;};\n"
    "\n"
    "  this.$_reset = function() {};\n"
    "}\n"
    "\n"
    "function $_cloner(obj) {\n"
    "    if(obj == null || typeof(obj) != 'object')\n"
    "        return obj;\n"
    "\n"
    "    var temp = {};\n"
    "\n"
    "    for(var key in obj)\n"
    "        temp[key] = obj[key];\n"
    "    return temp;\n"
    "}\n"
    "\n"
    "function Func(compute, args, arity, scope, subfunc) {\n"
    "  if (args.length >= arity) {\n"
    "    this.$_apply = function(param) {\n"
    "    var got = this.$_get();\n"
    "    var ret = got.$_apply(param);\n"
    "    return ret;\n"
    "    };\n"
    "  } else {\n"
    "    this.$_apply = function(param) {\n"
    "      var a1 = args;\n"
    "      var a2 = [];\n"
    "      for (i in a1) a2.push(a1[i]);\n"
    "      a2.push(param);\n"
    "      if (subfunc) return compute(scope, a2);\n"
    "      return new Func(compute, a2, arity, scope, false);\n"
    "    }\n"
    "  }\n"
    "\n"
    "  if (arity > args.length) {\n"
    "    this.$_get = function() {return this;};\n"
    "  } else {\n"
    "    this.$_get = function() {\n"
    "      var ret = compute(scope, args);\n"
    "      this.$_get = function() {return ret;};\n"
    "      return ret;\n"
    "    }\n"
    "  }\n"
    "\n"
    "  this.$_reset = function() {};\n"
    "}\n"
    "\n"
    "function $_plusFunc_core(scope, args) {\n"
    "  return args[0].$_get() + args[1].$_get();\n"
    "}\n"
    "\n"
    "function $_minusFunc_core(scope, args) {\n"
    "  return args[0].$_get() - args[1].$_get();\n"
    "}\n"
    "\n"
    "function $_timesFunc_core(scope, args) {\n"
    "  return args[0].$_get() * args[1].$_get();\n"
    "}\n"
    "\n"
    "function $_divFunc_core(scope, args) {\n"
    "  return args[0].$_get() / args[1].$_get();\n"
    "}\n"
    "\n"
    "function $_ifelse_core(scope, args) {\n"
    "  if (args[0].$_get()) return args[1].$_get();\n"
    "  return args[2].$_get();\n"
    "}\n"
    "\n"
    "function $_swapfunc_core(scope, args) {\n"
    "  var ret = new Func(function(zscope, zargs) {\n"
    "   return args[0].$_apply(zargs[1]).$_apply(zargs[0]).$_get();\n"
    "  }, [], 2, scope, false);\n"
    "\n"
    "  return ret;\n"
    "}\n"
    "\n"
    "function $_concat_core(scope, args) {\n"
    "  return args[0].$_get() + args[1].$_get();\n"
    "}\n"
    "\n"
    "function $_equals_core(scope, args) {\n"
    "  return args[0].$_get() == args[1].$_get();\n"
    "}\n"
    "\n"
    "\n"
    "function Const(v) {\n"
    "  this.$_get = function() {return v;}\n"
    "  this.$_reset = function() {}\n"
    "}\n"
    "\n"
    "\n"
    "function $_exec(sources) {  \n"
    "  var sinksToRun = {};\n"
    "  var lets = {};\n"
    "  for (i in sources) {\n"
    "    scope[i].$_set(sources[i]);\n"
    "    sourceInfo[i] = true;\n"
    "    var sa = sourceToSink[i];\n"
    "    for (j in sa) {\n"
    "      sinksToRun[sa[j]] = true;\n"
    "    }\n"
    "\n"
    "    sa = sourceToLets[i]\n"
    "    for (j in sa) {\n"
    "      lets[sa[j]] = true;\n"
    "    }\n"
    "  }\n"
    "\n"
    "  for (i in sourceInfo) {\n"
    "    if (!sourceInfo[i]) return (\"Cannot execute because the source '\"+i+\"' has not been set\");\n"
    "  }\n"
    "\n"
    "  for (i in lets) {\n"
    "    scope[i].$_reset();\n"
    "  }\n"
    "\n"
    "  var ret = {};\n"
    "\n"
    "  for (i in sinksToRun) {\n"
    "    sinks[i].$_reset();\n"
    "    ret[i] = sinks[i].$_get();\n"
    "  }\n"
    "\n"
    "  return JSON.stringify(ret);\n"
    "}\n"
    "    \n"
    "\n"
    "var scope = {};\n"
    "var sourceInfo = {};\n"
    "var sinks = {};\n"
    "var sourceToSink = {};\n"
    "var sourceToLets = {};\n"
    "// enumerate the sources... they must all be satisfied to execute\n"
    "\n"
    "sourceInfo[\"taxRate\"] = false;\n"
    "sourceToSink[\"taxRate\"] = [\"Total\", \"Tax\"];\n"
    "sourceToLets[\"taxRate\"] = [\"total\", \"tax\"];\n"
    "sourceInfo[\"sales\"] = false;\n"
    "sourceToSink[\"sales\"] = [\"Subtotal\", \"Total\", \"Tax\"];\n"
    "sourceToLets[\"sales\"] = [\"total\", \"subtotal\", \"tax\"];\n"
    "scope[\"tax\"] = new Thunk(function(scope) { return scope[\"*\"].$_apply(\n"
    "   scope[\"subtotal\"]).$_apply(\n"
    "   scope[\"taxRate\"]).$_get();}, scope);\n"
    "\n"
    "scope[\"sales\"] = new Value(false);\n"
    "scope[\"/\"] = new Func($_divFunc_core, [], 2, scope, false);\n"
    "\n"
    "scope[\"*\"] = new Func($_timesFunc_core, [], 2, scope, false);\n"
    "\n"
    "scope[\"total\"] = new Thunk(function(scope) { return scope[\"+\"].$_apply(\n"
    "   scope[\"subtotal\"]).$_apply(\n"
    "   scope[\"tax\"]).$_get();}, scope);\n"
    "\n"
    "scope[\"true\"] = new Const(true);\n"
    "\n"
    "scope[\"-\"] = new Func($_minusFunc_core, [], 2, scope, false);\n"
    "\n"
    "sinks[\"Tax\"] = new Thunk(function(scope) { return scope[\"tax\"].$_get();}, scope);\n"
    "\n"
    "scope[\"subtotal\"] = new Thunk(function(scope) { return scope[\"sales\"].$_get();}, scope);\n"
    "\n"
    "sinks[\"Subtotal\"] = new Thunk(function(scope) { return scope[\"subtotal\"].$_get();}, scope);\n"
    "\n"
    "scope[\"&\"] = new Func($_concat_core, [], 2, scope, false);\n"
    "\n"
    "scope[\"==\"] = new Func($_equals_core, [], 2, scope, false);\n"
    "\n"
    "scope[\"false\"] = new Const(false);\n"
    "\n"
    "scope[\"$swapfunc\"] = new Func($_swapfunc_core, [], 1, scope, false);\n"
    "\n"
    "scope[\"$ifelse\"] = new Func($_ifelse_core, [], 3, scope, false);\n"
    "\n"
    "scope[\"taxRate\"] = new Value(false);\n"
    "sinks[\"Total\"] = new Thunk(function(scope) { return scope[\"total\"].$_get();}, scope);\n"
    "\n"
    "scope[\"+\"] = new Func($_plusFunc_core, [], 2, scope, false);\n"
    "function dog(n) {return 'hi '+n;}\n"
    ;


    NSString *ret = [self.webview stringByEvaluatingJavaScriptFromString: prog];
    printf("Web exec load %s\n", [ret UTF8String]);
}


- (IBAction)setTotal:(id)sender {
    dTotal = [[sender text] doubleValue];
    
    NSString *ret = [self.webview stringByEvaluatingJavaScriptFromString:
                     [NSString stringWithFormat:@"$_exec({\"sales\": %f})", dTotal]];
//                     [NSString stringWithFormat:@"execute({sales: %f})", dTotal]];
    printf("Web exec total %s\n", [ret UTF8String]);

    
    [self compute];
}


- (IBAction)setName:(id)sender {
    printf("Dude2\n");
}



- (IBAction)setTaxrate:(id)sender {
    dTaxrate = [[sender text] doubleValue];
    
    NSString *ret = [self.webview stringByEvaluatingJavaScriptFromString:
                     [NSString stringWithFormat:@"$_exec({\"taxRate\": %f})", dTaxrate]];
    printf("Web exec tax %s\n", [ret UTF8String]);
    
    [self compute];
}

- (void) compute {
    [self.subtotal setText: [NSString stringWithFormat:@"$%1.2f", dTotal]];
    [self.tax setText: [NSString stringWithFormat:@"$%1.2f", dTotal * dTaxrate]];
    [self.theTotal setText: [NSString stringWithFormat:@"$%1.2f",  dTotal +(dTotal * dTaxrate)]];
}

@end
