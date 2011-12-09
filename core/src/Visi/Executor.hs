module Visi.Executor (builtInExp, eval) where
    
{- ***** BEGIN LICENSE BLOCK *****
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
 * ***** END LICENSE BLOCK ***** -}
 
 
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Visi.Expression
import Visi.Parse
import Visi.Util
 
calcSinks :: [Expression] -> [(String, Expression, String)]
calcSinks exprs = 
    exprs >>= sinker
    where sinker (SinkExp _ (FuncName name) (TVar tv) expr) = [(name, expr, tv)]
          sinker _ = []

calcSources :: [Expression] -> [(String, String)]
calcSources exprs = 
    exprs >>= sinker
    where sinker (SourceExp _ (FuncName name) (TVar tv)) = [(name, tv)]
          sinker (LetExp _ _ _ expr) = calcSources [expr]
          sinker _ = []

{-                        SinkExp LetId FuncName Type !Expression
                          | SourceExp LetId FuncName Type             
-}

maybeChan :: Maybe a -> (a -> IO ()) -> IO ()
maybeChan (Just param) f = f param
maybeChan _ _ = return ()

type SourceVars = Map.Map String Value

eval :: SourceVars -> LetScope -> Expression -> Value
eval sourceVars scope exp = 
     let res = eval1 sourceVars scope exp in
     res -- trace ("Eval "++show exp++" res "++show res) res

eval1 :: SourceVars -> LetScope -> Expression -> Value
eval1 sourceVars scope (LetExp _ funcName _ exp) = eval sourceVars scope exp -- error $ "Crap got a let!! " ++ show funcName -- eval scope exp
eval1 sourceVars scope (FuncExp funcName p exp) = FuncValue doFuncApply
     where doFuncApply v = eval sourceVars (Map.insert funcName (ValueConst v) scope) exp
eval1 sourceVars scope (Apply _ _ e1 e2) =
     let (FuncValue exp) = eval sourceVars scope e1 in
     let param = eval sourceVars scope e2 in
     exp(param)
eval1 sourceVars scope (Var funcName) = eval sourceVars scope $ scope Map.! funcName
eval1 sourceVars scope (BuiltIn _ _ func) = FuncValue func
eval1 sourceVars scope (ValueConst v) = v
eval1 sourceVars scope (SourceExp _ (FuncName name) _) = 
    case Map.lookup name sourceVars of
        Just(v) -> v
        _ -> UndefinedValue
        
-- eval1 _ _ what = error $ "Yikes... don't know how to deal with: " ++ show what
--                  | Group !(Map.Map FuncName Expression) Type !Expression


builtInExp = boolTrue : boolFalse : builtInConcat : builtInIf : builtInAdd : 
             builtInMult : builtInSub : builtInDiv : builtInAnd : builtInOr :
             builtInReverse : builtInEq :
             builtInLen : builtInShow : []

funcDoubleDouble = TFun (TPrim PrimDouble) (TPrim PrimDouble)
funcDoubleDoubleDouble = TFun (TPrim PrimDouble) funcDoubleDouble

funcBoolBool = TFun (TPrim PrimBool) (TPrim PrimBool)
funcBoolBoolBool = TFun (TPrim PrimBool) funcBoolBool

funcStrStr = TFun (TPrim PrimStr) (TPrim PrimStr)
funcStrStrStr = TFun (TPrim PrimStr) funcStrStr

boolTrue :: Expression
boolTrue = LetExp (LetId "boolTrue") (FuncName "true") (TPrim PrimBool) (ValueConst $ BoolValue True)

boolFalse :: Expression
boolFalse = LetExp (LetId "boolFalse") (FuncName "false") (TPrim PrimBool) (ValueConst $ BoolValue False)

ifTVar = TVar "IfElse##"
builtInIf :: Expression
builtInIf = BuiltIn (FuncName "$ifelse") (TFun (TPrim PrimBool) (TFun ifTVar (TFun ifTVar ifTVar)))  ifThing
             where ifThing :: Value -> Value
                   ifThing (BoolValue v) = FuncValue ifElse
                        where ifElse true = FuncValue $ elseThing true
                              elseThing true false = if v then true else false
                   ifThing _ = FuncValue ifElse
                     where ifElse true = FuncValue $ elseThing true
                           elseThing true false = UndefinedValue

builtInAdd :: Expression
builtInAdd = BuiltIn (FuncName "+") funcDoubleDoubleDouble addThing
             where addThing :: Value -> Value
                   addThing (DoubleValue v) = FuncValue partialAdd
                     where partialAdd :: Value -> Value
                           partialAdd (DoubleValue v') = DoubleValue $ v + v'
                           partialAdd _ = UndefinedValue
                   addThing _ = FuncValue partialAdd
                     where partialAdd :: Value -> Value
                           partialAdd _ = UndefinedValue

eqVar = TVar "Eq##"
builtInEq :: Expression
builtInEq = BuiltIn (FuncName "==") (TFun eqVar (TFun eqVar $ TPrim PrimBool)) eqThing
             where eqThing :: Value -> Value
                   eqThing v = FuncValue partialEq
                     where partialEq :: Value -> Value
                           partialEq v' = BoolValue $ v == v'

builtInAnd :: Expression
builtInAnd = BuiltIn (FuncName "&&") funcBoolBoolBool andThing
             where andThing :: Value -> Value
                   andThing (BoolValue v) = FuncValue partialAnd
                     where partialAnd :: Value -> Value
                           partialAnd (BoolValue v') = BoolValue $ v && v'
                           partialAnd _ = UndefinedValue
                   andThing _ = FuncValue partialAnd
                     where partialAnd :: Value -> Value
                           partialAnd _ = UndefinedValue

builtInOr :: Expression
builtInOr  = BuiltIn (FuncName "||") funcBoolBoolBool orThing
             where orThing :: Value -> Value
                   orThing (BoolValue v) = FuncValue partialOr
                     where partialOr :: Value -> Value
                           partialOr (BoolValue v') = BoolValue $ v || v'
                           partialOr _ = UndefinedValue
                   orThing _ = FuncValue partialOr
                     where partialOr :: Value -> Value
                           partialOr _ = UndefinedValue

builtInConcat :: Expression
builtInConcat = BuiltIn (FuncName "&") funcStrStrStr catThing
             where catThing :: Value -> Value
                   catThing (StrValue v) = FuncValue partialCat
                     where partialCat :: Value -> Value
                           partialCat (StrValue v') = StrValue $ v ++ v'
                           partialCat _ = UndefinedValue
                   catThing _= FuncValue partialCat
                     where partialCat :: Value -> Value
                           partialCat _ = UndefinedValue

builtInSub :: Expression
builtInSub = BuiltIn (FuncName "-") funcDoubleDoubleDouble subThing
             where subThing :: Value -> Value
                   subThing (DoubleValue v) = FuncValue partialSub
                     where partialSub :: Value -> Value
                           partialSub (DoubleValue v') = DoubleValue $ v - v'
                           partialSub _ = UndefinedValue
                   subThing _ = FuncValue partialSub
                     where partialSub :: Value -> Value
                           partialSub _ = UndefinedValue

builtInDiv :: Expression
builtInDiv = BuiltIn (FuncName "/") funcDoubleDoubleDouble divThing
             where divThing :: Value -> Value
                   divThing (DoubleValue v) = FuncValue partialDiv
                     where partialDiv :: Value -> Value
                           partialDiv (DoubleValue v') = if (v' == 0.0) then UndefinedValue else DoubleValue $ v / v'
                           partialDiv _ = UndefinedValue
                   divThing _ = FuncValue partialDiv
                     where partialDiv :: Value -> Value
                           partialDiv _ = UndefinedValue

builtInMult :: Expression
builtInMult = BuiltIn (FuncName "*") funcDoubleDoubleDouble multThing
              where multThing :: Value -> Value
                    multThing (DoubleValue v) = FuncValue partialMult
                     where partialMult :: Value -> Value
                           partialMult (DoubleValue v') = DoubleValue $ v * v'
                           partialMult _ = UndefinedValue
                    multThing _ = FuncValue partialMult
                     where partialMult :: Value -> Value
                           partialMult _ = UndefinedValue

builtInLen :: Expression
builtInLen = BuiltIn (FuncName "len") (TFun (TPrim PrimStr) (TPrim PrimDouble)) lenThing
             where lenThing (StrValue str) = DoubleValue $ fromIntegral $ length str
                   lenThing _ = UndefinedValue

builtInReverse :: Expression
builtInReverse = BuiltIn (FuncName "reverse") (TFun (TPrim PrimStr) (TPrim PrimStr)) revThing
                 where revThing (StrValue str) = StrValue $ reverse str
                       revThing _ = UndefinedValue

builtInShow :: Expression
builtInShow = BuiltIn (FuncName "show") (TFun (TPrim PrimDouble) (TPrim PrimStr)) showThing
             where showThing (DoubleValue int) = StrValue $ show int
                   showThing _ = UndefinedValue