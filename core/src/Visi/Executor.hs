module Visi.Executor (builtInExp, eval, maybeChan, calcSources, calcSinks) where
    
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
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

import Visi.Expression

 -- | give a list of Expressions and a map of function name to type, return the function name
 -- | the expression and the type
calcSinks :: [Expression] ->  Map.Map T.Text Type -> [(T.Text, Expression, Type)]
calcSinks exprs map = 
    exprs >>= sinker
    where sinker (SinkExp _ _ (FuncName name) _ expr) = [(name, expr, map Map.! name)]
          sinker _ = []

-- | given a list of Expressions, the expression to Type Map, return the name and type
calcSources :: [Expression] -> Map.Map T.Text Type -> [(T.Text, Type)]
calcSources exprs map = 
    exprs >>= sinker
    where sinker (SourceExp _ _ (FuncName name) _) = [(name, map Map.! name)]
          sinker (LetExp _ _ _ _ _ expr) = calcSources [expr] map
          sinker _ = []

{-                        SinkExp LetId FuncName Type !Expression
                          | SourceExp LetId FuncName Type             
-}

updateScope (LetExp _ _ funcName _ _ exp) curScope = Map.insert funcName exp curScope

maybeChan :: Maybe a -> (a -> IO ()) -> IO ()
maybeChan (Just param) f = f param
maybeChan _ _ = return ()

type SourceVars = Map.Map T.Text Value

eval :: SourceVars -> LetScope -> Expression -> Value
eval sourceVars scope exp = 
     let res = eval1 sourceVars scope exp in
     res -- trace ("Eval "++show exp++" res "++show res) res

eval1 :: SourceVars -> LetScope -> Expression -> Value
eval1 sourceVars scope (LetExp _ _ funcName _ _ exp) = eval sourceVars scope exp
eval1 sourceVars scope (InnerLet _ _ letExp actualExp) = eval sourceVars (updateScope letExp scope) actualExp
eval1 sourceVars scope (FuncExp _ funcName p exp) = FuncValue doFuncApply
     where doFuncApply v = eval sourceVars (Map.insert funcName (ValueConst NoSourceLoc v) scope) exp
eval1 sourceVars scope (Apply _ _ _ e1 e2) =
     let (FuncValue exp) = eval sourceVars scope e1 in
     let param = eval sourceVars scope e2 in
     exp param
eval1 sourceVars scope (Var _ funcName) = eval sourceVars scope $ scope Map.! funcName
eval1 sourceVars scope (BuiltIn _ _ _ func) = FuncValue func
eval1 sourceVars scope (ValueConst _ v) = v
eval1 sourceVars scope (SourceExp _ _ (FuncName name) _) = fromMaybe UndefinedValue $ Map.lookup name sourceVars
        
-- eval1 _ _ what = error $ "Yikes... don't know how to deal with: " ++ show what
--                  | Group !(Map.Map FuncName Expression) Type !Expression


builtInExp = [ boolTrue,  boolFalse, builtInConcat, builtInIf, builtInAdd 
             , builtInMult, builtInSub, builtInDiv, builtInAnd, builtInOr
             , builtInReverse, builtInEq, builtInLen, builtInShow
             , builtInGt, builtInGte, builtInLt, builtInLte, builtInFloor
             ]

funcDoubleDouble = tFun (TPrim PrimDouble) (TPrim PrimDouble)
funcDoubleDoubleDouble = tFun (TPrim PrimDouble) funcDoubleDouble

funcBoolBool = tFun (TPrim PrimBool) (TPrim PrimBool)
funcBoolBoolBool = tFun (TPrim PrimBool) funcBoolBool

funcStrStr = tFun (TPrim PrimStr) (TPrim PrimStr)
funcStrStrStr = tFun (TPrim PrimStr) funcStrStr

boolTrue :: Expression
boolTrue = LetExp (builtInLoc "boolTrue") (LetId $ T.pack "boolTrue") (FuncName $ T.pack "true") False (TPrim PrimBool) (ValueConst NoSourceLoc $ BoolValue True)

boolFalse :: Expression
boolFalse = LetExp (builtInLoc "boolFalse") (LetId $ T.pack "boolFalse") (FuncName $ T.pack "false") False (TPrim PrimBool) (ValueConst NoSourceLoc $ BoolValue False)

ifTVar = TVar $ T.pack "IfElse##"
builtInIf :: Expression
builtInIf = BuiltIn (builtInLoc "If/Then/Else") (FuncName $ T.pack "$ifelse") (tFun (TPrim PrimBool) (tFun ifTVar (tFun ifTVar ifTVar)))  ifThing
             where ifThing :: Value -> Value
                   ifThing (BoolValue v) = FuncValue ifElse
                        where ifElse true = FuncValue $ elseThing true
                              elseThing true false = if v then true else false
                   ifThing _ = FuncValue ifElse
                     where ifElse true = FuncValue $ elseThing true
                           elseThing true false = UndefinedValue

builtInAdd :: Expression
builtInAdd = BuiltIn (builtInLoc "+") (FuncName $ T.pack "+") funcDoubleDoubleDouble addThing
             where addThing :: Value -> Value
                   addThing (DoubleValue v) = FuncValue partialAdd
                     where partialAdd :: Value -> Value
                           partialAdd (DoubleValue v') = DoubleValue $ v + v'
                           partialAdd _ = UndefinedValue
                   addThing _ = FuncValue partialAdd
                     where partialAdd :: Value -> Value
                           partialAdd _ = UndefinedValue

eqVar = TVar $ T.pack "Eq##"
builtInEq :: Expression
builtInEq = BuiltIn (builtInLoc "==") (FuncName $ T.pack "==") (tFun eqVar (tFun eqVar $ TPrim PrimBool)) eqThing
             where eqThing :: Value -> Value
                   eqThing v = FuncValue partialEq
                     where partialEq :: Value -> Value
                           partialEq v' = BoolValue $ v == v'


builtInGt :: Expression
builtInGt = BuiltIn (builtInLoc ">") (FuncName $ T.pack ">") (tFun (TPrim PrimDouble) (tFun (TPrim PrimDouble) $ TPrim PrimBool)) gtThing
             where gtThing :: Value -> Value
                   gtThing (DoubleValue v) = FuncValue partialGt
                     where partialGt :: Value -> Value
                           partialGt (DoubleValue v') = BoolValue (v > v')
                           partialGt _ = UndefinedValue
                   gtThing _ = FuncValue partialGt
                     where partialGt :: Value -> Value
                           partialGt _ = UndefinedValue

builtInGte :: Expression
builtInGte = BuiltIn (builtInLoc ">=") (FuncName $ T.pack ">=") (tFun (TPrim PrimDouble) (tFun (TPrim PrimDouble) $ TPrim PrimBool)) gtThing
             where gtThing :: Value -> Value
                   gtThing (DoubleValue v) = FuncValue partialGt
                     where partialGt :: Value -> Value
                           partialGt (DoubleValue v') = BoolValue $ v >= v'
                           partialGt _ = UndefinedValue
                   gtThing _ = FuncValue partialGt
                     where partialGt :: Value -> Value
                           partialGt _ = UndefinedValue

builtInLt :: Expression
builtInLt = BuiltIn (builtInLoc "<") (FuncName $ T.pack "<") (tFun (TPrim PrimDouble) (tFun (TPrim PrimDouble) $ TPrim PrimBool)) gtThing
             where gtThing :: Value -> Value
                   gtThing (DoubleValue v) = FuncValue partialGt
                     where partialGt :: Value -> Value
                           partialGt (DoubleValue v') = BoolValue $ v < v'
                           partialGt _ = UndefinedValue
                   gtThing _ = FuncValue partialGt
                     where partialGt :: Value -> Value
                           partialGt _ = UndefinedValue                           

builtInLte :: Expression
builtInLte = BuiltIn (builtInLoc "<=") (FuncName $ T.pack "<=") (tFun (TPrim PrimDouble) (tFun (TPrim PrimDouble) $ TPrim PrimBool)) gtThing
             where gtThing :: Value -> Value
                   gtThing (DoubleValue v) = FuncValue partialGt
                     where partialGt :: Value -> Value
                           partialGt (DoubleValue v') = BoolValue $ v <= v'
                           partialGt _ = UndefinedValue
                   gtThing _ = FuncValue partialGt
                     where partialGt :: Value -> Value
                           partialGt _ = UndefinedValue

builtInAnd :: Expression
builtInAnd = BuiltIn (builtInLoc "&&") (FuncName $ T.pack "&&") funcBoolBoolBool andThing
             where andThing :: Value -> Value
                   andThing (BoolValue v) = FuncValue partialAnd
                     where partialAnd :: Value -> Value
                           partialAnd (BoolValue v') = BoolValue $ v && v'
                           partialAnd _ = UndefinedValue
                   andThing _ = FuncValue partialAnd
                     where partialAnd :: Value -> Value
                           partialAnd _ = UndefinedValue

builtInOr :: Expression
builtInOr  = BuiltIn (builtInLoc "||") (FuncName $ T.pack "||") funcBoolBoolBool orThing
             where orThing :: Value -> Value
                   orThing (BoolValue v) = FuncValue partialOr
                     where partialOr :: Value -> Value
                           partialOr (BoolValue v') = BoolValue $ v || v'
                           partialOr _ = UndefinedValue
                   orThing _ = FuncValue partialOr
                     where partialOr :: Value -> Value
                           partialOr _ = UndefinedValue

builtInConcat :: Expression
builtInConcat = BuiltIn (builtInLoc "&") (FuncName $ T.pack "&") funcStrStrStr catThing
             where catThing :: Value -> Value
                   catThing (StrValue v) = FuncValue partialCat
                     where partialCat :: Value -> Value
                           partialCat (StrValue v') = StrValue $ v `T.append` v'
                           partialCat _ = UndefinedValue
                   catThing _= FuncValue partialCat
                     where partialCat :: Value -> Value
                           partialCat _ = UndefinedValue

builtInSub :: Expression
builtInSub = BuiltIn (builtInLoc "-") (FuncName $ T.pack "-") funcDoubleDoubleDouble subThing
             where subThing :: Value -> Value
                   subThing (DoubleValue v) = FuncValue partialSub
                     where partialSub :: Value -> Value
                           partialSub (DoubleValue v') = DoubleValue $ v - v'
                           partialSub _ = UndefinedValue
                   subThing _ = FuncValue partialSub
                     where partialSub :: Value -> Value
                           partialSub _ = UndefinedValue

builtInDiv :: Expression
builtInDiv = BuiltIn (builtInLoc "/") (FuncName $ T.pack "/") funcDoubleDoubleDouble divThing
             where divThing :: Value -> Value
                   divThing (DoubleValue v) = FuncValue partialDiv
                     where partialDiv :: Value -> Value
                           partialDiv (DoubleValue v') = if v' == 0.0 then UndefinedValue else DoubleValue $ v / v'
                           partialDiv _ = UndefinedValue
                   divThing _ = FuncValue partialDiv
                     where partialDiv :: Value -> Value
                           partialDiv _ = UndefinedValue

builtInMult :: Expression
builtInMult = BuiltIn (builtInLoc "*") (FuncName $ T.pack "*") funcDoubleDoubleDouble multThing
              where multThing :: Value -> Value
                    multThing (DoubleValue v) = FuncValue partialMult
                     where partialMult :: Value -> Value
                           partialMult (DoubleValue v') = DoubleValue $ v * v'
                           partialMult _ = UndefinedValue
                    multThing _ = FuncValue partialMult
                     where partialMult :: Value -> Value
                           partialMult _ = UndefinedValue


builtInFloor :: Expression
builtInFloor = BuiltIn (builtInLoc "floor") (FuncName $ T.pack "floor") (tFun (TPrim PrimDouble) (TPrim PrimDouble)) floorThing
             where floorThing (DoubleValue d) = DoubleValue $ (fromIntegral (floor d)) 
                   floorThing _ = UndefinedValue

builtInLen :: Expression
builtInLen = BuiltIn (builtInLoc "len") (FuncName $ T.pack "len") (tFun (TPrim PrimStr) (TPrim PrimDouble)) lenThing
             where lenThing (StrValue str) = DoubleValue $ fromIntegral $ T.length str
                   lenThing _ = UndefinedValue

builtInReverse :: Expression
builtInReverse = BuiltIn (builtInLoc "reverse") (FuncName $ T.pack "reverse") (tFun (TPrim PrimStr) (TPrim PrimStr)) revThing
                 where revThing (StrValue str) = StrValue $ T.reverse str
                       revThing _ = UndefinedValue

builtInShow :: Expression
builtInShow = BuiltIn (builtInLoc "show") (FuncName $ T.pack "show") (tFun (TPrim PrimDouble) (TPrim PrimStr)) showThing
             where showThing (DoubleValue int) = StrValue $ T.pack $ show int
                   showThing _ = UndefinedValue