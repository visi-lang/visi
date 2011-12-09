module Visi.Expression ( 
                            Expression(LetExp, FuncExp, Apply,
                                       SinkExp, SourceExp,
                                       Var, BuiltIn, ValueConst,
                                       Group),
                           AllTypeVars(AllTypeVars),
                           VarScope, LetScope, TVarInfo(TVarInfo),
                           Type(TVar, TPrim, TOper),
                           FuncName(FuncName), 
                           LetId(LetId),
                           Prim(PrimDouble, PrimBool, PrimStr),
                           valuePrim,
                           tFun, funcOperName,
                           Value(DoubleValue, StrValue, BoolValue, FuncValue, UndefinedValue)) where

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
import Visi.Util
import Data.Char

newtype LetId = LetId String deriving (Eq, Ord, Show)

data Expression = LetExp LetId FuncName Type !Expression 
                  | SinkExp LetId FuncName Type !Expression
                  | SourceExp LetId FuncName Type
                  | FuncExp FuncName Type !Expression
                  | Apply LetId Type Expression !Expression
                  | Var FuncName
                  | BuiltIn FuncName Type (Value -> Value)
                  | ValueConst Value
                  | Group !(Map.Map FuncName Expression) Type !Expression

data Type = TVar String
            | TPrim Prim
            | TOper String [Type]
            deriving (Eq, Ord)

newtype FuncName = FuncName String deriving (Eq, Ord)


instance Show FuncName where
  show (FuncName name) = name

-- | The name of the function operator
funcOperName = "➜"

-- | create a type that represents a Function
tFun t1 t2 = TOper funcOperName [t1,t2]

startsWithLetter (a:b) = isLetter a

instance Show Expression where
  show (LetExp _ (FuncName name) t1 exp) = "let " ++ name ++ " = " ++ show exp ++ " :: " ++ show t1
  show (Apply _ t2 (Apply t1 _ (Var (FuncName name)) left) right) = show left ++ " " ++ name ++ " " ++ show right
  show (FuncExp (FuncName param) rt exp) = "func " ++ param ++ " = " ++ show exp ++ " :: " ++ show rt
  show (Apply _ _ e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
  show (Var (FuncName name)) = name
  show (ValueConst v) = show v
  show (Group map _ _) = "Group " ++ show map
  show (BuiltIn (FuncName name) tpe _) = name ++ " :: " ++ show tpe
  show (SinkExp _ (FuncName name) tpe _) = "Sink: " ++ name ++ " :: " ++ show tpe
  show (SourceExp _ (FuncName name) tpe) = "Source: " ++ name ++ " :: " ++ show tpe



instance Show Type where
  show (TVar tv) = "TVar " ++ tv
  show (TOper name (a:b:[])) | not $ startsWithLetter name = show a ++ " " ++ name ++ " " ++ show b
  show (TOper name params) = "TOper " ++ name ++ ": " ++ show params
  show (TPrim prim) = show prim

data Prim = PrimDouble | PrimBool | PrimStr deriving (Eq, Ord)

instance Show Prim where
  show PrimDouble = "Double"
  show PrimBool = "Bool"
  show PrimStr = "String"

valuePrim :: Value -> Type
valuePrim (DoubleValue _) = TPrim PrimDouble
valuePrim (StrValue _) = TPrim PrimStr
valuePrim (BoolValue _) = TPrim PrimBool

data Value = DoubleValue Double
             | StrValue String
             | BoolValue Bool
             | UndefinedValue
             | FuncValue (Value -> Value)

instance Eq Value where
  DoubleValue d == DoubleValue d' = d == d'
  StrValue d == StrValue d' = d == d'
  BoolValue d == BoolValue d' = d == d'
  _ == _ = False

instance Show Value where
    show (DoubleValue i) = show i
    show (StrValue i) = show i
    show (BoolValue i) = show i
    show (FuncValue i) = "Value -> Value"

data AllTypeVars = AllTypeVars !(Map.Map String TVarInfo) deriving (Show)

type VarScope = Map.Map FuncName Type

type LetScope = Map.Map FuncName Expression

data TVarInfo = TVarInfo String !Expression (Maybe Type) deriving (Show)


