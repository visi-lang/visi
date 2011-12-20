module Visi.Expression ( 
                            Expression(LetExp, InnerLet, FuncExp, Apply,
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
import qualified Data.Text as T
import Visi.Util
import Data.Char

newtype LetId = LetId T.Text deriving (Eq, Ord, Show)

type CanBeGeneric = Bool

data Expression = LetExp LetId FuncName CanBeGeneric Type Expression
                  | InnerLet Type Expression Expression -- defines a Let plus an expression to be evaluated in the scope of the Let
                  | SinkExp LetId FuncName Type Expression
                  | SourceExp LetId FuncName Type
                  | FuncExp FuncName Type Expression
                  | Apply LetId Type Expression Expression
                  | Var FuncName
                  | BuiltIn FuncName Type (Value -> Value)
                  | ValueConst Value
                  | Group (Map.Map FuncName Expression) Type Expression

data Type = TVar T.Text
            | TPrim Prim
            | TOper T.Text [Type]
            deriving (Eq, Ord)

newtype FuncName = FuncName T.Text deriving (Eq, Ord)


instance Show FuncName where
  show (FuncName name) = T.unpack name

-- | The name of the function operator
funcOperName = T.pack "->"

-- | create a type that represents a Function
tFun t1 t2 = TOper funcOperName [t1,t2]

startsWithLetter (a:b) = isLetter a

instance Show Expression where
  show (LetExp _ (FuncName name) _ t1 exp) = "let " ++ (T.unpack name) ++ " = " ++ show exp ++ " :: " ++ show t1
  show (InnerLet _ letExp evalExp) = show letExp ++ "\n" ++ show evalExp
  show (Apply _ t2 (Apply t1 _ (Var (FuncName name)) left) right) = show left ++ " " ++ (T.unpack name) ++ " " ++ show right
  show (FuncExp (FuncName param) rt exp) = "func " ++ (T.unpack param) ++ " = " ++ show exp ++ " :: " ++ show rt
  show (Apply _ _ e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
  show (Var (FuncName name)) = T.unpack name
  show (ValueConst v) = show v
  show (Group map _ _) = "Group " ++ show map
  show (BuiltIn (FuncName name) tpe _) = (T.unpack name) ++ " :: " ++ show tpe
  show (SinkExp _ (FuncName name) tpe _) = "Sink: " ++ (T.unpack name) ++ " :: " ++ show tpe
  show (SourceExp _ (FuncName name) tpe) = "Source: " ++ (T.unpack name) ++ " :: " ++ show tpe



instance Show Type where
  show (TVar tv) = "TVar " ++ T.unpack tv
  show (TOper name (a:b:[])) | not $ startsWithLetter $ T.unpack name = show a ++ " " ++ (T.unpack name) ++ " " ++ show b
  show (TOper name params) = "TOper " ++ (T.unpack name) ++ ": " ++ show params
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
             | StrValue T.Text
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

data AllTypeVars = AllTypeVars (Map.Map T.Text TVarInfo) deriving (Show)

type VarScope = Map.Map FuncName Type

type LetScope = Map.Map FuncName Expression

data TVarInfo = TVarInfo T.Text Expression (Maybe Type) deriving (Show)


