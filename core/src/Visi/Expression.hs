module Visi.Expression ( 
                            Expression(LetExp, InnerLet, FuncExp, Apply,
                                       SinkExp, SourceExp, InvokeMethod,
                                       Var, BuiltIn, ValueConst,
                                       Group),
                            SourceLoc(NoSourceLoc, SourceFromURL, SourceLoc),
                           VarScope, LetScope, 
                           Type(TVar, TPrim, TOper, StructuralType),
                           FuncName(FuncName), 
                           LetId(LetId),
                           Prim(PrimDouble, PrimBool, PrimStr),
                           valuePrim,
                           SourcePoint, SourceSpan,
                           builtInLoc,
                           tFun, funcOperName,
                           defaultValueForType,
                           getSourceLoc,
                           HasSourceLoc,
                           SourceInfo,
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
 * Portions created by the Initial Developer are Copyright (C) 2011-2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** -}

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Char

newtype LetId = LetId T.Text deriving (Eq, Ord, Show)

type CanBeGeneric = Bool

type SourcePoint = (Int, Int)
type SourceSpan = (SourcePoint, SourcePoint)
type SourceInfo = (SourceSpan, T.Text)

data SourceLoc = NoSourceLoc 
                 | BuiltInSource String SourceLoc
                 | SourceFromURL String SourceInfo SourceLoc
                 | SourceLoc SourceInfo SourceLoc deriving (Show, Eq)

data Expression = LetExp SourceLoc LetId FuncName CanBeGeneric Type Expression
                  | InnerLet SourceLoc Type Expression Expression -- defines a Let plus an expression to be evaluated in the scope of the Let
                  | SinkExp SourceLoc LetId FuncName Type Expression
                  | SourceExp SourceLoc LetId FuncName Type
                  | InvokeMethod SourceLoc LetId FuncName Type
                  | FuncExp SourceLoc FuncName Type Expression
                  | Apply SourceLoc LetId Type Expression Expression
                  | Var SourceLoc FuncName
                  | BuiltIn SourceLoc FuncName Type (Value -> Value)
                  | ValueConst SourceLoc Value
                  | Group SourceLoc (Map.Map FuncName Expression) Type Expression

data Type = TVar T.Text
            | TPrim Prim
            | TOper T.Text [Type]
            | StructuralType (Map.Map T.Text Type) -- Structural type
            deriving (Eq, Ord)

newtype FuncName = FuncName T.Text deriving (Eq, Ord)

instance Show FuncName where
  show (FuncName name) = T.unpack name

-- | The name of the function operator
funcOperName = T.pack "->"

-- | create a type that represents a Function
tFun t1 t2 = TOper funcOperName [t1,t2]

startsWithLetter (a:b) = isLetter a


class HasSourceLoc a where
  getSourceLoc :: a -> SourceLoc

instance HasSourceLoc SourceLoc where
  getSourceLoc a = a

instance HasSourceLoc Expression where
  getSourceLoc (LetExp sl _ (FuncName name) _ t1 exp) = sl
  getSourceLoc (InnerLet sl _ letExp evalExp) = sl
  getSourceLoc (Apply sl _ t2 _ right) = sl
  getSourceLoc (FuncExp sl (FuncName param) rt exp) = sl
  getSourceLoc (InvokeMethod sl _ (FuncName name) _) = sl
  getSourceLoc (Var sl (FuncName name)) = sl
  getSourceLoc (ValueConst sl v) = sl
  getSourceLoc (Group sl map _ _) = sl
  getSourceLoc (BuiltIn sl (FuncName name) tpe _) = sl
  getSourceLoc (SinkExp sl _ (FuncName name) tpe _) = sl
  getSourceLoc (SourceExp sl _ (FuncName name) tpe) = sl

instance Show Expression where
  show (LetExp _ _ (FuncName name) _ t1 exp) = "let " ++ T.unpack name ++ " = " ++ show exp ++ " :: " ++ show t1
  show (InnerLet _ _ letExp evalExp) = show letExp ++ "\n" ++ show evalExp
  show (Apply _ _ t2 (Apply _ t1 _ (Var _ (FuncName name)) left) right) = show left ++ " " ++ T.unpack name ++ " " ++ show right
  show (FuncExp _ (FuncName param) rt exp) = "func " ++ T.unpack param ++ " = " ++ show exp ++ " :: " ++ show rt
  show (Apply _ _ _ e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
  show (InvokeMethod _ _ (FuncName name) _) = "#" ++ T.unpack name
  show (Var _ (FuncName name)) = T.unpack name
  show (ValueConst _ v) = show v
  show (Group _ map _ _) = "Group " ++ show map
  show (BuiltIn _ (FuncName name) tpe _) = T.unpack name ++ " :: " ++ show tpe
  show (SinkExp _ _ (FuncName name) tpe _) = "Sink: " ++ T.unpack name ++ " :: " ++ show tpe
  show (SourceExp _ _ (FuncName name) tpe) = "Source: " ++ T.unpack name ++ " :: " ++ show tpe



instance Show Type where
  show (TVar tv) = "TVar " ++ T.unpack tv
  show (TOper name (a:b:[])) | not $ startsWithLetter $ T.unpack name = show a ++ " " ++ T.unpack name ++ " " ++ show b
  show (TOper name params) = "TOper " ++ T.unpack name ++ ": " ++ show params
  show (TPrim prim) = show prim
  show (StructuralType info) = "StructuralType " ++ show info

data Prim = PrimDouble | PrimBool | PrimStr deriving (Eq, Ord)


defaultValueForType (TPrim PrimDouble) = DoubleValue 0.0
defaultValueForType (TPrim PrimStr) = StrValue $ T.pack ""
defaultValueForType (TPrim PrimBool) = BoolValue False
defaultValueForType _ = UndefinedValue

instance Show Prim where
  show PrimDouble = "Double"
  show PrimBool = "Bool"
  show PrimStr = "String"

valuePrim :: Value -> Maybe Type
valuePrim (DoubleValue _) = Just $ TPrim PrimDouble
valuePrim (StrValue _) = Just $ TPrim PrimStr
valuePrim (BoolValue _) = Just $ TPrim PrimBool
valuePrim _ = Nothing

builtInLoc name = BuiltInSource name NoSourceLoc

data Value = DoubleValue Double
             | StrValue T.Text
             | BoolValue Bool
             | UndefinedValue -- FIXME we hate undefined and it should go away 'cause it's null
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



type VarScope = Map.Map FuncName Type

type LetScope = Map.Map FuncName Expression



