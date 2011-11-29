module Visi.Expression ( 
                            Expression(LetExp, FuncExp, Apply,
                                       SinkExp, SourceExp,
                                       Var, BuiltIn, ValueConst,
                                       Group),
                           TypeSeen(NoType, MustBe, CouldBe),
                           AllTypeVars(AllTypeVars),
                           VarScope, LetScope, TVarInfo(TVarInfo),
                           Type(TVar, TPrim, TFun, TParam),
                           FuncName(FuncName), 
                           LetId(LetId),
                           Prim(PrimDouble, PrimBool, PrimStr),
                           valuePrim, collectVars, collectSubs, resolveLets, thread,
                           loopSet, flatten, letScope, mm,
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
-- import Control.Monad.Error

newtype LetId = LetId String deriving (Eq, Ord, Show)

data Expression = LetExp LetId FuncName Type !Expression 
                  | SinkExp LetId FuncName Type !Expression
                  | SourceExp LetId FuncName Type
                  | FuncExp FuncName Type Type !Expression
                  | Apply LetId Type Type Expression !Expression
                  | Var Type FuncName
                  | BuiltIn FuncName Type (Value -> Value)
                  | ValueConst Value
                  | Group !(Map.Map FuncName Expression) Type !Expression

data Type = TVar String |
            TPrim Prim |
            TParam String |
            TFun Type Type
            deriving (Eq, Ord)

newtype FuncName = FuncName String deriving (Eq, Ord)


instance Show FuncName where
  show (FuncName name) = name


instance Show Expression where
  show (LetExp _ (FuncName name) t1 exp) = "let " ++ name ++ " = " ++ show exp ++ " :: " ++ show t1
  show (Apply _ t2 t3 (Apply _ t1 _ (Var _ (FuncName name)) left) right) = show left ++ " " ++ name ++ " " ++ show right
  show (FuncExp (FuncName param) pt rt exp) = "func " ++ param ++ " = " ++ show exp ++ " :: " ++ 
                                                              show pt ++ " -> " ++ show rt
  show (Apply _ _ _ e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
  show (Var _ (FuncName name)) = name
  show (ValueConst v) = show v
  show (Group map _ _) = "Group " ++ show map
  show (BuiltIn (FuncName name) tpe _) = name ++ " :: " ++ show tpe
  show (SinkExp _ (FuncName name) tpe _) = "Sink: " ++ name ++ " :: " ++ show tpe
  show (SourceExp _ (FuncName name) tpe) = "Source: " ++ name ++ " :: " ++ show tpe

instance Show Type where
  show (TVar tv) = "TVar " ++ tv
  show (TParam i) = "TParam " ++ show i
  show (TPrim prim) = show prim
  show (TFun t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

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

instance Show Value where
    show (DoubleValue i) = show i
    show (StrValue i) = show i
    show (BoolValue i) = show i
    show (FuncValue i) = "Value -> Value"



data TypeSeen = NoType
                | MustBe Type
                | CouldBe (Set.Set Type)
                deriving (Show, Eq)

data AllTypeVars = AllTypeVars !(Map.Map String TVarInfo) deriving (Show)

type VarScope = Map.Map FuncName Type

type LetScope = Map.Map FuncName Expression

data TVarInfo = TVarInfo String !Expression (Set.Set Type)! TypeSeen deriving (Show)


{- ----------------- functions ------------------------- -}
{- t1 mustBe equal to t2 -}
mustBe :: String -> Type -> Type -> AllTypeVars -> AllTypeVars
{-
mustBe _ t1 t2 atv@(AllTypeVars map) | trace info False = error "Foo"
            where info = "mustBe " ++ magic t1 ++ " t2 "++ magic t2
                  magic (TVar name) = "TVar " ++ name ++ " " ++ show (map Map.! name)
                  magic t = show t
-}
mustBe _ t1 t2 atv | t1 == t2 = atv -- they are already the same
mustBe _ t1 t2 atv | (isTParam t1) && (isTPrim t2) = atv
-- mustBe _ t1 t2 atv | (isTParam t2) && (isTPrim t1) = atv
mustBe whre ot1@(TVar t1) ot2@(TVar t2) atv@(AllTypeVars map) | bothMust =
  case (ts1, ts2) of 
    (t1, t2) | t1 == t2 -> atv
    (ts1, ts2) | (isVar ts1) && (isPrim ts2) -> AllTypeVars $ Map.insert t1 revised map
             where revised = case (vtrace ("L1: " ++ show t1) (map Map.! t1)) of
                               TVarInfo name exp set _ -> TVarInfo name exp set ts2
    (ts1, ts2) | (isPrim ts1) && (isVar ts2) -> AllTypeVars $ Map.insert t2 revised map
             where revised = case (vtrace ("L2: " ++ show t2) (map Map.! t2)) of
                               TVarInfo name exp set _ -> TVarInfo name exp set ts1
    (ts1, ts2) | (isVar ts1) && (isVar ts2) -> atv -- trace ("hmmm... merge "++t1++" and "++t2) atv -- FIXME do something more??
    _ -> error $ "BothMust " ++ show ts1 ++ " " ++ show ts2
  where ts1 = typeSeen t1 map
        ts2 = vtrace ("T2 is: " ++ whre) (typeSeen t2 map)
        bothMust = (isMust ts1) && (isMust ts2)
mustBe whre t1 t2 _ | fixedType t1 && fixedType t2 = error $ "disimislar fixed types " ++ show t1 ++ 
                                                          " and " ++ show t2 ++ " in " ++ whre
-- mustBe whre t1 t2 atv | fixedType t1 && isTvar t2 = mustBe ("Reverse " ++ whre) t2 t1 atv
mustBe whre ot1@(TVar t1) t2 atv@(AllTypeVars map) = 
    let cur = vtrace ("MB1: " ++ show t1) (map Map.! t1) in
    let (atv'', revised) = case cur of 
                    TVarInfo name exp set NoType -> 
                        (atv, TVarInfo name exp (t2 `Set.insert` set) (MustBe t2))
                    TVarInfo name exp set (MustBe (TPrim _)) ->
                        (mustBe ("Reverse "++whre) t2 ot1 atv, cur)
                    TVarInfo name exp set (MustBe ot) | ot == t2 -> 
                        (atv, TVarInfo name exp (t2 `Set.insert` set) (MustBe t2))
                    TVarInfo name exp set (MustBe tv@(TVar tvn)) ->
                      let atv' = mustBe whre tv t2 atv in
                      (atv', TVarInfo name exp (tv `Set.insert` set) (MustBe t2))
                    TVarInfo name _ set what -> error $ "Trying to make " ++ name ++ " set " ++
                                                  show set ++ " and what " ++ show what ++ 
                                                  " must be " ++ show t2 ++ " in " ++ whre
        in
    let mapSigma = case atv'' of AllTypeVars m -> m in
    let map' = Map.insert t1 revised mapSigma in
    let ret = reviseAllTypes t2 ot1 $ AllTypeVars map' in
    ret
mustBe from (TFun f1 f2) (TFun f1' f2') atv = mustBe ("Outer tfun "++from) f1 f1' $ mustBe ("Inner tfun "++from) f2 f2' atv
mustBe whre t1 t2@(TVar tv2) atv@(AllTypeVars map) =
       let tv2Info = vtrace ("MB2: " ++ show tv2) (map Map.! tv2) in
       case infoType tv2Info of 
         Right(t, exp) ->  mustBe (whre ++ ", expression: " ++ show exp) t1 t atv
         Left True -> mustBe (whre ++ " known parameter type") t2 t1 atv
         _ -> error $ "From: " ++ whre ++ " Unable to unify " ++ show t1 ++ " with " ++ show tv2Info
mustBe whre t1 t2 _ = error $ "From: " ++ whre ++ " Unable to unify (fall through) " ++ show t1 ++ " with " ++ show t2

fixedType (TPrim _) = True
fixedType _ = False

isTvar (TVar _) = True
isTvar _ = False

isMust (MustBe _) = True
isMust _ = False

typeSeen name map = case vtrace ("TS: "++ show name ++ " in " ++ show map) (map Map.! name) of
         (TVarInfo _ _ _ ret) -> ret

isPrim (MustBe (TPrim _)) = True
isPrim _ = False

isVar (MustBe (TVar _)) = True
isVar (MustBe (TParam _)) = True
isVar _ = False

isTParam (TParam _) = True
isTParam _ = False

isTPrim (TPrim _) = True
isTPrim _ = False

infoType :: TVarInfo -> Either Bool (Type, Expression)
infoType (TVarInfo _ exp _ (MustBe t)) = Right (t, exp)
infoType (TVarInfo _ exp _ NoType) = Left True
infoType _ = Left False

reviseAllTypes :: Type -> Type -> AllTypeVars -> AllTypeVars
reviseAllTypes (TVar tv) seen (AllTypeVars map) = 
    let (TVarInfo name exp set tpe) = vtrace ("RT: " ++ show tv)  (map Map.! tv) in
    let revised = TVarInfo name exp (seen `Set.insert` set) tpe in
    AllTypeVars $ Map.insert tv revised map
reviseAllTypes _ _ ret = ret


mm (AllTypeVars map) = map

resolveLets :: Expression -> AllTypeVars -> (AllTypeVars, [(String, Type)])
resolveLets (Group map _ _) atv = List.foldl' resolve (atv, []) (Map.elems map)
            where resolve (atv, lst) (LetExp _ (FuncName name) t _) = 
                      let (t', atv') = thread loopSet t atv in
                      (atv', (name, t') : lst)
                  resolve (atv, lst) what = (atv, lst)

fixSet :: String -> Type -> LoopSet -> LoopSet
fixSet name (TVar tv) (LoopSet set lst) = 
       if Set.member tv set then
          error $ "Looping on "++tv
       else callSet name $ LoopSet (Set.insert tv set) lst
fixSet name _ set = callSet name set

data LoopSet = LoopSet (Set.Set String) [String]

loopSet = LoopSet Set.empty []

expando :: Set.Set String -> AllTypeVars -> Set.Set String
expando root atv@(AllTypeVars map) = 
   let exps = List.map (map Map.!) $ Set.toList root in
   let root' = Set.fromList $ flatten $ List.map tvString exps in
   if root == root' then root else expando root' atv

expLoopSet :: LoopSet -> AllTypeVars -> String
expLoopSet (LoopSet set _) atv@(AllTypeVars map) =
   let exps = List.map (map Map.!) $ Set.toList set in
   let subs = Set.toList $ expando set atv in
   let allSeen = flatten $ List.map (\a -> show a ++ "\n") $ List.map (map Map.!) subs in
   let strs = flatten $ List.map (\e -> show e ++ "\n") exps in
   "Dude: " ++ strs ++ " \nAll Seen "++ allSeen

tvString :: TVarInfo -> [String]
tvString (TVarInfo _ _ set _) = flatten $ List.map tvName $ Set.toList set

tvName (TVar tv) = [tv]
tvName _ = []

instance Show LoopSet where
  show (LoopSet set lst) = "LoopSet "++show set++" "++ show lst

callSet :: String -> LoopSet -> LoopSet
callSet name (LoopSet set lst) = LoopSet set $ lst ++ [name]

getTypeInfo :: String -> AllTypeVars -> TVarInfo
getTypeInfo name (AllTypeVars map) = vtrace ("GTI: " ++ show name) (map Map.! name)

insertTypeVar :: TVarInfo -> AllTypeVars -> AllTypeVars
insertTypeVar tv@(TVarInfo name _ _ _) (AllTypeVars map) = AllTypeVars $ Map.insert name tv map

thread :: LoopSet -> Type -> AllTypeVars -> (Type, AllTypeVars)
thread ls@(LoopSet set _) (TVar tv) world | Set.member tv set = error $ "Loop on threading "++tv++" set "++show ls ++ "\n" ++ (expLoopSet ls world)
thread set tf@(TFun t1 t2) atv = 
       let (t2', atv') = thread (callSet ("F2 " ++ show tf) set) t2 atv in
       let (t1', atv'') = thread (callSet ("F1 " ++ show tf) set) t1 atv' in
       (TFun t1' t2', atv'')
thread set t@(TVar tv) atv = 
       let newSet = fixSet ("A TVar "++show t) t set in
       case getTypeInfo tv atv of
         TVarInfo _ _ _ (MustBe pt@(TPrim _)) -> (pt, atv)
         TVarInfo name exp set (MustBe pt@(TVar inner)) ->
           let (rt, atm) = thread (callSet ("inner "++show t++" with "++inner) newSet) pt atv in
           let ut = TVarInfo name exp set $ MustBe rt in
           let atv'' = insertTypeVar ut atv in
           (rt, atv'')
         tv@(TVarInfo name exp set NoType) -> 
           error $ "Failed to find a type " ++ name ++ " exp "++ show exp ++ " tv " ++ show set ++
                   " atv " ++ (flatten $ List.map (\a -> (show $ snd a) ++ "\n") $ Map.toList $ mm atv)
         gak -> (t, atv) -- error $ "Gak show " ++ show (t, atv)
thread _ t atv = (t, atv)


letScope :: Expression -> LetScope
letScope (Group map _ _) = map
letScope _ = Map.empty

mergeATV :: AllTypeVars -> AllTypeVars -> AllTypeVars
mergeATV (AllTypeVars m1) (AllTypeVars m2) = AllTypeVars $ m1 `Map.union` m2

-- | in the Apply has a type parameter, convert it to a type variable
fixApply :: LetId -> Type -> Type
fixApply (LetId id) (TParam p) = vtrace ("FixApp: "++p) (TVar $ p ++ id)
fixApply li (TFun t1 t2) = TFun (fixApply li t1) (fixApply li t2)
fixApply _ t = t

fixTp :: Maybe LetId -> Type -> Type
fixTp (Just (LetId id)) (TParam p) = vtrace ("FIXTP: " ++ p) (TVar $ p ++ id)
fixTp li@(Just (LetId id)) (TFun t1 t2) = TFun (fixTp li t1) (fixTp li t2)
fixTp _ t = t

{- Convert a Type into an AllTypeVars -}
typeToAllVars :: Type -> Expression -> AllTypeVars
typeToAllVars (TVar name) exp = AllTypeVars $ Map.singleton name $ TVarInfo name exp Set.empty NoType
typeToAllVars (TFun t1 t2) exp = (typeToAllVars t1 exp) `mergeATV` (typeToAllVars t2 exp)
typeToAllVars _ _ = AllTypeVars Map.empty

-- | Given an expression, find all the type variables in the expression
collectVars :: (Maybe LetId) -> Expression -> AllTypeVars
collectVars li e@(LetExp _ _ t1 exp) = (typeToAllVars (fixTp li t1) e) `mergeATV` (collectVars li exp)
-- collectVars li e@(FuncExp _ t1 t2 exp) = (typeToAllVars (fixTp li t1) e) `mergeATV` (typeToAllVars (fixTp li t2) e) `mergeATV` (collectVars li exp)
collectVars li e@(Apply letId t1 t2 e1 e2) = (typeToAllVars (fixApply letId t1) e2) `mergeATV` 
                                    (typeToAllVars (fixApply letId t2) e) `mergeATV` (collectVars (Just letId) e1)
                                    `mergeATV` (collectVars (Just letId) e2)
collectVars li e@(Var t1 _) = (typeToAllVars (fixTp li t1) e)
collectVars li e@(BuiltIn _ t1 _) = vtrace ("Built in li " ++ show li ++ " t1 " ++ show t1) (typeToAllVars (fixTp li t1) e)
collectVars li (Group map t1 exp) = Map.fold (mergeATV . (collectVars li)) (AllTypeVars Map.empty) map
                                 `mergeATV` (typeToAllVars (fixTp li t1) exp) `mergeATV` (collectVars li exp)

collectVars li e@(SinkExp _ _ t1 expr) = (typeToAllVars (fixTp li t1) e) `mergeATV` (collectVars li expr)
collectVars li e@(SourceExp _ _ t1) = (typeToAllVars (fixTp li t1) e)
collectVars li e@(FuncExp paramName pt rt exp) = (typeToAllVars (fixTp li pt) e) `mergeATV`
                                                 (typeToAllVars (fixTp li rt) e) `mergeATV`
                                                 (collectVars li exp)
-- FIXME Are there any other expressions we want to look at?
collectVars li e = vtrace ("Missed " ++ show e ++ "/" ++ show li) (AllTypeVars Map.empty)

collectSubs :: VarScope -> AllTypeVars -> Expression -> (AllTypeVars, Type)
collectSubs scope atv (ValueConst v) = (atv, valuePrim v)
collectSubs scope atv (BuiltIn _ t _) = (atv, t)
collectSubs scope atv (SourceExp _ _ t) = (atv, t)
collectSubs scope atv (LetExp _ name t1 exp) = 
    let scope' = Map.insert name t1 scope in
    let (atv', t1') = collectSubs scope' atv exp in
    (mustBe ("Let " ++ show name) t1 t1' atv', t1')  
collectSubs scope atv (SinkExp _ name t1 exp) = 
    let scope' = Map.insert name t1 scope in
    let (atv', t1') = collectSubs scope' atv exp in
    (mustBe ("Sink " ++ show name) t1 t1' atv', t1')
collectSubs scope atv (Var t1 funcName) =
    let varType = (vtrace ("CS: "++ show scope) (scope Map.! funcName)) in
    (mustBe ("other var "++show funcName) t1 varType atv, varType)
 --     mustBe ("Var " ++ show funcName) t1 varType atv, varType) -- FIXME -- is this right?
collectSubs scope atv (FuncExp paramName pt rt exp) =
    let scope' = Map.insert paramName pt scope in
    let (atv', rt') = collectSubs scope' atv exp in
    (mustBe ("Func " ++ show paramName) rt rt' atv', TFun pt rt')
collectSubs scope atv (Apply letId t1' t2' exp1 exp2) =
    let t1 = fixApply letId t1' in
    let t2 = fixApply letId t2' in
    let (satv, st) = collectSubs scope atv exp1 in
    let (atv', TFun funParam funRet) = (satv,case st of
                                               (TVar tv) -> case getTypeInfo tv atv of
                                                              (TVarInfo _ _ _ (MustBe t)) -> t
                                                              snark -> error $ "Oh no... it a " ++ show snark ++
                                                                            " t1 " ++ show t1 ++
                                                                            " t2 " ++ show t2 ++
                                                                            " exp1 " ++ show exp1 ++
                                                                            " exp2 " ++ show exp2 ++
                                                                            " satv " ++ show satv ++
                                                                            " st " ++ show st
                                               x -> x) in
    let (atv'', paramType) = collectSubs scope atv' exp2 in
    let atv''' = mustBe "Pushing the param type back [fixed]" (fixApply letId paramType) (fixApply letId t1) $
                 mustBe "Return type" (fixApply letId t2) (fixApply letId funRet) $
                 mustBe "func param" (fixApply letId t1) (fixApply letId funParam) atv'' in
    (atv''', funRet)
collectSubs scope atv (Group exprs t1 exp) =
    let foldMe name (LetExp _ _ t1 _) map = Map.insert name t1 map
        foldMe name (SinkExp _ _ t1 expr) map = Map.insert name t1 map
        foldMe name (SourceExp _ _ t1) map = Map.insert name t1 map
        foldMe name (BuiltIn _ t1 _) map = Map.insert name t1 map in
    let scope' = Map.foldrWithKey foldMe scope exprs in
    let doAnExp curAtv exp = fst $ collectSubs scope' curAtv exp in
    let atv' = List.foldl' doAnExp atv $ Map.elems exprs in
    let (atv'', tret) = collectSubs scope' atv' exp in
    (mustBe "Group" t1 tret atv'', tret)


