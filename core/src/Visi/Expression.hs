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
import Control.Monad.Error
import Control.Applicative

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
-- | Look up a type variable
findType :: Type -> AllTypeVars -> ThrowsError TVarInfo
findType (TVar t1) (AllTypeVars map) = 
    case Map.lookup t1 map of
        (Just v) -> return v
        _ -> throwError $ TypeError $ "Cannot find type var: " ++ t1
findType tv _ = throwError $ TypeError $ "Trying to look up " ++ (show tv) ++ " but it's not a Type Variable"

setTVIType (TVarInfo name exp set _) newType = TVarInfo name exp set newType

setATV (TVar t1) newTV (AllTypeVars map) = return $ AllTypeVars $ Map.insert t1 newTV map
setATV theType _ _ = throwError $ TypeError $ "Trying to update a type variable, but got " ++ (show theType)

testBothMust ot1 ot2 atv =
    case runIt of
        (Left _) -> False
        (Right ret) -> ret
    where runIt =
                do
                  ts1 <- typeSeen ot1 atv
                  ts2 <- typeSeen ot2 atv
                  let bothMust = (isMust ts1) && (isMust ts2)
                  return bothMust

{- t1 mustBe equal to t2 -}
mustBe :: String -> Type -> Type -> AllTypeVars -> ThrowsError AllTypeVars
{-
mustBe _ t1 t2 atv@(AllTypeVars map) | trace info False = error "Foo"
            where info = "mustBe " ++ magic t1 ++ " t2 "++ magic t2
                  magic (TVar name) = "TVar " ++ name ++ " " ++ show (map Map.! name)
                  magic t = show t
-}
mustBe _ t1 t2 atv | t1 == t2 = return atv -- they are already the same
mustBe _ t1 t2 atv | (isTParam t1) && (isTPrim t2) = return atv -- If they are both type parameters... punt -- FIXME
mustBe whre ot1 ot2 atv | testBothMust ot1 ot2 atv =
  do
      ts1 <- typeSeen ot1 atv
      ts2 <- typeSeen ot2 atv
      let bothMust = (isMust ts1) && (isMust ts2)
      case (ts1, ts2) of 
        (t1, t2) | t1 == t2 -> return atv
        (ts1, ts2) | (isVar ts1) && (isPrim ts2) -> 
            do
                foundTInfo <- findType ot1 atv
                let newTypeVarInfo = setTVIType foundTInfo ts2
                setATV ot1 newTypeVarInfo atv
        (ts1, ts2) | (isPrim ts1) && (isVar ts2) ->
            do
                foundInfo <- findType ot2 atv
                let newTypeVarInfo = setTVIType foundInfo ts1
                setATV ot2 newTypeVarInfo atv
        (ts1, ts2) | (isVar ts1) && (isVar ts2) -> return atv -- trace ("hmmm... merge "++t1++" and "++t2) atv -- FIXME do something more??
        _ -> throwError $ TypeError $ "BothMust " ++ show ts1 ++ " " ++ show ts2

        
mustBe whre t1 t2 _ | fixedType t1 && fixedType t2 = throwError $ TypeError $ "disimislar fixed types " ++ show t1 ++ 
                                                          " and " ++ show t2 ++ " in " ++ whre
-- mustBe whre t1 t2 atv | fixedType t1 && isTvar t2 = mustBe ("Reverse " ++ whre) t2 t1 atv
mustBe whre ot1@(TVar _) ot2 atv@(AllTypeVars map) = 
    do 
        cur <- findType ot1 atv
        (atv', revised) <- case cur of
                                TVarInfo name exp set NoType -> 
                                    return $ (atv, TVarInfo name exp (ot2 `Set.insert` set) (MustBe ot2))
                                TVarInfo name exp set (MustBe (TPrim _)) ->
                                    do
                                        newATV <- mustBe ("Reverse "++whre) ot2 ot1 atv
                                        return $ (newATV, cur)
                                TVarInfo name exp set (MustBe ot) | ot == ot2 -> 
                                    return $ (atv, TVarInfo name exp (ot2 `Set.insert` set) (MustBe ot2))
                                TVarInfo name exp set (MustBe tv@(TVar tvn)) ->
                                      do
                                          atv' <- mustBe whre tv ot2 atv
                                          return $ (atv', TVarInfo name exp (tv `Set.insert` set) (MustBe ot2))
                                      
                                TVarInfo name _ set what -> throwError $ TypeError $ "Trying to make " ++ name ++ " set " ++
                                                              show set ++ " and what " ++ show what ++ 
                                                              " must be " ++ show ot2 ++ " in " ++ whre
        atv'' <- setATV ot1 revised atv'
        return atv''

mustBe from (TFun f1 f2) (TFun f1' f2') atv = 
    do
        atv' <- mustBe ("Inner tfun "++from) f2 f2' atv
        atv'' <- mustBe ("Outer tfun "++from) f1 f1' atv'
        return atv''
        
mustBe whre t1 t2@(TVar tv2) atv@(AllTypeVars map) =
       let tv2Info = vtrace ("MB2: " ++ show tv2) (map Map.! tv2) in
       case infoType tv2Info of 
         Right(t, exp) ->  mustBe (whre ++ ", expression: " ++ show exp) t1 t atv
         Left True -> mustBe (whre ++ " known parameter type") t2 t1 atv
         _ -> error $ "From: " ++ whre ++ " Unable to unify " ++ show t1 ++ " with " ++ show tv2Info
mustBe whre t1 t2 _ = throwError $ TypeError $ "From: " ++ whre ++ " Unable to unify (fall through) " ++ show t1 ++ " with " ++ show t2

fixedType (TPrim _) = True
fixedType _ = False

isTvar (TVar _) = True
isTvar _ = False

isMust (MustBe _) = True
isMust _ = False

-- | Given a Type Variable, what is the type we've seen it to be?
typeSeen :: Type -> AllTypeVars -> ThrowsError TypeSeen
typeSeen tvar atv = 
    do
        atv <- findType tvar atv
        return $ case atv of
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
fixApply (LetId id) (TParam p) = (TVar $ p ++ id)
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

-- | collect all the type variable substitutions
collectSubs :: VarScope -> AllTypeVars -> Expression -> ThrowsError (AllTypeVars, Type)
collectSubs scope atv (ValueConst v) = return (atv, valuePrim v)
collectSubs scope atv (BuiltIn _ t _) = return (atv, t)
collectSubs scope atv (SourceExp _ _ t) = return (atv, t)
collectSubs scope atv (LetExp _ name t1 exp) = 
    let scope' = Map.insert name t1 scope in
    do
        (atv', t1') <- collectSubs scope' atv exp
        atv'' <- mustBe ("Let " ++ show name) t1 t1' atv'
        return (atv'', t1')  
collectSubs scope atv (SinkExp _ name t1 exp) = 
    let scope' = Map.insert name t1 scope in
    do
        (atv', t1') <- collectSubs scope' atv exp
        atv'' <- mustBe ("Sink " ++ show name) t1 t1' atv'
        return (atv'', t1')
collectSubs scope atv (Var t1 funcName) =
    do
        varType <- justOr (Map.lookup funcName scope) (throwError $ TypeError $ "Can't find " ++ show funcName ++ " in current scope")
        atv' <- mustBe ("other var "++show funcName) t1 varType atv
        return (atv', varType)
 --     mustBe ("Var " ++ show funcName) t1 varType atv, varType) -- FIXME -- is this right?
collectSubs scope atv (FuncExp paramName pt rt exp) =
    let scope' = Map.insert paramName pt scope in
    do
        (atv', rt') <- collectSubs scope' atv exp
        atv'' <- mustBe ("Func " ++ show paramName) rt rt' atv'
        return (atv'', TFun pt rt')
collectSubs scope atv (Apply letId t1' t2' exp1 exp2) =
    do
        let t1 = fixApply letId t1'
        let t2 = fixApply letId t2'
        (satv, st) <- collectSubs scope atv exp1
        (funParam, funRet) <- (resolvedOr st (\v -> v |- (\t -> findType t atv) |- tvarMust)) |- expectFuncType
        (atv'', paramType) <- collectSubs scope satv exp2
        atv''' <- mustBe "func param" (fixApply letId t1) (fixApply letId funParam) atv'' |-
                  mustBe "Pushing the param type back [fixed]" (fixApply letId paramType) (fixApply letId t1) |-
                  mustBe "Return type" (fixApply letId t2) (fixApply letId funRet)
        return (atv''', funRet)
collectSubs scope atv (Group exprs t1 exp) =
    let foldMe name (LetExp _ _ t1 _) map = Map.insert name t1 map
        foldMe name (SinkExp _ _ t1 expr) map = Map.insert name t1 map
        foldMe name (SourceExp _ _ t1) map = Map.insert name t1 map
        foldMe name (BuiltIn _ t1 _) map = Map.insert name t1 map in
    let scope' = Map.foldrWithKey foldMe scope exprs in
    let doAnExp curAtv exp = 
                        do
                            atv' <- curAtv
                            (ret, _) <- collectSubs scope' atv' exp
                            return ret
                        in
    let atv' = List.foldl' doAnExp (return atv) $ Map.elems exprs in
    do
        innerAtv <- atv'
        (atv'', tret) <- collectSubs scope' innerAtv exp
        retAtv <- mustBe "Group" t1 tret atv''
        return (retAtv, tret)
-- collectSubs _ _ exp = throwError $ TypeError $ "Trying to collectSubs but can't collect them for " ++ show exp

resolvedOr tv@(TVar _) f = f $ Right tv
resolvedOr t _ = Right t

(|-) :: Either b a -> (a -> Either b c) -> Either b c
(Right a) |- f = f a
(Left b) |- _ = Left b

expectFuncType (TFun a b) = Right $ (a,b)
expectFuncType t = throwError $ TypeError $ "Looking for a Function, but got type " ++ show t

asTVar (TVar tv) = Right tv
asTVar t = throwError $ TypeError $ "Looking for a Type Variable, but got " ++ show t

tvarMust (TVarInfo _ _ _ (MustBe t)) = Right t
tvarMust (TVarInfo name _ _ found) = throwError $ TypeError $ "Trying to resolve the type for " ++ name ++ " but only got " ++ show found