module Visi.Expression ( 
                            Expression(LetExp, FuncExp, Apply,
                                       SinkExp, SourceExp,
                                       Var, BuiltIn, ValueConst,
                                       Group),
                           AllTypeVars(AllTypeVars),
                           VarScope, LetScope, TVarInfo(TVarInfo),
                           Type(TVar, TPrim, TFun, TOper),
                           FuncName(FuncName), 
                           LetId(LetId),
                           Prim(PrimDouble, PrimBool, PrimStr),
                           valuePrim, collectVars, collectSubs, resolveLets, thread,
                           loopSet, flatten, letScope, mm,
                           analyze,
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
                  | Var FuncName
                  | BuiltIn FuncName Type (Value -> Value)
                  | ValueConst Value
                  | Group !(Map.Map FuncName Expression) Type !Expression

data Type = TVar String
            | TPrim Prim
            -- TParam String |
            | TOper String [Type]
            | TFun Type Type
            deriving (Eq, Ord)

newtype FuncName = FuncName String deriving (Eq, Ord)


instance Show FuncName where
  show (FuncName name) = name


instance Show Expression where
  show (LetExp _ (FuncName name) t1 exp) = "let " ++ name ++ " = " ++ show exp ++ " :: " ++ show t1
  show (Apply _ t2 t3 (Apply _ t1 _ (Var (FuncName name)) left) right) = show left ++ " " ++ name ++ " " ++ show right
  show (FuncExp (FuncName param) pt rt exp) = "func " ++ param ++ " = " ++ show exp ++ " :: " ++ 
                                                              show pt ++ " -> " ++ show rt
  show (Apply _ _ _ e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
  show (Var (FuncName name)) = name
  show (ValueConst v) = show v
  show (Group map _ _) = "Group " ++ show map
  show (BuiltIn (FuncName name) tpe _) = name ++ " :: " ++ show tpe
  show (SinkExp _ (FuncName name) tpe _) = "Sink: " ++ name ++ " :: " ++ show tpe
  show (SourceExp _ (FuncName name) tpe) = "Source: " ++ name ++ " :: " ++ show tpe

instance Show Type where
  show (TVar tv) = "TVar " ++ tv
  show (TOper name params) = "TOper " ++ name ++ ": " ++ show params
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

data AllTypeVars = AllTypeVars !(Map.Map String TVarInfo) deriving (Show)

type VarScope = Map.Map FuncName Type

type LetScope = Map.Map FuncName Expression

data TVarInfo = TVarInfo String !Expression (Maybe Type) deriving (Show)


{- ----------------- functions ------------------------- -}
-- | Look up a type variable
findType :: Type -> AllTypeVars -> ThrowsError TVarInfo
findType (TVar t1) (AllTypeVars map) = 
    case Map.lookup t1 map of
        (Just v) -> return v
        _ -> throwError $ TypeError $ "Cannot find type var: " ++ t1
findType tv _ = throwError $ TypeError $ "Trying to look up " ++ (show tv) ++ " but it's not a Type Variable"

setTVIType (TVarInfo name exp _) newType = TVarInfo name exp $ newType

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
-- mustBe _ t1 t2 atv | (isTParam t1) && (isTPrim t2) = return atv -- If they are both type parameters... punt -- FIXME
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
                                TVarInfo name exp Nothing -> 
                                    return $ (atv, TVarInfo name exp (Just ot2))
                                TVarInfo name exp (Just (TPrim _)) ->
                                    do
                                        newATV <- mustBe ("Reverse "++whre) ot2 ot1 atv
                                        return $ (newATV, cur)
                                TVarInfo name exp (Just ot) | ot == ot2 -> 
                                    return $ (atv, TVarInfo name exp (Just ot2))
                                TVarInfo name exp (Just tv@(TVar tvn)) ->
                                      do
                                          atv' <- mustBe whre tv ot2 atv
                                          return $ (atv', TVarInfo name exp (Just ot2))
                                      
                                TVarInfo name _ what -> throwError $ TypeError $ "Trying to make " ++ name ++ " set " ++
                                                              " and what " ++ show what ++ 
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
mustBe whre t1 t2 _ = throwError $ TypeError $ "From: " ++ whre ++ " Unable to unify (fall through) " ++ show t1 ++ " with " ++ show t2

fixedType (TPrim _) = True
fixedType _ = False

isTvar (TVar _) = True
isTvar _ = False

isMust (Just _) = True
isMust _ = False

-- | Given a Type Variable, what is the type we've seen it to be?
typeSeen :: Type -> AllTypeVars -> ThrowsError (Maybe Type)
typeSeen tvar atv = 
    do
        atv <- findType tvar atv
        return $ case atv of
                    (TVarInfo _ _ ret) -> ret

isPrim (Just (TPrim _)) = True
isPrim _ = False

isVar (Just (TVar _)) = True
-- isVar (MustBe (TParam _)) = True
isVar _ = False

--isTParam (TParam _) = True
--isTParam _ = False

isTPrim (TPrim _) = True
isTPrim _ = False

infoType :: TVarInfo -> Either Bool (Type, Expression)
infoType (TVarInfo _ exp (Just t)) = Right (t, exp)
infoType (TVarInfo _ exp Nothing) = Left True


reviseAllTypes :: Type -> Type -> AllTypeVars -> ThrowsError AllTypeVars
reviseAllTypes (TVar tv) seen (AllTypeVars map) = 
    do
        (TVarInfo name exp tpe) <- (Map.lookup tv map) `justOr` (throwError $ TypeError $ "Can't find type variable " ++ tv)
        let revised = TVarInfo name exp tpe
        return $ AllTypeVars $ Map.insert tv revised map
reviseAllTypes _ _ ret = return $ ret


mm (AllTypeVars map) = map

resolveLets :: Expression -> AllTypeVars -> ThrowsError (AllTypeVars, [(String, Type)])
resolveLets (Group map _ _) atv = 
            List.foldl' resolve (return (atv, [])) (Map.elems map)
            where resolve pm (LetExp _ (FuncName name) t _) = 
                      do
                          (atv, lst) <- pm
                          (t', atv') <- thread loopSet t atv
                          return (atv', (name, t') : lst)
                  resolve pm what = pm

fixSet :: String -> Type -> LoopSet -> ThrowsError LoopSet
fixSet name (TVar tv) (LoopSet set lst) = 
       if Set.member tv set then
          throwError $ TypeError $ "Looping on "++tv
       else return $ callSet name $ LoopSet (Set.insert tv set) lst
fixSet name _ set = return $ callSet name set

data LoopSet = LoopSet (Set.Set String) [String]

loopSet = LoopSet Set.empty []

tvName (TVar tv) = [tv]
tvName _ = []

instance Show LoopSet where
  show (LoopSet set lst) = "LoopSet "++show set++" "++ show lst

callSet :: String -> LoopSet -> LoopSet
callSet name (LoopSet set lst) = LoopSet set $ lst ++ [name]

getTypeInfo :: String -> AllTypeVars -> TVarInfo
getTypeInfo name (AllTypeVars map) = vtrace ("GTI: " ++ show name) (map Map.! name)

insertTypeVar :: TVarInfo -> AllTypeVars -> AllTypeVars
insertTypeVar tv@(TVarInfo name _ _) (AllTypeVars map) = AllTypeVars $ Map.insert name tv map

thread :: LoopSet -> Type -> AllTypeVars -> ThrowsError (Type, AllTypeVars)
thread ls@(LoopSet set _) (TVar tv) world | Set.member tv set = 
    throwError $ TypeError $ "Loop on threading "++tv++" set "++show ls ++ "\n"
thread set tf@(TFun t1 t2) atv = 
    do
        (t2', atv') <- thread (callSet ("F2 " ++ show tf) set) t2 atv 
        (t1', atv'') <- thread (callSet ("F1 " ++ show tf) set) t1 atv'
        return (TFun t1' t2', atv'')
thread set t@(TVar tv) atv = 
    do
       newSet <- fixSet ("A TVar "++show t) t set
       case getTypeInfo tv atv of
         TVarInfo _ _ (Just pt@(TPrim _)) -> return $ (pt, atv)
         TVarInfo name exp (Just pt@(TVar inner)) ->
           do
             (rt, atm) <- thread (callSet ("inner "++show t++" with "++inner) newSet) pt atv
             let ut = TVarInfo name exp $ Just rt
             let atv'' = insertTypeVar ut atv
             return $ (rt, atv'')
         tv@(TVarInfo name exp Nothing) -> 
           throwError $ TypeError $ "Failed to find a type " ++ name ++ " exp "++ show exp ++ " tv " ++
                                    " atv " ++ (flatten $ List.map (\a -> (show $ snd a) ++ "\n") $ Map.toList $ mm atv)
         gak -> return (t, atv) -- error $ "Gak show " ++ show (t, atv)
thread _ t atv = return (t, atv)


letScope :: Expression -> LetScope
letScope (Group map _ _) = map
letScope _ = Map.empty

mergeATV :: AllTypeVars -> AllTypeVars -> AllTypeVars
mergeATV (AllTypeVars m1) (AllTypeVars m2) = AllTypeVars $ m1 `Map.union` m2

-- | in the Apply has a type parameter, convert it to a type variable
--fixApply :: LetId -> Type -> Type
--fixApply (LetId id) (TParam p) = (TVar $ p ++ id)
--fixApply li (TFun t1 t2) = TFun (fixApply li t1) (fixApply li t2)
--fixApply _ t = t

--fixTp :: Maybe LetId -> Type -> Type
--fixTp (Just (LetId id)) (TParam p) = vtrace ("FIXTP: " ++ p) (TVar $ p ++ id)
--fixTp li@(Just (LetId id)) (TFun t1 t2) = TFun (fixTp li t1) (fixTp li t2)
--fixTp _ t = t

{- Convert a Type into an AllTypeVars -}
typeToAllVars :: Type -> Expression -> AllTypeVars
typeToAllVars (TVar name) exp = AllTypeVars $ Map.singleton name $ TVarInfo name exp Nothing
typeToAllVars (TFun t1 t2) exp = (typeToAllVars t1 exp) `mergeATV` (typeToAllVars t2 exp)
typeToAllVars _ _ = AllTypeVars Map.empty

-- | Given an expression, find all the type variables in the expression
collectVars :: Expression -> AllTypeVars
collectVars e@(LetExp _ _ t1 exp) = (typeToAllVars t1 e) `mergeATV` (collectVars exp)
collectVars e@(Apply letId t1 t2 e1 e2) = (typeToAllVars t1 e2) `mergeATV` 
                                    (typeToAllVars t2 e) `mergeATV` (collectVars e1)
                                    `mergeATV` (collectVars e2)
collectVars (Var _) = AllTypeVars Map.empty
collectVars e@(BuiltIn _ t1 _) = typeToAllVars t1 e
collectVars (Group map t1 exp) = Map.fold (mergeATV . collectVars) (AllTypeVars Map.empty) map
                                 `mergeATV` (typeToAllVars t1 exp) `mergeATV` (collectVars exp)

collectVars e@(SinkExp _ _ t1 expr) = (typeToAllVars t1 e) `mergeATV` (collectVars expr)
collectVars e@(SourceExp _ _ t1) = (typeToAllVars t1 e)
collectVars e@(FuncExp paramName pt rt exp) = (typeToAllVars pt e) `mergeATV`
                                                 (typeToAllVars rt e) `mergeATV`
                                                 (collectVars exp)
collectVars (ValueConst _) = AllTypeVars Map.empty

-- | An implementation based on http://dysphoria.net/2009/06/28/hindley-milner-type-inference-in-scala/
type Nongen = Set.Set Type

prune :: AllTypeVars -> Type -> ThrowsError (AllTypeVars, Type)
prune atv tv@(TVar name) =
    do
        tvi <- findType tv atv
        case tvi of
            tvar@(TVarInfo _ _ (Just t)) ->
              do
                  (atv', t') <- prune atv t
                  atv'' <- setATV tv (setTVIType tvar $ Just t') atv'
                  return (atv'', t')
            _ -> return (atv, tv)
prune atv (TFun t1 t2) = 
    do
      (atv', t1') <- prune atv t1
      (atv'', t2') <- prune atv' t2
      return (atv'', TFun t1' t2')
prune atv t = return $ (atv, t)

isgeneric nongen atv tv@(TVar _) =
    do
        (atv', oi) <- occursin (Set.elems nongen) atv tv
        return $ vtrace ("Isgeneric for "++show tv++ " is "++ show oi) (atv', not oi)
isgeneric nongen atv _ = return (atv, False)

occursin [] atv _ = return (atv, False)
occursin (x:xs) atv tv =
    do
        (atv', oi) <- occursintype atv tv x
        if oi then return (atv', True) else occursin xs atv' tv
        
occursintype :: AllTypeVars -> Type -> Type -> ThrowsError (AllTypeVars, Bool)
occursintype atv v t2 =
    do
        (atv', t2') <- prune atv t2
        case t2' of
            v2 | v2 == v -> return (atv', True)
            (TFun t1 t2) -> occursin [t1, t2] atv' v
            (TOper name params) -> occursin params atv' v
            _ -> return (atv', False)

newVariable (AllTypeVars atv) =
    let len = Map.size atv in
    let name = "synthetic"++show len in
    let nv = TVarInfo name (Var $ FuncName $ name ++ "$$##") Nothing in
    (AllTypeVars $ Map.insert name nv atv, TVar name)

fresh :: Nongen -> AllTypeVars -> Type -> ThrowsError (AllTypeVars, Type)
fresh nongen atv tpe = 
    do
        (atv', t, _) <- fresh' nongen atv tpe Map.empty
        return (atv', t)

fresh' :: Nongen -> AllTypeVars -> Type -> (Map.Map Type Type) -> ThrowsError (AllTypeVars, Type, Map.Map Type Type)
fresh' nongen atv t map =
    do
        (atv', t') <- prune atv t
        case t' of
            (TVar _) -> 
              do
                  (atv'', gen) <- isgeneric nongen atv' t'
                  let updateMap = case Map.lookup t' map of
                                      (Just t'') -> return (atv'', t'', map)
                                      _ -> let (atv''', ntv) = newVariable atv'' in
                                           let map' = Map.insert t' ntv map in
                                           return (atv''', ntv, map')
                  if gen then updateMap
                      else return (atv'', t', map)
            (TFun t1 t2) ->
              do
                  (atv'', t1', map') <- fresh' nongen atv' t1 map
                  (atv''', t2', map'') <- fresh' nongen atv'' t2 map'
                  return (atv''', TFun t1' t2', map'')
            (TOper name args) ->
              do
                  let foldMe (atvT, lst, map') t = do
                                                      (atvRet, tRet, mapRet) <- fresh' nongen atvT t map'
                                                      return (atvRet, lst ++ [tRet], mapRet)
                  (atv'', args', map') <- foldM foldMe (atv', [], map) args
                  return (atv'', TOper name args', map')
            _ -> return (atv', t', map)

gettype :: VarScope -> Nongen -> AllTypeVars -> FuncName -> ThrowsError (AllTypeVars, Type)
gettype scope nongen atv name =
    case Map.lookup name scope of
        (Just t) -> fresh nongen atv t
        _ -> throwError $ TypeError $ "Could not find " ++ show name ++ " in scope"
        
analyze :: VarScope -> Nongen -> AllTypeVars -> Expression -> ThrowsError (AllTypeVars, Type)
analyze scope nongen atv (Var funcName) = gettype scope nongen atv funcName
analyze scope nongen atv (ValueConst v) = return (atv, valuePrim v)
analyze scope nongen atv (BuiltIn _ t _) = return (atv, t)
analyze scope nongen atv (SourceExp _ _ t) = return (atv, t)
analyze scope nongen atv (LetExp _ name t1 exp) = 
    do
        let scope' = Map.insert name t1 scope
        (atv', rt) <- analyze scope' nongen atv exp
        atv'' <- unify atv' t1 rt
        return (atv'', t1)
analyze scope nongen atv (SinkExp _ name t1 exp) = 
    do
        let scope' = Map.insert name t1 scope
        (atv', rt) <- analyze scope' nongen atv exp
        atv'' <- unify atv' t1 rt
        return (atv'', t1)
analyze scope nongen atv (FuncExp paramName pt rt exp) =
    do
        let scope' = Map.insert paramName pt scope
        (atv', rt') <- analyze scope' (Set.insert pt nongen) atv exp
        atv'' <- unify atv' rt rt'
        return (atv'', TFun pt rt')
analyze scope nongen atv (Apply letId t1 t2 exp1 exp2) =
    do
        (atv', funType) <- vtrace ("Appy " ++ show letId) analyze scope nongen atv exp1
        (atv'', argType) <- analyze scope nongen atv' exp2
        atv''' <- unify atv'' (TFun argType t2) funType
        atv'''' <- unify atv''' t1 funType
        return (atv'''', t2)
analyze scope nongen atv (Group exprs t1 exp) =
    let foldMe name (LetExp _ _ t1 _) map = Map.insert name t1 map
        foldMe name (SinkExp _ _ t1 expr) map = Map.insert name t1 map
        foldMe name (SourceExp _ _ t1) map = Map.insert name t1 map
        foldMe name (BuiltIn _ t1 _) map = Map.insert name t1 map in
    let scope' = Map.foldrWithKey foldMe scope exprs in
    let doAnExp curAtv exp = 
                        do
                            atv' <- curAtv
                            (ret, _) <- analyze scope' nongen atv' exp
                            return ret
                        in
    let atv' = List.foldl' doAnExp (return atv) $ Map.elems exprs in
    do
        innerAtv <- atv'
        (atv'', tret) <- analyze scope' nongen innerAtv exp
        return (atv'', t1)

unify :: AllTypeVars -> Type -> Type -> ThrowsError AllTypeVars
unify atv t1 t2 =
    do
        (atv', t1') <- vtrace ("Unifying " ++ show t1 ++ " and " ++ show t2) prune atv t1
        (atv'', t2') <- prune atv' t2
        case (t1', t2') of 
            ((TVar a), (TVar b)) | a == b -> return $ vtrace ("Same old " ++ a) atv''
            (a@(TVar _), b) | a /= b ->
              do
                  (atv3, oit) <- vtrace ("testing oit " ++ show a ++ " and " ++ show b) occursintype atv'' a b 
                  if oit then throwError $ TypeError $ "Recursive Unification of " ++ show a
                      else do
                             tvi <- findType a atv3
                             setATV a (setTVIType tvi $ Just b) atv3
            (o, v@(TVar _)) -> unify atv'' v o
            ((TFun p1 r1), (TFun p2 r2)) ->
              do
                  atv''' <- vtrace ("Mr yak 1 "++ show t1' ++ " and " ++ show t2') unify atv'' p1 p2
                  atv'''' <- vtrace ("Mr yak 2 "++ show t1' ++ " and " ++ show t2') unify atv''' r1 r2
                  return atv''''
            (a@(TOper n1 p1), b@(TOper n2 p2)) ->
              if n1 /= n2 || (length p1) /= (length p2) then throwError $ TypeError $ "Type mismatch " ++ show a ++ " /= " ++ show b
                  else List.foldl' unifyPair (return atv'') $ zip p1 p2
            _ -> return atv''

unifyPair eAtv (t1, t2) =
    do
        atv <- eAtv
        unify atv t1 t2


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
collectSubs scope atv (Var funcName) =
    do
        varType <- justOr (Map.lookup funcName scope) (throwError $ TypeError $ "Can't find " ++ show funcName ++ " in current scope")
        -- atv' <- mustBe ("other var "++show funcName) t1 varType atv
        return (atv, varType)
 --     mustBe ("Var " ++ show funcName) t1 varType atv, varType) -- FIXME -- is this right?
collectSubs scope atv (FuncExp paramName pt rt exp) =
    let scope' = Map.insert paramName pt scope in
    do
        (atv', rt') <- collectSubs scope' atv exp
        atv'' <- mustBe ("Func " ++ show paramName) rt rt' atv'
        return (atv'', TFun pt rt')
collectSubs scope atv (Apply letId t1 t2 exp1 exp2) =
    do
        (satv, st) <- collectSubs scope atv exp1
        (funParam, funRet) <- (resolvedOr st (\v -> v |- (\t -> findType t atv) |- tvarMust)) |- expectFuncType
        (atv'', paramType) <- collectSubs scope satv exp2
        atv''' <- mustBe "func param" t1 funParam atv'' >>=
                  mustBe "Pushing the param type back [fixed]" paramType t1 >>=
                  mustBe "Return type" t2 funRet
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

expectFuncType (TFun a b) = Right $ (a,b)
expectFuncType t = throwError $ TypeError $ "Looking for a Function, but got type " ++ show t

asTVar (TVar tv) = Right tv
asTVar t = throwError $ TypeError $ "Looking for a Type Variable, but got " ++ show t

tvarMust (TVarInfo _ _ (Just t)) = Right t
tvarMust (TVarInfo name _ found) = throwError $ TypeError $ "Trying to resolve the type for " ++ name ++ " but only got " ++ show found