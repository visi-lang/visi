module Visi.Typer (collectTypes, buildLetScope) where

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
import Visi.Expression 
import Control.Monad.Error
import Control.Applicative
import Control.Monad.State

-- | An implementation based on http://dysphoria.net/2009/06/28/hindley-milner-type-inference-in-scala/
type Nongen = Set.Set Type

type TypeVars = Map.Map T.Text TVarInfo
type TypeMap = Map.Map Type Type
type StateThrow = StateT (TypeVars, TypeMap ,Expression) ThrowsError

collectTypes :: Expression -> ThrowsError [(T.Text, Type)]
collectTypes exp = 
    do
        (a, s) <- runStateT (processTypes exp) (Map.empty, Map.empty, Var $ FuncName $ T.pack "Starting")
        return a
        
getATV :: StateThrow TypeVars
getATV =
    do
        (atv, _, _) <- get
        return atv
 
putATV :: TypeVars -> StateThrow ()      
putATV atv =
    do
        (_, m, e) <- get
        put (atv, m, e)
        
getTypeMap =
    do
        (_, tm, _) <- get
        return tm

putTypeMap tm =
    do
        (atv, _, e) <- get
        put (atv, tm, e)
        
getCurExp =
    do
        (_, _, e) <- get
        return e
        
putCurExp e =
    do
        (atv, m, _) <- get
        put (atv, m, e)

findTVI :: Type -> StateThrow TVarInfo
findTVI (TVar t1)  = 
    do
        map <- getATV
        case Map.lookup t1 map of
            (Just v) -> return v
            _ -> throwError $ TypeError $ "Cannot find type var: " ++ T.unpack t1
findTVI tv = throwError $ TypeError $ "Trying to look up " ++ show tv ++ " but it's not a Type Variable"

setTVIType (TVarInfo name exp _) newType = TVarInfo name exp newType

updateType tv nt =
    do
        cur <- findTVI tv
        let updated = setTVIType cur $ Just nt
        setATV tv updated

setATV (TVar t1) newTV = 
    do
        map <- getATV
        putATV $ Map.insert t1 newTV map
setATV theType _ = throwError $ TypeError $ "Trying to update a type variable, but got " ++ show theType

instnce (TVarInfo _ _ i) = i

-- gettype _ nongen (FuncName name) | vtrace ("Gettyoe for " ++ name ++ " Nongen " ++ show nongen) False = error "Never"
gettype scope nongen name =
    case Map.lookup name scope of
        Just t -> do 
          t' <- prune t
          ret <- fresh nongen t'
          -- return $ vtrace ("Returning " ++ show ret) ret
          return ret
        _ -> throwError $ TypeError $ "Could not find " ++ show name ++ " in scope"

fresh nongen tpe = 
    do
        putTypeMap Map.empty
        t <- fresh' nongen tpe
        putTypeMap Map.empty -- clear out the type map
        return t

fresh' nongen t =
    do
        t' <- prune t
        case t' of
            (TVar _) -> 
              do
                  gen <- isGeneric nongen t'
                  map <- getTypeMap
                  let updateMap = case Map.lookup t' map of
                                      Just t'' -> return t''
                                      _ ->
                                        do
                                            ntv <- newVariable
                                            let map' = Map.insert t' ntv map
                                            putTypeMap map'
                                            return ntv
                  if gen then updateMap else return t'
            (TOper name args) ->
              do
                  args' <- mapM (fresh' nongen) args
                  return $ TOper name args'
            _ -> return t'

synthetic = T.pack "synthetic"

synthLen = T.length synthetic

newVariable =
    do
      atv <- getATV
      curExp <- getCurExp
      let len = Map.size atv
      let name = T.pack $ T.unpack synthetic ++ show len
      let nv = TVarInfo name curExp Nothing
      let t = TVar name
      setATV t nv
      return t

unify t1 t2 =
    do
        t1' <- prune t1
        t2' <- prune t2
        case (t1', t2') of
            -- (a,b) | vtrace ("Unifying " ++ show a ++ " & " ++ show b) False -> error "Yikes"
            (TVar a, TVar b) | a == b -> return ()
            (a@(TVar _), b) ->
                do
                    oit <- occursInType a b
                    if oit 
                      then throwError $ TypeError $  "Recursive Unification of " ++ show a ++ " and " ++ show b
                      else do tvi <- findTVI a
                              updateType a b
            (o, v@(TVar _)) -> unify v o
            (a@(TOper n1 p1),b@(TOper n2 p2)) ->
                if n1 /= n2 || length p1 /= length p2 
                then throwError $ TypeError $ "Type mismatch " ++ show a ++ " /= " ++ show b
                else mapM_ (uncurry unify) $ zip p1 p2
            (a, b) | a == b -> return ()
            (a, b) -> throwError $ TypeError $ "Failed to unify " ++ show a ++ " & " ++ show b

prune tv@(TVar _) =
    do
        atv <- findTVI tv
        case instnce atv of
            Just t ->
              do
                  t' <- prune t
                  updateType tv t'
                  return t'
            _ -> return tv
prune (TOper name params) =
    do
        params' <- mapM prune params
        return $ TOper name params'
prune t = return t

isGeneric nongen tv@(TVar _) = 
    do
        oi <- occursIn (Set.elems nongen) tv
        return $ not oi
isGeneric _ _ = return False

occursIn [] _ = return False
occursIn (x:xs) tv =
    do
        oi <- occursInType tv x
        if oi then return True else occursIn xs tv
        
occursInType t1 t2 =
    do
        t2' <- prune t2
        case t2' of
            v | v == t1 -> return True
            TOper _ args -> occursIn args t1
            _ -> return False

createTypeVar e tv@(TVar t1)  = 
    do
        map <- getATV
        case Map.lookup t1 map of
            (Just v) -> prune tv
            _ -> do
                   setATV tv $ TVarInfo t1 e Nothing
                   prune tv
createTypeVar e (TOper n p) =
    do
        p' <- mapM (createTypeVar e) p
        prune $ TOper n p'
createTypeVar _ t = prune t
        
calcType :: VarScope -> Nongen -> Expression -> StateThrow Type
calcType scope nongen (Var funcName) = gettype scope nongen funcName
calcType scope nongen (ValueConst v) = return $ valuePrim v
calcType scope nongen (BuiltIn _ t _) = prune t
calcType scope nongen (SourceExp _ _ t) = prune t
calcType scope nongen e@(LetExp _ name canBeGen t1 exp) = 
    do
        putCurExp e
        t1' <- createTypeVar e t1
        let scope' = Map.insert name t1' scope
        rt <- calcType scope' (if canBeGen then nongen else Set.insert t1' nongen) exp
        unify t1' rt
        prune t1'
calcType scope nongen e@(SinkExp _ name t1 exp) = 
    do
        putCurExp e
        t1' <- createTypeVar e t1
        let scope' = Map.insert name t1' scope
        rt <- calcType scope' (Set.insert t1' nongen) exp
        unify t1' rt
        prune t1'

calcType scope nongen e@(FuncExp paramName pt exp) =
    do
        putCurExp e
        pt' <- createTypeVar e pt
        let scope' = Map.insert paramName pt' scope
        rt <- calcType scope' (Set.insert pt' nongen) exp
        pt'' <- prune pt'
        return $ tFun pt'' rt
calcType scope nongen e@(Apply letId t1 exp1 exp2) =
    do
        putCurExp e
        t1' <- createTypeVar e t1
        funType <- calcType scope nongen exp1
        argType <- calcType scope nongen exp2
        unify (tFun argType t1') funType
        prune t1'
calcType scope nongen (InnerLet t letExp actualExp) = 
    do
        let (LetExp _ name canBeGen t1 exp) = letExp
        t1' <- createTypeVar exp t1
        t' <- createTypeVar actualExp t
        let scope' = Map.insert name t1' scope
        calcType scope' nongen letExp
        ret <- calcType scope' nongen actualExp
        prune ret
calcType scope nongen (Group exprs _ exp) =
    do
        let it name e t1 = do
                                t1' <- createTypeVar e t1
                                return (name, t1', isNongen e)        
        pairs <- mapM (processExp it) $ Map.assocs exprs
        let scope' = scope `Map.union` Map.fromList (map nv pairs)
        -- all the source expressions are *NOT* generic. We need to
        -- know what the actual types are
        let nongen' = nongen `Set.union` Set.fromList (pairs >>= genPull)
        mapM_ (calcType scope' nongen') $ Map.elems exprs
        return $ (TOper $ T.pack "na") []

genPull (_, t, True) = [t]
genPull _ = []

isNongen (LetExp _ _ _ _ (SourceExp _ _ t1)) = True
isNongen e = False

nv (name, value, _) = (name, value)


-- | Take an Expression which should be a Group and turn it into a scope of top level expressions
buildLetScope :: Expression -> LetScope
buildLetScope (Group exprs _ _) = exprs
buildLetScope _ = Map.empty

processExp it (name, e@(LetExp _ _ _ t1 _)) = it name e t1
processExp it (name, e@(SinkExp _ _ t1 _)) = it name e t1
processExp it (name, e@(SourceExp _ _ t1)) = it name e t1
processExp it (name, e@(BuiltIn _ t1 _)) = it name e t1

-- we have to repeated run the typer to get all the synthetic variables resolved
processTypes' :: Expression -> [(T.Text, Type)] -> StateThrow [(T.Text, Type)]
processTypes' e@(Group exprs _ _) org =
    do
        calcType Map.empty Set.empty e
        let nameType (FuncName name) e t = do
                                    t' <- prune t
                                    return (name, t')            
        ret <- mapM (processExp nameType) $ Map.assocs exprs
        if testEq org ret then return ret else processTypes' e ret

testEq [] [] = True
testEq [] _ = False
testEq _ [] = False
testEq ((n1, t1):r1) ((n2, t2):r2) = n1 == n2 
                                     && testTypeEq t1 t2 
                                     && testEq r1 r2

testTypeEq (TVar n1) (TVar n2) = n1 == n2 
                                 || (T.take synthLen n1 == synthetic && T.take synthLen n2 == synthetic)
testTypeEq (TOper n1 r1) (TOper n2 r2) = (n1 == n2 && length r1 == length r2) 
                                         && List.foldl' testPairTypeEq True (zip r1 r2)
  
testTypeEq t1 t2 = t1 == t2

testPairTypeEq b (t1, t2) = b && testTypeEq t1 t2


processTypes :: Expression -> StateThrow [(T.Text, Type)]
processTypes e = processTypes' e []

