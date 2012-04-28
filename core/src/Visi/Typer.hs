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
import Data.List ( foldl' ) 
import qualified Data.Text as T
import Visi.Util
import Visi.Expression 
import Control.Monad.Error
import Control.Monad.State

-- | An implementation based on http://dysphoria.net/2009/06/28/hindley-milner-type-inference-in-scala/
type Nongen = Set.Set TypeAlias

type TypeVars = Map.Map TypeAlias TVarInfo
type TypeMap = Map.Map TypeAlias TypeAlias
type MethodMap = Map.Map Type (Map.Map T.Text TypeAlias)
type StateThrow = StateT StateInfo ThrowsError

data AllTypeVars = AllTypeVars (Map.Map T.Text TVarInfo) deriving (Show)

data TVarInfo = TVarInfo T.Text Expression (Maybe Type) deriving (Show)

newtype TypeAlias = TypeAlias Int deriving (Show, Eq, Ord)

newtype AliasPair = AliasPair (TypeAlias, Type) deriving (Show, Eq)

type MagicMapping = Map.Map TypeAlias [AliasPair]

data StateInfo = StateInfo {si_cnt :: Int, si_mapping :: MagicMapping ,
                            si_typeVars :: TypeVars, si_typeMap :: TypeMap, si_expression:: Expression, si_methodMap :: MethodMap}

collectTypes :: Expression -> ThrowsError [(T.Text, Type)]
collectTypes exp = 
    do
        (a, s) <- runStateT (processTypes exp) StateInfo{ si_typeVars = Map.empty,
                                                          si_typeMap = Map.empty, 
                                                          si_cnt = 1,
                                                          si_mapping = Map.empty,
                                                          si_expression = Var NoSourceLoc $ FuncName $ T.pack "Starting",
                                                          si_methodMap = Map.empty {- FIXME build method map Map.fromList [((TPrim PrimDouble), 
                                                                          (Map.fromList [((T.pack "fizzbin"), (TPrim PrimDouble))
                                                                          ,((T.pack "meowfizz"), (TPrim PrimDouble))]))
                                                                          ,((TPrim PrimStr), (Map.singleton (T.pack "fizzbin") (TPrim PrimStr)))] -}}
        return a


aliasForType :: Type -> StateThrow TypeAlias
aliasForType tpe = 
  do
    st <- get
    let c1 = 1 + (si_cnt st)
    let magic = si_mapping st
    let ta = TypeAlias c1
    put st{si_cnt = c1, si_mapping = Map.insert ta [AliasPair (ta, tpe)] magic}
    return ta

getAlias :: TypeAlias -> StateThrow AliasPair
getAlias ta =
  do
    theMap <- fmap si_mapping get
    case Map.lookup ta theMap of
      Just (ret:_) -> return $ ret
      _ -> throwError $ TypeError $ "Cannot find type alias: " ++ show ta

getAliasInfo :: TypeAlias -> StateThrow [AliasPair]
getAliasInfo ta =
  do
    theMap <- fmap si_mapping get
    case Map.lookup ta theMap of
      Just ret -> return $ ret
      _ -> throwError $ TypeError $ "Cannot find type alias: " ++ show ta

reviseAlias :: TypeAlias -> Type -> StateThrow [AliasPair]
reviseAlias ta tpe = 
  do
    st <- get
    let theMap = si_mapping st
    let lst = AliasPair (ta, tpe) : maybe [] id (Map.lookup ta theMap)
    put st{si_mapping = Map.insert ta lst theMap}
    return lst

getATV :: StateThrow TypeVars
getATV = fmap si_typeVars get

putATV :: TypeVars -> StateThrow ()      
putATV atv = get >>= (\s -> put s{si_typeVars = atv})
        
getTypeMap = fmap si_typeMap get

putTypeMap tm = get >>= (\s -> put s{si_typeMap = tm})
        
getCurExp = fmap si_expression get
        
putCurExp e = get >>= (\s -> put s{si_expression = e})

getMethMap = fmap si_methodMap get

putMethMap mm = get >>= (\s -> put s{si_methodMap = mm})


findTVI :: TypeAlias -> StateThrow TVarInfo
findTVI t1  = 
    do
        map <- getATV
        case Map.lookup t1 map of
            Just v -> return v
            _ -> throwError $ TypeError $ "Cannot find type var: " ++ show t1
-- findTVI tv = throwError $ TypeError $ "Trying to look up " ++ show tv ++ " but it's not a Type Variable"

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
            (StructuralType theMap) ->
                do
                  revised <- mapM (\(k, v) -> do {v' <- fresh' nongen v; return (k,v')}) $ Map.toList theMap
                  return $ StructuralType $ Map.fromList revised  
            _ -> return t'

syntheticString = "synthetic"

synthetic = T.pack syntheticString

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

hasMethods clz (StructuralType theMap) =
    do
        methMap <- vtrace ("hasMethods for " ++ show clz ++ " and " ++ show theMap) getMethMap
        case (Map.lookup clz methMap) of
            Just meths -> 
                do
                    let methPairs = Map.toList theMap
                    let testAndUnify (name, tpe) = do
                            case (Map.lookup name meths) of
                                Just otype -> 
                                    do 
                                        tpe' <- vtrace ("Pruning " ++ show tpe ++ " and " ++ show otype ++ " for clz " ++ show clz) prune tpe
                                        otype' <- prune otype
                                        unify tpe' otype'
                                _ -> throwError $ TypeError $ "Failed to find method " ++ T.unpack name ++ " on " ++ show clz
                    mapM_ testAndUnify methPairs
            _ -> throwError $ TypeError $ "No known methods for " ++ show clz ++ " theMap " ++ show theMap


testSameGen (TVar _) (TVar _) = True
testSameGen a b = testSame a b

testSame mt1@(StructuralType mm1) mt2@(StructuralType mm2) = 
    let testit bool (k, tpe) = bool && (testSameGen (mm1 Map.! k) tpe) in
    (Map.size mm1 == Map.size mm2) && (Map.keysSet mm1 == Map.keysSet mm2) && (foldl' testit True $ Map.toList mm2)
testSame mt1 mt2 = mt1 == mt2

unify t1 t2 =
    do
        methMap <- getMethMap
        putMethMap methMap
        t1' <- prune t1
        t2' <- prune t2
        case (t1', t2') of
            -- (a,b) | vtrace ("Unifying " ++ show a ++ " & " ++ show b) False -> error "Yikes"
            (a, b) | a `testSame` b -> return ()
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
            (a, v@(StructuralType theMap)) -> 
                do
                  hasMethods a v
                  return ()
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
prune (StructuralType theMap) = 
    do
        revised <- mapM (\(k, v) -> do {v' <- prune v; return (k,v')}) $ Map.toList theMap
        return $ StructuralType $ Map.fromList revised
prune t = return t


isSynthetic t = 
    T.isPrefixOf synthetic t

isGeneric nongen tv@(TVar v) | isSynthetic v = return False
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
calcType scope nongen (Var _ funcName) = gettype scope nongen funcName
calcType scope nongen (ValueConst _ v) = case valuePrim v of
    Just tpe -> return tpe
    _ -> throwError $ TypeError $ "Expection primative value but got value: " ++ show v
calcType scope nongen (BuiltIn _ _ t _) = prune t
calcType scope nongen (SourceExp _ _ _ t) = prune t
calcType scope nongen e@(LetExp _ _ name canBeGen t1 exp) = 
    do
        putCurExp e
        t1' <- createTypeVar e t1
        let scope' = Map.insert name t1' scope
        rt <- calcType scope' (if canBeGen then nongen else Set.insert t1' nongen) exp
        unify t1' rt
        prune t1'
calcType scope nongen e@(SinkExp _ _ name t1 exp) = 
    do
        putCurExp e
        t1' <- createTypeVar e t1
        let scope' = Map.insert name t1' scope
        rt <- calcType scope' (Set.insert t1' nongen) exp
        unify t1' rt
        prune t1'

calcType scope nongen e@(FuncExp _ paramName pt exp) =
    do
        putCurExp e
        pt' <- createTypeVar e pt
        let scope' = Map.insert paramName pt' scope
        rt <- calcType scope' (Set.insert pt' nongen) exp
        pt'' <- prune pt'
        return $ tFun pt'' rt
calcType scope nongen e@(InvokeMethod _ letId method@(FuncName methodText) (TOper oprName [source, dest])) =
    do
        putCurExp e
        t1' <- createTypeVar e dest
        source' <- prune source
        let ret = TOper oprName [source', t1']
        return ret
calcType scope nongen e@(Apply _ letId t1 exp1 exp2) =
    do
        putCurExp e
        t1' <- createTypeVar e t1
        funType <- calcType scope nongen exp1
        argType <- showStructType funType $ calcType scope nongen exp2
        unify (tFun argType t1') funType
        ret <- prune t1'
        funType' <- prune funType
        unify (tFun argType ret) funType'
        let (TOper _ [_, ret']) = funType'
        return ret'
calcType scope nongen (InnerLet _ t letExp actualExp) = 
    do
        let (LetExp _ _ name canBeGen t1 exp) = letExp
        t1' <- createTypeVar exp t1
        t' <- createTypeVar actualExp t
        let scope' = Map.insert name t1' scope
        let nongen' = Set.insert t1' nongen
        calcType scope' nongen' letExp
        ret <- calcType scope' nongen' actualExp
        prune ret
calcType scope nongen (Group _ exprs _ exp) =
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

-- showStructType (TOper _ [st@(StructuralType _), other]) rest = vtrace ("Struct type " ++ show st ++ " other " ++ show other) rest
showStructType _ rest = rest

genPull (_, t, True) = [t]
genPull _ = []

isNongen (LetExp _ _ _ _ _ (SourceExp _ _ _ t1)) = True
isNongen e = False

nv (name, value, _) = (name, value)


-- | Take an Expression which should be a Group and turn it into a scope of top level expressions
buildLetScope :: Expression -> LetScope
buildLetScope (Group _ exprs _ _) = exprs
buildLetScope _ = Map.empty

processExp it (name, e@(LetExp _ _ _ _ t1 _)) = it name e t1
processExp it (name, e@(SinkExp _ _ _ t1 _)) = it name e t1
processExp it (name, e@(SourceExp _ _ _ t1)) = it name e t1
processExp it (name, e@(BuiltIn _ _ t1 _)) = it name e t1

-- we have to repeated run the typer to get all the synthetic variables resolved
processTypes' :: Expression -> [(T.Text, Type)] -> StateThrow [(T.Text, Type)]
processTypes' e@(Group _ exprs _ _) org =
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
                                         && foldl' testPairTypeEq True (zip r1 r2)
  
testTypeEq t1 t2 = t1 `testSame` t2

testPairTypeEq b (t1, t2) = b && testTypeEq t1 t2


processTypes :: Expression -> StateThrow [(T.Text, Type)]
processTypes e = processTypes' e []

