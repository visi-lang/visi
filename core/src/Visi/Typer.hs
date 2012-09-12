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
type Nongen = Set.Set TypePtr


-- type TypeVars = Map.Map TypePtr TVarInfo
type FreshMap = Map.Map TypePtr TypePtr
type MethodMap = Map.Map TypePtr (Map.Map T.Text TypePtr)
type TypeLookup = Map.Map Type TypePtr
type StateThrow = StateT StateInfo ThrowsError

-- data AllTypeVars = AllTypeVars (Map.Map T.Text TVarInfo) deriving (Show)

-- data TVarInfo = TVarInfo TypePtr Expression (Maybe Type) deriving (Show)

newtype TypePtr = TypePtr Int deriving (Show, Eq, Ord)

data TypeInfo = TypeInfo TypePtr Type (Maybe Expression) 
                | TypeAlias TypePtr TypePtr deriving (Show, Eq)


type MagicMapping = Map.Map TypePtr [TypeInfo]

data StateInfo = StateInfo {si_cnt :: Int, si_mapping :: MagicMapping ,
                            si_lookup :: TypeLookup,
                            {-si_typeVars :: TypeVars, -} si_freshMap :: FreshMap,
                            si_expression:: Expression, si_methodMap :: MethodMap}

collectTypes :: Expression -> ThrowsError [(T.Text, Type)]
collectTypes exp =
    do
        (a, s) <- runStateT (buildMethodMap [((TPrim PrimDouble),
                                             (Map.fromList [((T.pack "fizzbin"), (TPrim PrimDouble))
                                             ,((T.pack "meowfizz"), (TPrim PrimDouble))]))
                                             ,((TPrim PrimStr), 
                                                (Map.singleton (T.pack "fizzbin") (TPrim PrimStr)))] >> processTypes exp 
                                                  >>= extractTypes) stateStartingPoint
        return a

stateStartingPoint = StateInfo{si_freshMap = Map.empty,
                               si_cnt = 1,
                               si_lookup = Map.empty,
                               si_mapping = Map.empty,
                               si_expression = Var NoSourceLoc $ FuncName $ T.pack "Starting",
                               si_methodMap = Map.empty}

buildMethodMap :: [(Type, (Map.Map T.Text Type))] -> StateThrow ()
buildMethodMap info = 
    do
        let tt (txt, t1) = aliasForType t1 Nothing >>= (\t1' -> return (txt, t1'))
        let mt (t1, m) = do
                            t1' <- aliasForType t1 Nothing
                            lst <- mapM tt $ Map.toList m
                            return (t1', Map.fromList lst)
        lst <- mapM mt info
        st <- get
        put st{si_methodMap = Map.fromList lst}


extractTypes :: [(T.Text, TypePtr)] -> StateThrow [(T.Text, Type)]
extractTypes ptr = mapM cvt ptr
    where cvt (txt, tp) = prunedToType tp >>= (\t' -> return (txt, t'))

shortAlias :: Type -> StateThrow TypePtr
shortAlias t = aliasForType t Nothing

aliasForType :: Type -> Maybe Expression -> StateThrow TypePtr
aliasForType tpe mexp =
  do
    st <- get
    let lu = si_lookup st
    case Map.lookup tpe lu of
        Just tp -> return tp
        _ -> do
            let c1 =  (1 + (si_cnt st))
            let magic = (si_mapping st) --  vtrace ("Creating new type for " ++ show tpe ++ " number " ++ show c1)
            let ta = TypePtr c1
            put st{si_cnt = c1, si_mapping = Map.insert ta [TypeInfo ta tpe mexp] magic,
                    si_lookup = Map.insert tpe ta lu}
            return ta


getFreshMap = fmap si_freshMap get

putFreshMap tm = get >>= (\s -> put s{si_freshMap = tm})

getCurExp :: StateThrow Expression
getCurExp = fmap si_expression get

putCurExp e = get >>= (\s -> put s{si_expression = e})

getMethMap :: StateThrow MethodMap
getMethMap = fmap si_methodMap get

putMethMap mm = get >>= (\s -> put s{si_methodMap = mm})

gettype :: VarScope -> Nongen -> FuncName -> StateThrow TypePtr
gettype scope nongen name =
    case Map.lookup name scope of
        Just t -> do
          tp <- aliasForType t Nothing
          ret <- fresh nongen tp
          return ret
        _ -> throwError $ TypeError $ "Could not find " ++ show name ++ " in scope"

fresh :: Nongen -> TypePtr -> StateThrow TypePtr
fresh nongen tpe =
    do
        putFreshMap Map.empty
        t <- fresh' nongen tpe
        putFreshMap Map.empty -- clear out the type map
        return t

newVariable :: StateThrow TypePtr
newVariable =
    do
        cnt <- fmap si_cnt get
        cur <- getCurExp
        let name = T.pack $ T.unpack synthetic ++ show cnt
        let t = TVar name
        alias <- aliasForType t $ Just cur
        return alias


prunedToType :: TypePtr -> StateThrow Type
prunedToType tp =
  do
    (tp', t') <- prunedType tp
    return t'


fresh' :: Nongen -> TypePtr -> StateThrow TypePtr
fresh' nongen t =
    do
        (tp', t') <- prunedType t
        case t' of
            (TVar _) ->
              do
                  gen <- isGeneric nongen tp'
                  map <- getFreshMap
                  let updateMap = case Map.lookup tp' map of
                                      Just tp'' -> return tp''
                                      _ ->
                                        do
                                            ntv <- newVariable
                                            putFreshMap $ Map.insert tp' ntv map
                                            return ntv
                  if gen then updateMap else return tp'
            (TOper name args) ->
              do
                  argsp <- mapM (\s -> aliasForType s Nothing) args
                  args' <- mapM (fresh' nongen) argsp
                  newArgs <- mapM prunedToType args'
                  aliasForType (TOper name newArgs) Nothing
            (StructuralType theMap) -> do
                  let sa (k,v) = do
                                    v' <- shortAlias v
                                    return (k, v')
                  l2 <- mapM sa (Map.toList theMap)
                  revised <- mapM (\(k, v) -> do {v' <- fresh' nongen v; v'' <- prunedToType v'; return (k,v'')}) l2
                  shortAlias $ StructuralType $ Map.fromList revised
            _ -> return tp'

syntheticString = "synthetic"

synthetic = T.pack syntheticString

synthLen = T.length synthetic


hasMethods :: Type -> Type -> StateThrow ()
hasMethods clz (StructuralType theMap) = 
    do
        methMap <- getMethMap
        clz' <- shortAlias clz
        case (Map.lookup clz' methMap) of
            Just meths ->
                do
                    let methPairs = (Map.toList theMap) :: [(T.Text, Type)]

                    let testAndUnify (name, tpe) = do
                            case (Map.lookup name meths) of
                                Just otype -> do
                                                tpe' <- shortAlias tpe
                                                unify tpe' otype                                        
                                _ -> throwError $ TypeError $ "Failed to find method " ++ T.unpack name ++ " on " ++ show clz
                    mapM_ testAndUnify methPairs
            _ -> throwError $ TypeError $ "No known methods for '" ++ show clz ++ 
                                          "' short '" ++ show clz' ++ "' theMap '" ++ show theMap ++ "'"

testSameGen (TVar _) (TVar _) = True
testSameGen a b = testSame a b

testSame mt1@(StructuralType mm1) mt2@(StructuralType mm2) =
    let testit bool (k, tpe) = bool && (testSameGen (mm1 Map.! k) tpe) in
    (Map.size mm1 == Map.size mm2) && (Map.keysSet mm1 == Map.keysSet mm2) && (foldl' testit True $ Map.toList mm2)
testSame mt1 mt2 = mt1 == mt2


updateType :: TypePtr -> TypePtr -> StateThrow ()
updateType a b | a == b = return ()
updateType source becomes =
    do
        st <- get
        let lu = si_mapping st
        let cur = lu Map.! source
        put st{si_mapping = Map.insert source ((TypeAlias source becomes):cur) lu}

unify :: TypePtr -> TypePtr -> StateThrow ()
unify a b | a == b = return ()
unify tp1 tp2 =
    do
        (tp1', t1') <- prunedType tp1
        (tp2', t2') <- prunedType tp2
        case (t1', t2') of
            (a, b) | a `testSame` b -> return ()
            (a@(TVar _), b) ->
                do
                    oit <- occursInType tp1 tp2'
                    if oit
                      then throwError $ TypeError $  "Recursive Unification of " ++ show a ++ " and " ++ show b
                      else updateType tp1 tp2'
            (o, v@(TVar _)) -> unify tp2' tp1'
            (a@(TOper n1 p1),b@(TOper n2 p2)) ->
                if n1 /= n2 || length p1 /= length p2
                then throwError $ TypeError $ "Type mismatch " ++ show a ++ " /= " ++ show b
                else do
                        p1' <- mapM shortAlias p1
                        p2' <- mapM shortAlias p2
                        mapM_ (uncurry unify) $ zip p1' p2'
                        newTypes <- mapM prunedToType p1'
                        let newOpr = TOper n2 newTypes
                        ta <- shortAlias newOpr
                        updateType tp1 ta
            (StructuralType a, StructuralType b) ->
              do
                unifiedMap <- unifyStructureMaps a b
                newTypePtr <- shortAlias $ StructuralType unifiedMap
                updateType tp1 newTypePtr

            (a, v@(StructuralType theMap)) -> hasMethods a v

            (a, b) ->
              do
                 dumpTypes
                 throwError $ TypeError $ "Failed to unify " ++ show a ++ " & " ++ show b ++
                                                   " tp1 " ++ show tp1 ++ " tp2 " ++ show tp2 ++
                                                   " tp1' " ++ show tp1' ++ " tp2' " ++ show tp2'

dumpTypes :: StateThrow ()
dumpTypes = 
    do
        return ()

unifyStructureMaps :: Map.Map T.Text Type -> Map.Map T.Text Type -> StateThrow (Map.Map T.Text Type)
unifyStructureMaps m1 m2 =
    let lst = Map.toList m1 in
    foldM doItem m2 lst

doItem :: Map.Map T.Text Type -> (T.Text, Type) -> StateThrow (Map.Map T.Text Type)
doItem theMap (name, tpe) =
    case Map.lookup name theMap of
        Just t2 -> 
            do
                ta1 <- shortAlias tpe
                ta2 <- shortAlias t2
                unify ta1 ta2
                return theMap
        _ -> return $ Map.insert name tpe theMap

prunedType :: TypePtr -> StateThrow (TypePtr, Type)
prunedType tp = 
    do
        mapping <- fmap si_mapping get
        case Map.lookup tp mapping of
            Nothing -> throwError $ TypeError $ "Unable to find type ptr " ++ show tp
            Just ((TypeInfo _ ret _):_) -> refineType tp ret
            Just (all@((TypeAlias _ ret):rest)) | ret == tp -> error $ "Type loop " ++ show all
            Just ((TypeAlias _ ret):_) -> prunedType ret

prune :: Type -> StateThrow Type
prune t = shortAlias t >>= prunedToType

refineType :: TypePtr -> Type -> StateThrow (TypePtr, Type)
refineType ptr tpe@(TOper name tpes) = 
    do
        tpes' <- mapM prune tpes
        if tpes == tpes' then return (ptr, tpe)
            else do
                    let nopr = TOper name tpes'
                    ta <- shortAlias nopr
                    updateType ptr ta
                    return (ta, nopr)
refineType tpr tpe = return (tpr, tpe)

isSynthetic t =
    T.isPrefixOf synthetic t

isGeneric ::  Nongen -> TypePtr -> StateThrow Bool
isGeneric nongen t =
    do
        t' <- prunedToType t
        case t' of
            tv@(TVar v) | isSynthetic v -> return False
            tv@(TVar _) ->
                do
                    tv' <- aliasForType tv Nothing
                    oi <- occursIn (Set.elems nongen) tv'
                    return $ not oi
            _ -> return False


occursIn :: [TypePtr] -> TypePtr -> StateThrow Bool
occursIn [] _ = return False
occursIn (x:xs) tv =
    do
        oi <- occursInType tv x
        if oi then return True else occursIn xs tv

occursInType :: TypePtr -> TypePtr -> StateThrow Bool
occursInType t1 t2 =
    do
        (tp2', t2') <- prunedType t2
        (tp1', t1') <- prunedType t1
        case t2' of
            v | v == t1' -> return True
            TOper _ args -> mapM shortAlias args >>= (\ta -> occursIn ta t1)
            _ -> return False
{-
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
-}


calcType :: VarScope -> Nongen -> Expression -> StateThrow TypePtr
-- calcType _ _ x | vtrace ("Calctype for " ++ show x) False = error "ignore"
calcType scope nongen (Var _ funcName) = gettype scope nongen funcName
calcType scope nongen e@(ValueConst _ v) = case valuePrim v of
    Just tpe -> aliasForType tpe $ Just e
    _ -> throwError $ TypeError $ "Expection primative value but got value: " ++ show v
calcType scope nongen e@(BuiltIn _ _ t _) = aliasForType t $ Just e
calcType scope nongen e@(SourceExp _ _ _ t) = aliasForType t $ Just e
calcType scope nongen (InnerLet _ t letExp actualExp) =
    do
        let (LetExp _ _ name canBeGen t1 exp) = letExp
        t1' <- aliasForType t1 $ Just exp
        t' <- aliasForType t $ Just actualExp
        t1'' <- prunedToType t1'
        let scope' = Map.insert name t1'' scope
        let nongen' = Set.insert t1' nongen
        calcType scope' nongen' letExp
        calcType scope' nongen' actualExp
        
calcType scope nongen e@(LetExp _ _ name canBeGen t1 exp) =
    do
        putCurExp e
        t1' <- aliasForType t1 $ Just e
        (_, t1'') <- prunedType t1'
        let scope' = Map.insert name t1'' scope
        rt <- calcType scope' (if canBeGen then nongen else Set.insert t1' nongen) exp
        unify t1' rt
        return t1'
calcType scope nongen e@(FuncExp _ paramName pt exp) =
    do
        putCurExp e
        pt' <- aliasForType pt $ Just e
        (_, pt'') <- prunedType pt'
        let scope' = Map.insert paramName pt'' scope
        rt <- calcType scope' (Set.insert pt' nongen) exp
        rpt <- prunedToType pt'
        rrt <- prunedToType rt
        aliasForType (tFun rpt rrt) Nothing
calcType scope nongen e@(Apply _ letId t1 exp1 exp2) =
    do
        putCurExp e
        tp1 <- aliasForType t1 $ Just e
        t1' <- prunedToType tp1
        funType <- calcType scope nongen exp1
        argType <- showStructType funType $ calcType scope nongen exp2
        argType' <- prunedToType argType
        synt <- aliasForType (tFun argType' t1') Nothing
        unify synt funType
        ret <- prunedToType tp1
        funType' <- prunedToType funType
        synt' <- aliasForType (tFun argType' ret) Nothing
        unify synt' funType
        let (TOper _ [_, ret']) = funType'
        aliasForType ret' Nothing
calcType scope nongen e@(SinkExp _ _ name t1 exp) =
    do
        putCurExp e
        t1' <- aliasForType t1 $ Just e
        let scope' = Map.insert name t1 scope
        rt <- calcType scope' (Set.insert t1' nongen) exp
        unify t1' rt
        return rt
calcType scope nongen e@(Group _ exprs _ exp) =
    do
        let it name e t1 = do
                                t1' <- aliasForType t1 $ Just e
                                t1'' <- prunedToType t1'
                                return (name, t1'', isNongen e, t1')
        pairs <- mapM (processExp it) $ Map.assocs exprs
        let scope' = scope `Map.union` Map.fromList (map nv pairs)
        -- all the source expressions are *NOT* generic. We need to
        -- know what the actual types are
        let nongen' = nongen `Set.union` Set.fromList (pairs >>= genPull)
        mapM_ (calcType scope' nongen') $ Map.elems exprs
        aliasForType ((TOper $ T.pack "na") []) (Just e)
calcType scope nongen e@(InvokeMethod _ letId method@(FuncName methodText) (TOper oprName [source, dest])) =
    do
        putCurExp e
        dp <- aliasForType dest $ Just e
        sp <- aliasForType source $ Just e
        t1' <- prunedToType dp
        source' <- prunedToType sp
        aliasForType (TOper oprName [source', t1']) $ Just e
calcType _ _ e = error $ "FIXME calcType " ++ show e


-- showStructType (TOper _ [st@(StructuralType _), other]) rest = vtrace ("Struct type " ++ show st ++ " other " ++ show other) rest
showStructType _ rest = rest

genPull (_, _, True, t) = [t]
genPull _ = []

isNongen (LetExp _ _ _ _ _ (SourceExp _ _ _ t1)) = True
isNongen e = False

nv (name, value, _, _) = (name, value)


-- | Take an Expression which should be a Group and turn it into a scope of top level expressions
buildLetScope :: Expression -> LetScope
buildLetScope (Group _ exprs _ _) = exprs
buildLetScope _ = Map.empty

processExp it (name, e@(LetExp _ _ _ _ t1 _)) = it name e t1
processExp it (name, e@(SinkExp _ _ _ t1 _)) = it name e t1
processExp it (name, e@(SourceExp _ _ _ t1)) = it name e t1
processExp it (name, e@(BuiltIn _ _ t1 _)) = it name e t1

-- we have to repeated run the typer to get all the synthetic variables resolved
processTypes' :: Expression -> [(T.Text, TypePtr)] -> StateThrow [(T.Text, TypePtr)]
processTypes' e@(Group _ exprs _ _) org =
    do
        calcType Map.empty Set.empty e
        let nameType (FuncName name) e t = do
                                    al <- aliasForType t $ Just e
                                    return (name, al)
        ret <- mapM (processExp nameType) $ Map.assocs exprs
        tested <- testEq org ret
        if tested then return ret else processTypes' e ret

testEq :: [(T.Text, TypePtr)] -> [(T.Text, TypePtr)] -> StateThrow Bool
testEq [] [] = return True
testEq [] _ = return False
testEq _ [] = return False
testEq ((n1, t1):r1) ((n2, t2):r2) = 
    if n1 /= n2 then return False 
        else do
          (_, t1') <- prunedType t1
          (_, t2') <- prunedType t2
          if not (testTypeEq t1' t2') then return False else testEq r1 r2

testTypeEq (TVar n1) (TVar n2) = n1 == n2
                                 || (T.take synthLen n1 == synthetic && T.take synthLen n2 == synthetic)
testTypeEq (TOper n1 r1) (TOper n2 r2) = (n1 == n2 && length r1 == length r2)
                                         && foldl' testPairTypeEq True (zip r1 r2)

testTypeEq t1 t2 = t1 `testSame` t2

testPairTypeEq b (t1, t2) = b && testTypeEq t1 t2


processTypes :: Expression -> StateThrow [(T.Text, TypePtr)]
processTypes e = processTypes' e []

