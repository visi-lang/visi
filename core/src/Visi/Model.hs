module Visi.Model (Model, SinkActionInfo, newModel, setSourceValue, createSinkAction,
    SinkAction, addSinkAction, removeSinkAction, modelSources, modelSinks, setModelCode,
    runSinkActions, calcSourceSinkDeltas,
    SourceSinkAction(AddSourceAction, AddSinkAction, RemoveSourceAction, RemoveSinkAction),

    setDefaultSinkAction, removeDefaultSinkAction) where
    
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
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** -}   

import qualified Data.Text as T
import Visi.Expression
import qualified Data.Map as Map
import Data.UUID
import Visi.Util
import Visi.Parse
import Visi.Executor
import Visi.Typer
import Visi.Expression

type SinkAction a = Model a -> Int -> Value -> IO ()

data SourceSinkAction = 
    AddSourceAction Int T.Text Type |
    RemoveSourceAction Int |
    AddSinkAction Int T.Text Type |
    RemoveSinkAction Int
    deriving (Show, Eq)

data Model a = Model {
    name :: T.Text,
    code :: Maybe T.Text,
    letScope :: Either VisiError LetScope,
    sources :: Map.Map Int (T.Text, Type, Value),
    sinks :: Map.Map Int (SinkStuff a),
    defaultSinkAction :: Maybe (SinkAction a),
    localData :: Maybe a
    } deriving (Show)

data SinkStuff a = SinkStuff {sinkName :: T.Text, sinkType :: Type, sinkValue :: Value,
    sinkExpression :: Maybe Expression,
    sinkGuid :: Int,
    sinkActions :: [SinkActionInfo a]} deriving (Show)

data SinkActionInfo a = SinkActionInfo UUID (SinkAction a) deriving (Show)

-- | create a blank model
-- blankModel :: Model String
{-
blankModel = Model {name = T.pack "blank", code = Nothing, letScope = Left $ DefaultError "No Model",
                    sources = Map.empty, sinks = Map.empty, defaultSinkAction = Nothing,
                    localData = Nothing}
-}

-- | Create a new, named model
newModel :: T.Text -> Maybe a -> Model a
newModel n ld = Model {name = n, code = Nothing, letScope = Left $ DefaultError "No Model",
                       sources = Map.empty, sinks = Map.empty, defaultSinkAction = Nothing,
                       localData = ld}

-- | given a model, set the default sink action
-- | Whenever a new sink is created, the default sink action is added to that sink
setDefaultSinkAction action model =
    model {defaultSinkAction = Just action}

-- | remove the default sink action from the model
removeDefaultSinkAction model = model {defaultSinkAction = Nothing}

modelLocalData :: Model a -> Maybe a
modelLocalData model = localData model

-- | Is the model valid? Does it have correctly defined code?
validModel :: Model a -> Bool
validModel model =
    case letScope model of
        Right _ -> True
        _ -> False

-- | Calculate the difference in sources and sinks between the two models
calcSourceSinkDeltas :: Model a -> Model a -> [SourceSinkAction]
calcSourceSinkDeltas old new = 
  let pairIt (a, b, _) = (a,b) in
  let cvt = Map.map pairIt in
  let oldSource = cvt $ modelSources old in
  let newSource = cvt $ modelSources new in
  let cmp a b = if a == b then Nothing else Just a in
  let diff = Map.differenceWith cmp in
  let removeSource = diff oldSource newSource in
  let addSource = Map.toList $ diff newSource oldSource in
  let oldSink = cvt $ modelSinks old in
  let newSink = cvt $ modelSinks new in
  let removeSinks = diff oldSink newSink in
  let addSinks = Map.toList $ diff newSink oldSink in
  (map RemoveSourceAction $ Map.keys removeSource) ++
    (map (\(n, (name, t)) -> AddSourceAction n name t) addSource) ++
    (map RemoveSinkAction $ Map.keys removeSinks) ++
    (map (\(n, (name, t)) -> AddSinkAction n name t) addSinks)

-- | get the sources of the Model
modelSources :: Model a -> Map.Map Int (T.Text, Type, Value)
modelSources = sources

modelSinks :: Model a -> Map.Map Int (T.Text, Type, Value)
modelSinks model = 
  let pairMe sink = (sinkName sink, sinkType sink, sinkValue sink) in
  Map.map pairMe $ sinks model

fst3 (a, _, _) = a

mergeSourceInfo :: Map.Map Int (T.Text, Type, Value) -> [(Int, T.Text, Type)] -> Map.Map Int (T.Text, Type, Value)
mergeSourceInfo old new =
    Map.fromList $ map lookItUp new
    where
        lookItUp (guid, name, tpe) = case Map.lookup guid old of
            Just (oldName, oldType, oldVal) | oldType == tpe -> (guid, (name, tpe, oldVal))
            _ -> (guid, (name, tpe, defaultValueForType tpe))

-- | sets or updates the model code
setModelCode :: T.Text -> Model a -> Either (VisiError, Model a) (Model a, [T.Text])
setModelCode theCode model = 
    let updatedModel = model {code = Just theCode} in
    case parseLines $ T.unpack theCode of
        Left err -> Left (err, updatedModel {letScope = Left err})
        Right exps -> let top = builtInExp ++ exps in
             let grp = mkGroup top in
             case collectTypes grp of
                Left err -> Left (err, updatedModel {letScope = Left err})
                Right lets -> do
                  let theScope = buildLetScope grp
                  let typeMap = Map.fromList lets
                  let doEval vars (name, expr) = (name, eval vars theScope expr)
                  let sinks' = calcSinks top typeMap
                  let theSources = calcSources top typeMap
                  let theSources' =  mergeSourceInfo (sources updatedModel) theSources
                  let sourceVars = Map.map snd theSources'
                  let oldSinks = sinks updatedModel
                  let buildSinkMap = let calcSink (name, expr, tpe) = let theActions = justOr (fmap sinkActions 
                                                                                        (Map.lookup name oldSinks))
                                                                                        (fmap createSinkAction (listify $ 
                                                                                            defaultSinkAction updatedModel)) in
                                                     let calcValue = eval sourceVars theScope expr in
                                                     (name, SinkStuff{sinkName = name, sinkType = tpe, 
                                                        sinkExpression = Just expr,
                                                         sinkValue = calcValue, sinkActions = theActions}) in
                        Map.fromList $ map calcSink sinks'
                  let model' = updatedModel {letScope = Right theScope, 
                                             sources = theSources',
                                             sinks = buildSinkMap
                                             }
                  return (model', map fst3 sinks')


-- | Set a value in a model, recompute the model and list all the Sinks that changed
setSourceValue :: T.Text -> Value -> Model a -> Either VisiError (Model a, [Int])
setSourceValue name value model = 
    let theSources = sources model in
    case (Map.lookup name theSources, letScope model) of
        (_, Left err) -> Left err
        (Nothing, _) -> Left $ DefaultError $ "Source not found: " ++ (T.unpack name)
        (Just (tpe, _), _) | (Just tpe) /= valuePrim value -> Left $ DefaultError $ "Type mismatch for source: " ++ (T.unpack name)
        (Just (tpe, oldVal), Right theScope) | oldVal /= value -> let newSources = Map.insert name (tpe, value) theSources in
            let sourceVars = Map.map snd newSources in
            let calcValue expr = eval sourceVars theScope expr in
            let sinks' = sinks model in
            let recalcIt old = case sinkExpression old of
                                  Just expr -> old {sinkValue = eval sourceVars theScope expr}
                                  _ -> old in
            let recalcSinks = Map.map recalcIt sinks' in
            Right (model {sources = newSources, sinks = recalcSinks}, Map.keys sinks')
        _ -> Right (model, [])

runSinkActions :: [Int] -> Model a -> IO ()
runSinkActions names model =
    do
        let actions = collectSinkActions names model
        mapM_ passthru actions

collectSinkActions :: [Int] -> Model a -> [IO ()]
collectSinkActions names model =
    do
        let snk = sinks model
        name <- names
        stuff <- listify $ Map.lookup name snk
        let actions = sinkActions stuff
        let sn = sinkName stuff
        let sv = sinkValue stuff
        action <- actions
        let (SinkActionInfo _ func) = action
        return $ func model sn sv

addSinkAction :: Int -> SinkActionInfo a -> Model a -> Model a
addSinkAction name action model =
    model {sinks = updateSinks $ sinks model}
    where updateSinks sinkMap = Map.adjust (\s -> s {sinkActions = action:(sinkActions s)}) name sinkMap 


removeSinkAction :: Int -> SinkActionInfo a -> Model a -> Model a
removeSinkAction name action model =
    model {sinks = updateSinks $ sinks model}
    where updateSinks sinkMap = Map.adjust (\s -> s {sinkActions = filter (/= action) (sinkActions s)}) name sinkMap

-- | Given a function to execute when the Sink value changes
-- | create a sinkAction that can be added to a model (and later removed)
createSinkAction :: SinkAction a -> SinkActionInfo a
createSinkAction sa = SinkActionInfo (unsafeRandom 33) sa


instance Eq (SinkActionInfo a) where
    (SinkActionInfo uuid1 _) == (SinkActionInfo uuid2 _) = uuid1 == uuid2

instance Show (SinkAction a) where
    show _ = "<SinkAction>"