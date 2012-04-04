module Visi.Model (Model, SinkActionInfo, blankModel, newModel, setSourceValue, createSinkAction,
    SinkAction, addSinkAction, removeSinkAction) where
    
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

type SinkAction = T.Text -> Value -> IO ()

data Model = Model {
    name :: T.Text,
    code :: Maybe T.Text,
    letScope :: Either T.Text LetScope,
    sources :: Map.Map T.Text (Type, Value),
    sinks :: Map.Map T.Text SinkStuff,
    defaultSinkAction :: Maybe SinkAction
    } deriving (Show)

data SinkStuff = SinkStuff {sinkName :: T.Text, sinkType :: Type, sinkValue :: Value,
 sinkActions :: [SinkActionInfo]} deriving (Show)

data SinkActionInfo = SinkActionInfo UUID SinkAction deriving (Show)

-- | create a blank model
blankModel :: Model
blankModel = Model {name = T.pack "blank", code = Nothing, letScope = Left $ T.pack "blank",
                    sources = Map.empty, sinks = Map.empty, defaultSinkAction = Nothing}

-- | Create a new, named model
newModel :: T.Text -> Model
newModel n = Model {name = n, code = Nothing, letScope = Left $ T.pack "blank",
                    sources = Map.empty, sinks = Map.empty, defaultSinkAction = Nothing}

-- | given a model, set the default sink action
-- | Whenever a new sink is created, the default sink action is added to that sink
setDefaultSinkAction action model =
    model {defaultSinkAction = Just action}

-- | remove the default sink action from the model
removeDefaultSinkAction model = model {defaultSinkAction = Nothing}

-- | sets or updates the model code
-- setModelCode :: T.Text -> Model -> Model

-- | Set a value in a model, recompute the model and list all the Sinks that changed
setSourceValue :: T.Text -> Value -> Model -> (Model, [T.Text])
setSourceValue name value model = error "FIXME - - implement setValue"

runSinkActions :: [T.Text] -> Model -> IO ()
runSinkActions names model =
    do
        let actions = collectSinkActions names model
        mapM_ (\x -> x) actions

collectSinkActions :: [T.Text] -> Model -> [IO ()]
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
        return $ func sn sv

listify (Just a) = [a]
listify _ = []

addSinkAction :: T.Text -> SinkActionInfo -> Model -> Model
addSinkAction name action model =
    model {sinks = updateSinks $ sinks model}
    where updateSinks sinkMap = case Map.lookup name sinkMap of
                                    Just stuff -> Map.insert name 
                                                    (stuff {sinkActions = action:(sinkActions stuff)}) sinkMap
                                    _ -> sinkMap

removeSinkAction :: T.Text -> SinkActionInfo -> Model -> Model
removeSinkAction name action model =
    model {sinks = updateSinks $ sinks model}
    where updateSinks sinkMap = case Map.lookup name sinkMap of
                                    Just stuff -> Map.insert name 
                                                    (stuff {sinkActions = filter (/= action) (sinkActions stuff)}) sinkMap
                                    _ -> sinkMap
-- | Given a function to execute when the Sink value changes
-- | create a sinkAction that can be added to a model (and later removed)
createSinkAction :: SinkAction -> SinkActionInfo
createSinkAction sa = SinkActionInfo (unsafeRandom 33) sa


instance Eq SinkActionInfo where
    (SinkActionInfo uuid1 _) == (SinkActionInfo uuid2 _) = uuid1 == uuid2

instance Show SinkAction where
    show _ = "<SinkAction>"