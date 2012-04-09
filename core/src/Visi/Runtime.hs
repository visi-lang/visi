module Visi.Runtime (ErrorCallback, 
                     SourceSinkCallback, SetSinksCallback,
                     AppCallback(AppCallback),
                     ExecCommand,
                     VisiCommand(SetProgramText, SetValueSource, StopRunning),
                     runApp) where
    
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
import Visi.Expression
import Visi.Parse
import Visi.Executor
import Visi.Typer
import Visi.Model
import Visi.Util
import qualified Data.Text as T

-- | The visi runtime... an interface between abstract systems and the current system
-- | Right now, we only support a single app, but we'll expand to multiple running apps
 
type ErrorCallback = Maybe VisiError -> IO ()
type SourceSinkCallback = [SourceSinkAction] -> IO ()
type SetSinksCallback = [(T.Text, Value)] -> IO ()

data AppCallback = AppCallback ErrorCallback SourceSinkCallback SetSinksCallback

data VisiCommand = SetProgramText T.Text
                   | SetValueSource T.Text Value
                   | StopRunning deriving (Show)

type ExecCommand = VisiCommand -> IO ()

-- | run an application
runApp :: AppCallback -> IO ExecCommand
runApp callback@(AppCallback errorCallback sourceSinkCallback setSinksCallback) = 
  do
    let sinkAction model name value = setSinksCallback [(name, value)]
    let theModel = setDefaultSinkAction sinkAction $ newModel (T.pack "MyModel") Nothing
    runIt <- buildMessageQueue (theModel, callback) doRunRun
    return runIt

processSourceSetting callback@(AppCallback errorCallback sourceSinkCallback setSinksCallback) name value model =
  case setSourceValue name value model of
    Left err -> do
      errorCallback $ Just err
      return model
    Right (model', toRun) -> do
      errorCallback $ Nothing
      runSinkActions toRun model'
      return model'

doRunRun :: (Model T.Text, AppCallback) -> VisiCommand -> IO (Maybe (Model T.Text, AppCallback))
doRunRun (model, callback@(AppCallback errorCallback sourceSinkCallback setSinksCallback)) v =
  do
    putStrLn $ "Got cmd " ++ (show v)
    case v of
      StopRunning -> return Nothing

      SetProgramText text -> case setModelCode text model of
                              Left (err, model') -> do
                                errorCallback $ Just err
                                return $ Just (model', callback)

                              Right (model', updates) -> do
                                -- putStrLn $ "The model is " ++ show model'
                                errorCallback Nothing
                                let sourceSinkDeltas = calcSourceSinkDeltas model model'
                                sourceSinkCallback sourceSinkDeltas
                                runSinkActions updates model'
                                return $ Just (model', callback)

      SetValueSource name value -> do
        updatedModel <- processSourceSetting callback name value model
        return $ Just (updatedModel, callback)

