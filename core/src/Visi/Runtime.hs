module Visi.Runtime (ErrorCallback, 
                     SourceSinkCallback, SetSinksCallback,
                     SourceSinkInfo(SourceInfo, SinkInfo),
                     AppCallback(AppCallback),
                     ExecCommand,
                     VisiCommand(SetProgramText, SetStringSource, SetNumberSource, SetBoolSource, StopRunning),
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
import System.IO.Unsafe
import Visi.Expression
import Visi.Parse
import Control.Concurrent
import Visi.Executor
import Visi.Typer
import Data.IORef
import Control.Exception
import Visi.Model
import qualified Data.Text as T

-- | The visi runtime... an interface between abstract systems and the current system
-- | Right now, we only support a single app, but we'll expand to multiple running apps
 
type ErrorCallback = Maybe T.Text -> IO ()
type SourceSinkCallback = [SourceSinkInfo] -> IO ()
type SetSinksCallback = [(T.Text, Value)] -> IO ()

data AppCallback = AppCallback ErrorCallback SourceSinkCallback SetSinksCallback
 
data SourceSinkInfo = 
    SourceInfo T.Text Prim
    | SinkInfo T.Text Prim 
    deriving (Show)

{- 
runningApp :: IORef (Maybe (Chan AppCmd))
runningApp = unsafePerformIO $ newIORef Nothing

-- | Commands sent to a running app
data AppCmd = 
    AppStop
    | AppSetSource T.Text Value

setSource :: T.Text -> Value -> IO ()
setSource name value =
    let set = AppSetSource name value in
    do
        chan <- readIORef runningApp
        maybeChan chan $ flip writeChan set
-}

-- runOnThread :: 
runOnThread = 
  do
    localVar <- newEmptyMVar
    let loopIt = do
        func <- takeMVar localVar
        forkIO func
        loopIt
    let run func = do
        putMVar localVar func
    forkOS loopIt -- 3 OS threads running the round robin command handler
    forkOS loopIt -- FIXME get a real thread pool
    forkOS loopIt
    return run

data VisiCommand = SetProgramText T.Text
                   | SetStringSource T.Text T.Text
                   | SetNumberSource T.Text Double
                   | SetBoolSource T.Text Bool
                   | StopRunning deriving (Show)

type ExecCommand = VisiCommand -> IO ()

-- | run an application
runApp :: AppCallback -> IO ExecCommand
runApp callback@(AppCallback errorCallback sourceSinkCallback setSinksCallback) = 
  do
    mvar <- newEmptyMVar
    let runIt v = do
                    putMVar mvar v
    tf <- runOnThread
    let sinkAction model name value = setSinksCallback [(name, value)]
    let theModel = setDefaultSinkAction sinkAction $ newModel (T.pack "MyModel") Nothing
    tf $ doRunRun mvar theModel callback
    putStrLn "Dude... I started one!!!"
    return runIt

doRunRun mvar model callback@(AppCallback errorCallback sourceSinkCallback setSinksCallback) =
  do
    v <- takeMVar mvar
    putStrLn $ "Got cmd " ++ (show v)
    case v of
      StopRunning -> do
        return ()

      SetProgramText text -> case setModelCode text model of
                              Left (err, model) -> do
                                putStrLn $ "Got err" ++ (show err)
                                errorCallback $ Just $ T.pack $ show err
                                doRunRun mvar model callback

                              Right (model, updates) -> do
                                putStrLn $ "Success for sinks " ++ (show updates)
                                errorCallback Nothing
                                runSinkActions updates model
                                doRunRun mvar model callback
        

      _ -> doRunRun mvar model callback

baseProgram =
  case parseLines "" of
    Right exps -> let allExp = builtInExp ++ exps in
      let grp = mkGroup allExp in
      (grp, allExp)


{-
setProgramText callback@(AppCallback errorCallback sourceSinkCallback setSinksCallback) code =
  do
    let stuff = parseLines $ T.unpack code
    runIt stuff
    where
      runIt (Left err) = errorCallback $ Just $ T.pack $ show err
      runIt (Right exps) = runExp exps
      runExp exps =
        let allExp = builtInExp ++ exps in
        let grp = mkGroup allExp in
        do
          forkIO 
-}
{-
    let shutDown it = (Nothing, it) in
    do
      chan <- atomicModifyIORef runningApp shutDown
      maybeChan chan $ flip writeChan AppStop -- shutdown the app if it was running
      let stuff = parseLines code
      runIt stuff
      where runIt (Left err) = errorCallback $ T.pack $ show err
            runIt (Right exps) = runExp exps
            runExp exps = 
                let allExp = builtInExp ++ exps in
                let grp = mkGroup allExp  in
                do
                    chan <- newChan
                    writeIORef runningApp $ Just chan
                    forkOS $ threadRunApp callback chan grp allExp
                    return ()
-}    

{-
fromSink (name, _, TPrim p) = SinkInfo name p
fromSource (name, TPrim p) = SourceInfo name p

threadRunApp :: AppCallback -> Chan AppCmd -> Expression -> [Expression] -> IO ()
threadRunApp callback@(AppCallback errorCallback sourceSinkCallback setSinksCallback) chan grp top =
    Control.Exception.catch
        (do
            case collectTypes grp of 
              Right lets ->
                do
                  let theScope = buildLetScope grp
                  let typeMap = Map.fromList lets
                  let doEval vars (name, expr) = (name, eval vars theScope expr)
                  let sinks = calcSinks top typeMap
                  let sinkTypes = map fromSink sinks -- FIXME deal with non-primative
                  let sources = calcSources top typeMap
                  let sourceTypes = map fromSource sources -- FIXME deal with non-primative
                  sourceSinkCallback $ sinkTypes ++ sourceTypes
                  let sink' = do
                               (name, expr, _) <- sinks
                               return (name, expr)
                  let sinkSets = map (doEval Map.empty) sink'
                  setSinksCallback sinkSets
                  let runLoop vars =
                        do
                            toDo <- readChan chan
                            runCmd toDo
                        where runCmd AppStop = return ()
                              runCmd (AppSetSource name value) = 
                                  do
                                    let sinkSets = map (doEval newVars) sink'
                                    setSinksCallback sinkSets
                                    runLoop newVars
                                    where newVars = Map.insert name value vars  -- FIXME recalc and resend all sinks
                  runLoop Map.empty
              Left error -> errorCallback $ T.pack $ show error
            ) 
          (\e -> do
                   errorCallback $ T.pack $ show (e :: ErrorCall)
                   writeIORef runningApp Nothing
                   return ()) -- find & send sources and sinks or type errors
          
-}
