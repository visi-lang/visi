module Visi.Runtime (ErrorCallback, 
                     SourceSinkCallback, SetSinksCallback,
                     SourceSinkInfo(SourceInfo, SinkInfo),
                     AppCallback(AppCallback),
                     runApp, setSource) where
    
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
import System.IO.Unsafe
import Visi.Expression
import Visi.Parse
import Control.Concurrent
import Visi.Util
import qualified Text.PrettyPrint as PP
import Data.IORef
import Control.Concurrent.Chan
import Control.Exception

-- | The visi runtime... an interface between abstract systems and the current system
-- | Right now, we only support a single app, but we'll expand to multiple running apps
 
type ErrorCallback = String -> IO ()
type SourceSinkCallback = [SourceSinkInfo] -> IO ()
type SetSinksCallback = [(String, Value)] -> IO ()

data AppCallback = AppCallback ErrorCallback SourceSinkCallback SetSinksCallback
 
data SourceSinkInfo = 
    SourceInfo String Prim
    | SinkInfo String Prim 
    deriving (Show)
 
runningApp :: IORef (Maybe (Chan AppCmd))
runningApp = unsafePerformIO $ newIORef Nothing

-- | Commands sent to a running app
data AppCmd = 
    AppStop
    | AppSetSource String Value

setSource :: String -> Value -> IO ()
setSource name value =
    let set = AppSetSource name value in
    do
        chan <- readIORef runningApp
        maybeChan chan $ flip writeChan $ set

-- | run an application
-- FIXME this is a concurrency problem... if two different runApp calls are made
-- before the app is set up.
runApp :: String -> AppCallback -> IO ()
runApp code callback@(AppCallback errorCallback sourceSinkCallback setSinksCallback) = 
    let shutDown it = (Nothing, it) in
    do
      chan <- atomicModifyIORef runningApp shutDown
      maybeChan chan $ flip writeChan $ AppStop -- shutdown the app if it was running
      let stuff = parseLines code
      runIt stuff
      where runIt (Left err) = errorCallback $ show err
            runIt (Right exps) = runExp exps
            runExp exps = 
                let allExp = builtInExp ++ exps in
                let grp = mkGroup allExp  in
                do
                    chan <- newChan
                    writeIORef runningApp $ Just chan
                    forkOS $ threadRunApp callback chan grp allExp
                    return ()
      
threadRunApp :: AppCallback -> (Chan AppCmd) -> Expression -> [Expression] -> IO ()
threadRunApp callback@(AppCallback errorCallback sourceSinkCallback setSinksCallback) chan grp top =
    Control.Exception.catch
        (do
            let typeVars = collectVars Nothing grp
            let (atv, t) = collectSubs Map.empty typeVars grp
            let (atv', lets) = resolveLets grp atv
            let typeMap = Map.fromList lets
            let resolvedTypes = List.foldl' doFold atv' $ List.map fst $ Map.toList (mm atv')
                                   where doFold world tn = snd $ thread loopSet (TVar tn) world
            let theScope = letScope grp
            let doEval vars (name, expr) = (name, eval vars theScope expr)
            let calcSinkType name = (mm resolvedTypes) Map.! name
            let sinks = calcSinks top
            let sinkTypes = do
                                (name, _, tv) <- sinks
                                let (TVarInfo _ _ _ (MustBe(TPrim theType))) = calcSinkType tv
                                return $ SinkInfo name theType
            let sources = calcSources top
            let sourceTypes = do
                                (name, tv) <- sources
                                let (TVarInfo _ _ _ (MustBe(TPrim theType))) = calcSinkType tv
                                return $ SourceInfo name theType
            
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
            ) 
          (\e -> do
                   errorCallback $ show (e :: ErrorCall)
                   writeIORef runningApp Nothing
                   return ()) -- find & send sources and sinks or type errors
          


             {-
             main = 
                 do
                   putStrLn "Hello"
                   let stuff = parseLines samples
                   let top = mkGroup $ boolTrue : boolFalse : builtInConcat : builtInIf : builtInAdd : 
                             builtInMult : builtInSub : builtInDiv : 
                             builtInLen : builtInShow : stuff 
                   let typeVars = collectVars top 
                   let (atv, t) = collectSubs Map.empty typeVars top 
                   let (atv', lets) = resolveLets top atv
                   putStrLn $ "Subset is "++ (flatten $ List.map (\a -> show a ++ "\n") lets)
                   let theScope = letScope top
                   let resolvedTypes = List.foldl' doFold atv' $ List.map fst $ Map.toList (mm atv')
                                       where doFold world tn = snd $ thread loopSet (TVar tn) world
                   -- putStrLn $ flatten $ List.map (\a -> show a ++ "\n") $ Map.toList $ mm resolvedTypes
                   let main = theScope Map.! (FuncName "main")
                   let res = eval theScope main
                   putStrLn $ "Coolio... the res is " ++ show res
                   putStrLn "Done"
             -}


{-

{-
samples = [
"main = (show(moose + a(99) + fish(\"33\"))) & \" : \" & strc",
-- "main = mult4 add5 6",
"mouse p = len p",
"fish p = cos(mouse p)",
"sin p = p * 3",
"cos p = p + 8",
"moose = (5 * cat) + (sin (10 + 3) + (len strc)) + fish(\"44\")",
"strc = \"string\"",
"cat = 88",
"both = 1 + 1",
"frog p = p + 1",
"slen s = len s",
"loop p = p + 1",
"one p = (two(p)) + 1",
"two p = (one(p + 1)) + 1",
"three = id(33)",
"add a b = a + b",
"add5 = add 5",
-- "mult a b = a * b",
-- "mult4 a = mult 4",
"p = \"hi\"",
"sloth p q r s = p + q + r + (len s)",
"multi a b c d = a + b + c + d",
"a p = b(p + 1)",
"b p = c(p + 1)",
"c p = d(p + 1)",
"d p = e(p + 1)",
"e p = f(p + 1)",
"f p = g(p + 1)",
"g p = h(p + 1)",
"h p = i(p + 1)",
"i p = j(p + 1)",
"j p = k(p + 1)",
"k p = l(p + 1)",
"l p = m(p + 1)",
"m p = n(p + 1)",
"n p = o(p + 1)",
"o p = p * 6",
-- "four = id(\"33\")",
"id p = p",
"by = 33"
]
-}

samples = [
"main = 33 + (mouse \"22\") + (slug 33 44)",
"slug a b = if maybe then a else b",
"mouse p = len p // hello dude ",
--"sink[m2] = main",
-- "mouse2 p = p + 33",
"maybe = source[maybe]",
-- "meow = if true then \"44\" else \"elwood\"",
"last = true"
]
-}
{-
sink[Total] = total
sink[Tax] = tax
total = subtotal + tax
tax = taxable * taxRate
subtotal = taxable + nontaxable
taxRate = source[TaxRate]
taxable = source[Taxable]
nontaxable = source[NonTaxable]
sink[Dog] = dog
dog = if true then (reverse "hello") else (reverse "dude")    

-}


