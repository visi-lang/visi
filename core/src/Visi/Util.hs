module Visi.Util (flatten, Stringable, VisiError(TypeError, ParsingError, DefaultError), ThrowsError,
					passthru, listify, justFunc, vtrace, justOr, (|-), unsafeRandom,
                    buildMessageQueue, intHash, hexHash,
                    runOnThread) where

import Control.Monad.Error
import Text.Parsec
import Debug.Trace
import System.IO.Unsafe
import System.Random
import qualified Data.Map as Map
import System.IO.Unsafe
import Control.Concurrent
import Data.IORef
import Data.Digest.Pure.SHA
import qualified Data.Text as T
import Data.ByteString.Lazy.UTF8
import qualified Control.Exception as E
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

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

-- | flatten a List
flatten :: [[a]] -> [a]
flatten = concat

-- | Trace that you can disable
vtrace :: String -> a -> a
-- vtrace _ a = a
vtrace msg a = trace msg a

data VisiError = 
    TypeError String 
    | ParsingError ParseError
    | DefaultError String
    -- deriving (Eq)

type ThrowsError = Either VisiError

-- | Convert the error to a String
showError :: VisiError -> String
showError (TypeError str) = "Type error: " ++ str
showError (DefaultError str) = "Uncategorized error: " ++ str
showError (ParsingError pe) = show pe

-- | Create a random value... without actually being in the
-- | IO monad
unsafeRandom x = unsafePerformIO $ randomIO

instance Show VisiError where show = showError

instance Error VisiError where
    noMsg = DefaultError "An error has occurred"
    strMsg = DefaultError
    
justOr thing err = case thing of
                    (Just v) -> {-return-} v
                    _ -> err
                    

-- | Turn a Maybe into a List of 0 or 1 element
listify (Just a) = [a]
listify _ = []

passthru x = x

justFunc val maybe func =
	case maybe of
		Just x -> func x
		_ -> val


-- | Build a non-blocking message queue and associated
-- | place to pull values placed in the Queue.  Basically,
-- | this is Actors because the non-blocking message send is
-- | a -> IO ()... returning a unit is generally a bad thing,
-- | but this code exists to deal with creating non-blocking
-- | GUIs.
-- | Also, please note that this is what Iteratees and the 
-- | like where meant to deal with and at some point, I intend
-- | to rip out this code and anything that depends on it and
-- | replace it with Iteratees. @dpp
buildMessageQueue :: b -> (b -> a -> IO (Maybe b)) -> IO (a -> IO ())
buildMessageQueue start func =
    do
        mvar <- newEmptyMVar
        running <- newIORef True
        let runIt v = do
            doIt <- readIORef running
            if doIt then runOnThread $ putMVar mvar v else return ()
        let loopy state = E.catch (do
                                      v <- takeMVar mvar
                                      res <- func state v
                                      case res of
                                          Just state' -> loopy state'
                                          _ -> writeIORef running False)
                                  (\e -> do let err = show (e :: E.SomeException)
                                            hPutStrLn stderr ("Warning: Execution Failure: " ++ show e)
                                            loopy state)
        runOnThread $ loopy start
        return $ runIt

-- | Run the IO action on another thread
runOnThread :: IO () -> IO ()
runOnThread = 
  unsafePerformIO $ do
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

-- | calculate the hash of a text string and return the right-most bits
intHash :: Stringable a => a -> Int
intHash text =
    fromIntegral $ integerDigest $ sha1 $ fromString $ cvtToString text

hexHash :: Stringable a => a -> String
hexHash text = showDigest $ sha1 $ fromString $ cvtToString text

class Stringable s where
    cvtToString :: s -> String

instance Stringable String where
    cvtToString s = s

instance Stringable T.Text where
    cvtToString s = T.unpack s

instance Stringable B.ByteString where
    cvtToString = UB.toString


(|-) :: Either b a -> (a -> Either b c) -> Either b c
(Right a) |- f = f a
(Left b) |- _ = Left b