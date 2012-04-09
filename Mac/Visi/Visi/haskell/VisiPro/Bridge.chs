module VisiPro.Bridge(HVisiCmdPtr, convertFromC, doError, doSourceSink, doSetSinks ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import qualified Data.Text as T
import Foreign.Marshal.Alloc
import Visi.Runtime
import Visi.Expression
import Visi.Model

#include "VisiBridge.h"

type VoidPtr = Ptr ()



{#enum cmds as VisiCmds {upcaseFirstLetter} deriving (Show, Eq) #}

{#enum evts as VisiEvents {upcaseFirstLetter} deriving (Show, Eq) #}

{#enum theTypes as VisiTypes {upcaseFirstLetter} deriving (Show, Eq) #}

{-
sendEvent to (SetProgramText text) = 
    do
        ptr <- mallocBytes {#sizeof visi_command #}
        {#set visi_command->cmd #} ptr $ enumMe SetProgramTextCmd
        let doStr s =
                    do
                        {#set visi_command->theInfo.text #} ptr s
                        {#call runCommand #} to ptr
                        free ptr
        withText text doStr
-}

enumMe x = fromIntegral $ fromEnum x
withText t = withCString (T.unpack t)
peekText cStr =
  do
    cs <- peekCString cStr
    return $ T.pack cs

{#pointer *visi_command as HVisiCmdPtr newtype #}

getCommandCmd = {#get visi_command->cmd #}
getCommandText = {#get visi_command->cmdInfo.text #}
getEventCmd = {#get visi_event->cmd #}

newString text = newCString $ T.unpack text

getUnpacked what = 
  do
    cstr <- what 
    ret <- peekText cstr
    return ret

getTarget what = getUnpacked $ {#get visi_command->target#} what


doError objcId err = 
  do
    ptr <- mallocBytes {#sizeof visi_event #}
    {#set visi_event->cmd #} ptr $ enumMe ReportErrorEvent
    errStr <- case err of
      Just err -> newCString $ show err
      _ -> return nullPtr
    {#set visi_event->evtInfo.errorText #} ptr errStr
    {#call sendEvent #} objcId ptr




doSourceSink :: VoidPtr -> [SourceSinkAction] -> IO ()
doSourceSink objcId sourceSinkInfo = 
  let doAction (RemoveSourceAction src) = do
        ptr <- mallocBytes {#sizeof visi_event #}
        {#set visi_event->cmd #} ptr $ enumMe RemoveSourceEvent
        errStr <- newString src
        {#set visi_event->evtInfo.sourceSinkName #} ptr errStr
        {#call sendEvent #} objcId ptr
      doAction (RemoveSinkAction src) = do
        ptr <- mallocBytes {#sizeof visi_event #}
        {#set visi_event->cmd #} ptr $ enumMe RemoveSinkEvent
        errStr <- newString src
        {#set visi_event->evtInfo.sourceSinkName #} ptr errStr
        {#call sendEvent #} objcId ptr
      doAction _ = return () in
  mapM_ doAction sourceSinkInfo


doSetSinks :: VoidPtr -> [(T.Text, Value)] -> IO ()
doSetSinks objcId nvp = do
  putStrLn $ "Do Set Sinks: " ++ show nvp
  return ()

convertFromC :: VoidPtr -> IO (Maybe VisiCommand)
convertFromC what =
  do
    cmd <- getCommandCmd what
    cmdType' <- {#get visi_command->cmdType#} what
    let cmdType = toEnum $ fromIntegral $ cmdType'
    case toEnum $ fromIntegral cmd of
      SetProgramTextCmd -> do
        theStr <- getUnpacked $ {#get visi_command->cmdInfo.text #} what
        return $ Just $ SetProgramText theStr
      SetSourceCmd | cmdType == StringVisiType -> do
        theStr <- getUnpacked $ {#get visi_command->cmdInfo.text #} what
        target <- getTarget what
        return $ Just (SetValueSource target $ StrValue theStr)
      SetSourceCmd | cmdType == DoubleVisiType -> do
        cNum <- {#get visi_command->cmdInfo.number #} what
        target <- getTarget what
        return $ Just (SetValueSource target $ DoubleValue $ realToFrac cNum)

      SetSourceCmd | cmdType == BoolVisiType -> do
        bool <- ({#get visi_command->cmdInfo.boolValue #} what)
        target <- getTarget what
        return $ Just (SetValueSource target $ BoolValue (bool /= 0))
      StopRunningCmd -> return $ Just StopRunning
      _ -> return $ Nothing


{-
{#pointer *test_t as Test#}

getA :: Test -> IO Int
getA t = {#get test_t->a#} t >>= return . fromIntegral

setA :: Test -> Int -> IO ()
setA t i = {#set test_t->a#} t (fromIntegral i)

-}