module VisiPro.Bridge(HVisiCmdPtr, convertFromC, doError, doSourceSink, doSetSinks ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import qualified Data.Text as T
import Foreign.Marshal.Alloc
import Visi.Runtime

#include "VisiBridge.h"

type VoidPtr = Ptr ()



{#enum cmds as VisiCmds {upcaseFirstLetter} deriving (Show, Eq) #}

{#enum evts as VisiEvents {upcaseFirstLetter} deriving (Show, Eq) #}

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


doError objcId text = 
  do
    ptr <- mallocBytes {#sizeof visi_event #}
    {#set visi_event->cmd #} ptr $ enumMe ReportErrorEvent
    errStr <- case text of
      Just text -> newString text
      _ -> return nullPtr
    {#set visi_event->evtInfo.errorText #} ptr errStr
    {#call sendEvent #} objcId ptr



doSourceSink objcId sourceSinkInfo = return ()

doSetSinks objcId nvp = return ()

{-
type ErrorCallback = T.Text -> IO ()
type SourceSinkCallback = [SourceSinkInfo] -> IO ()
type SetSinksCallback = [(T.Text, Value)] -> IO ()
-}

convertFromC :: VoidPtr -> IO VisiCommand
convertFromC what =
  do
    cmd <- getCommandCmd what
    case toEnum $ fromIntegral cmd of
      SetProgramTextCmd -> do
        theStr <- getUnpacked $ {#get visi_command->cmdInfo.text #} what
        return $ SetProgramText theStr
      SetStringSourceCmd -> do
        theStr <- getUnpacked $ {#get visi_command->cmdInfo.text #} what
        target <- getTarget what
        return (SetStringSource target theStr)
      SetNumberSourceCmd -> do
        cNum <- {#get visi_command->cmdInfo.number #} what
        target <- getTarget what
        return (SetNumberSource target $ realToFrac cNum)

      SetBoolSourceCmd -> do
        bool <- ({#get visi_command->cmdInfo.boolValue #} what)
        target <- getTarget what
        return (SetBoolSource target (bool /= 0))
      StopRunningCmd -> return StopRunning


{-
{#pointer *test_t as Test#}

getA :: Test -> IO Int
getA t = {#get test_t->a#} t >>= return . fromIntegral

setA :: Test -> Int -> IO ()
setA t i = {#set test_t->a#} t (fromIntegral i)

-}