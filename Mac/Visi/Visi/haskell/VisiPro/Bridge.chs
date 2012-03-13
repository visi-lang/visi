module VisiPro.Bridge(VisiCommand(SetProgramText, SetStringSource, SetNumberSource, SetNumberBool),
    sendEvent) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import qualified Data.Text as T
import Foreign.Marshal.Alloc

#include "Test.h"

type VoidPtr = Ptr ()

data VisiCommand = SetProgramText T.Text
                   | SetStringSource T.Text T.Text
                   | SetNumberSource T.Text Double
                   | SetNumberBool T.Text Bool

{#enum cmds as VisiCmds {upcaseFirstLetter} deriving (Show, Eq) #}



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

enumMe x = fromIntegral $ fromEnum x
withText t = withCString (T.unpack t)


{#pointer *visi_command as HVisiCmdPtr newtype #}

getCommandCmd = {#get visi_command->cmd #}
getCommandText = {#get visi_command->theInfo.text #}
getEventCmd = {#get visi_event->cmd #}


{-
{#pointer *test_t as Test#}

getA :: Test -> IO Int
getA t = {#get test_t->a#} t >>= return . fromIntegral

setA :: Test -> Int -> IO ()
setA t i = {#set test_t->a#} t (fromIntegral i)

-}