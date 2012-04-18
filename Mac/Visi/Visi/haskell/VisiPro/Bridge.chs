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
import Visi.Util
import Data.Bits

#include "VisiBridge.h"

type VoidPtr = Ptr ()



{#enum cmds as VisiCmds {upcaseFirstLetter} deriving (Show, Eq) #}

{#enum evts as VisiEvents {upcaseFirstLetter} deriving (Show, Eq) #}

{#enum theTypes as VisiTypes {upcaseFirstLetter} deriving (Show, Eq) #}

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


getTarget :: VoidPtr -> IO T.Text
getTarget what = do
  targetPtr <- {#get visi_command->cmdTarget#} what
  peekText targetPtr

mask = 0x7ffffffffff

findHash model val =
  let testV = val .&. mask in
  let test a = vtrace ("Testing " ++ (show a) ++ " masked " ++ (show (a .&. mask)) ++ " against " ++ (show testV)) $ (a .&. mask) == testV in
  stringFromHash model test

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
  do
    let addIt name cmd tpe = do
                              ptr <- mallocBytes {#sizeof visi_event #}
                              name' <- newString name
                              {#set visi_event->evtInfo.sourceSinkName #} ptr name'
                              putStrLn $ "Setting source " ++ (show name) ++ " hash " ++ (show $ (intHash name) .&. mask)
                              {#set visi_event->targetHash #} ptr $ fromIntegral $ ((intHash name) .&. mask)
                              {#set visi_event->cmd #} ptr $ enumMe cmd
                              {#set visi_event->eventType #} ptr $ enumMe tpe
                              {#call sendEvent #} objcId ptr
    let figureType name cmd tpe = case tpe of
                                    TPrim PrimDouble -> addIt name cmd DoubleVisiType
                                    TPrim PrimStr -> addIt name cmd StringVisiType
                                    TPrim PrimBool -> addIt name cmd BoolVisiType
                                    _ -> return ()
    let doAction (RemoveSourceAction name) = do
          ptr <- mallocBytes {#sizeof visi_event #}
          {#set visi_event->cmd #} ptr $ enumMe RemoveSourceEvent
          {#set visi_event->targetHash #} ptr $ fromIntegral $ ((intHash name) .&. mask)
          {#call sendEvent #} objcId ptr
        doAction (RemoveSinkAction name) = do
          ptr <- mallocBytes {#sizeof visi_event #}
          {#set visi_event->cmd #} ptr $ enumMe RemoveSinkEvent
          {#set visi_event->targetHash #} ptr $ fromIntegral $ ((intHash name) .&. mask)
          {#call sendEvent #} objcId ptr
        doAction (AddSourceAction src theType) = figureType src AddSourceEvent theType
        doAction (AddSinkAction src theType) = figureType src AddSinkEvent theType
    mapM_ doAction sourceSinkInfo


doSetSinks :: VoidPtr -> [(T.Text, Value)] -> IO ()
doSetSinks objcId nvp = do
  let sendIt (name, value) = case valuePrim value of
                              (Just (TPrim prim)) -> do
                                ptr <- mallocBytes {#sizeof visi_event #}
                                {#set visi_event->cmd#} ptr $ enumMe SetSinkEvent
                                {#set visi_event->targetHash#} ptr $ fromIntegral $ ((intHash name) .&. mask)
                                case (prim, value) of
                                  (PrimDouble, DoubleValue dv) -> do
                                    {#set visi_event->evtValue.number#} ptr $ realToFrac dv
                                    {#set visi_event->eventType#} ptr $ enumMe DoubleVisiType
                                  (PrimStr, StrValue str') -> do
                                    str'' <- newString str'
                                    {#set visi_event->evtValue.text #} ptr str''
                                    {#set visi_event->eventType#} ptr $ enumMe StringVisiType            
                                  (PrimBool, BoolValue bv) -> do
                                    {#set visi_event->evtValue.boolValue#} ptr $ (if bv then 1 else 0)
                                    {#set visi_event->eventType#} ptr $ enumMe BoolVisiType
                                {#call sendEvent #} objcId ptr
                              _ -> return ()
  mapM_ sendIt nvp

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
        return $ Just $ SetValueSource target $ StrValue theStr
      SetSourceCmd | cmdType == DoubleVisiType -> do
        cNum <- {#get visi_command->cmdInfo.number #} what
        target <- getTarget what
        return $ Just $ SetValueSource target $ DoubleValue $ realToFrac cNum
      SetSourceCmd | cmdType == BoolVisiType -> do
        bool <- ({#get visi_command->cmdInfo.boolValue #} what)
        target <- getTarget what
        return $ Just $ SetValueSource target $ BoolValue (bool /= 0)
      StopRunningCmd -> return $ Just StopRunning
      _ -> return $ Nothing

