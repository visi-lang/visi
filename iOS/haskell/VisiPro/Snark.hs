{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}


module VisiPro.Snark where

import Foreign.Ptr
import Foreign.C
-- import Control.Concurrent
import Visi.Runtime
import Visi.Expression
import qualified Data.Text as T

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc


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

{-
  The interface between the Visi runtime and the external Objective-C system.
  It's a snarky pile of code... but it works
-}

-- Shared defines between Haskell-land and ObjC-land

#define BoolSourceType 1
#define BoolSourceStr "BOOL"
#define StringSourceType 2
#define StringSourceStr "STR"
#define NumberSourceType 3
#define NumberSourceStr "NUM"

#define DisplayErrorCmd 1
#define BeginProgramInfoCmd 2
#define EndProgramInfoCmd 3
#define SourceInfoCmd 4
#define SinkInfoCmd 5
#define SetSinkCmd 6

type PVP = Ptr VoidPtr

foreign import ccall safe "objc_getClassList" objc_getClassList :: Ptr VoidPtr -> CInt -> IO CInt
foreign import ccall safe "class_copyMethodList" class_copyMethodList :: VoidPtr -> VoidPtr -> IO PVP
foreign import ccall safe "class_getName" class_getName :: VoidPtr -> IO CString
type VoidPtr = Ptr ()

foreign export ccall releaseMe :: FunPtr a -> IO ()
releaseMe :: FunPtr a -> IO ()
releaseMe ptr = freeHaskellFunPtr ptr

type VoidStar = Ptr ()

foreign import ccall safe "sendInfoBack" sendInfoBack :: VoidStar -> CInt -> CString -> CString -> IO ()

sendInfo :: VoidStar -> Int -> T.Text -> T.Text -> IO ()
sendInfo void cmd name value = withCString (T.unpack name) runName
                               where runName name' = withCString (T.unpack value) runValue
                                        where runValue value' = sendInfoBack void (fromIntegral cmd) name' value'

foreign export ccall setProgramText :: VoidStar -> CString -> IO ()
-- | Set the text of the program
setProgramText :: VoidStar -> CString -> IO ()
setProgramText void text = 
    do
        text' <- peekCString text
        runApp text' $ AppCallback (errorCallback void) (sourceSinkCallback void) (setSinksCallback void)
              
errorCallback void errMsg = sendInfo void DisplayErrorCmd errMsg errMsg

blank = T.pack ""

sourceSinkCallback void info =
                      do
                        sendInfo void BeginProgramInfoCmd blank blank
                        mapM_ (sendSourceSinkInfo void) info
                        sendInfo void EndProgramInfoCmd blank blank          

setSinksCallback :: VoidStar -> [(T.Text, Value)] -> IO ()
setSinksCallback void info = mapM_ (sendSinks void) info
                  
sendSinks void (name, (DoubleValue value)) = sendInfo void SetSinkCmd name $ T.pack $ show value
sendSinks void (name, (StrValue value)) = sendInfo void SetSinkCmd name value
sendSinks void (name, (BoolValue value)) = sendInfo void SetSinkCmd name $ T.pack $ show value
sendSinks void (name, _) = sendInfo void SetSinkCmd name $ T.pack "N/A"

theTypeToStr PrimDouble =  T.pack NumberSourceStr
theTypeToStr PrimBool =  T.pack BoolSourceStr
theTypeToStr PrimStr =  T.pack StringSourceStr
sendSourceSinkInfo void (SourceInfo name theType) = sendInfo void SourceInfoCmd name $ theTypeToStr theType
sendSourceSinkInfo void (SinkInfo name theType) =sendInfo void SinkInfoCmd name $ theTypeToStr theType


foreign export ccall setSourceString :: CString -> CInt -> CString -> IO ()
-- | Set a String source value
setSourceString :: CString -> CInt -> CString -> IO ()
setSourceString name theType value = 
    do
        -- classList
        name'' <- peekCString name
        value'' <- peekCString value
        let name' = T.pack name''
        let value' = T.pack value''
        doSetSource name' (fromIntegral theType) value'

classList :: IO ()
classList =
    do
      putStrLn "Here"
      cnt' <- objc_getClassList nullPtr 0
      let cnt = fromIntegral cnt'
      buffer <- mallocBytes $ cnt * 8
      putStrLn "Malloced"
      objc_getClassList buffer cnt'
      putStrLn "About to map"
      mapM_ (showClass buffer) [0 .. (cnt - 1)]
      free buffer

ptrSize = sizeOf (nullPtr :: VoidPtr)

showClass buffer pos = 
  do
    ptr <- peekElemOff buffer pos 
    cstr <- class_getName ptr
    str <- peekCString cstr
    putStrLn $ "Name: " ++ str
    showMethods ptr

showMethods :: VoidPtr -> IO ()
showMethods classPtr =
  do
    outCount <- mallocBytes 64
    methods <- class_copyMethodList classPtr outCount
    showMethod methods
    free outCount
    free methods

showMethod :: PVP -> IO ()
showMethod meth =
  if nullPtr == meth then
    return ()
    else
      do
        dereffed <- peekElemOff meth 0
        if nullPtr == dereffed then
            return ()
          else
            do
              putStrLn "hi"
              let next = plusPtr meth ptrSize
              showMethod next


doSetSource :: T.Text -> Int -> T.Text -> IO ()
doSetSource name theType value = 
    let val = case theType of
                BoolSourceType -> if (value == (T.pack "t")) then BoolValue True else BoolValue False
                StringSourceType -> StrValue value
                NumberSourceType -> case reads $ T.unpack value of
                                      [] -> UndefinedValue
                                      [(num, _)] -> DoubleValue num
          in
    setSource name val

