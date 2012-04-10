module VisiPro.Mac where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import VisiPro.Bridge
import Visi.Runtime

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

import Visi.Util
import Visi.Typer
import Visi.Runtime
import Visi.Parse
import Visi.Executor
import Visi.Expression
import VisiPro.Bridge

type VoidPtr = Ptr ()

foreign import ccall safe "afterHaskellmain" afterHaskellmain :: CInt -> VoidPtr -> IO CInt

-- foreign export ccall newProcess :: VoidPtr -> IO ()

-- | Initialize the Haskell runtime and call back into the ObjC code
foreign export ccall haskellMain :: CInt -> VoidPtr -> IO CInt
haskellMain :: CInt -> VoidPtr -> IO CInt
haskellMain argc argv = 
    do
      ret <- afterHaskellmain argc argv
      return ret

foreign export ccall freeFunPtr :: FunPtr (VoidPtr -> IO ()) -> IO ()
freeFunPtr ptr = freeHaskellFunPtr ptr

foreign import ccall "wrapper"
  wrapIt :: (VoidPtr -> IO ()) -> IO (FunPtr (VoidPtr -> IO ()))

foreign export ccall startProcess :: VoidPtr -> IO (FunPtr (VoidPtr -> IO ()))
startProcess objcId =
    do
        handler <- runApp $ AppCallback (doError objcId) (doSourceSink objcId) (doSetSinks objcId)
        wrapIt $ handleMsg  handler

handleMsg handler msg =
    do
        myMsg <- convertFromC msg
        case myMsg of
          Just myMsg' -> handler myMsg'
          _ -> return ()
