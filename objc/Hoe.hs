{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

frog :: CInt -> CInt
frog n = 
	let z = fromIntegral n in
	fromIntegral $ z + 1

data ObjCId_struct
type ObjCId = Ptr ObjCId_struct

data ObjCSEL_struct
type ObjCSEL = Ptr ObjCSEL_struct

type VoidPtr = Ptr ()

foreign import ccall safe "objc_msgSend" objc_msgSend :: ObjCId -> ObjCSEL -> IO ObjCId
foreign import ccall safe "objc_msgSend" objc_msgSendInt :: ObjCId -> ObjCSEL -> Int -> IO ObjCId
foreign import ccall safe "sel_registerName" sel_registerName :: CString -> IO ObjCSEL
foreign import ccall safe "objc_lookUpClass" objc_lookUpClass :: CString -> IO ObjCId
foreign import ccall safe "objc_getClassList" objc_getClassList :: Ptr VoidPtr -> CInt -> IO CInt
foreign import ccall safe "class_getName" class_getName :: VoidPtr -> IO CString


makeAString :: IO ObjCId
makeAString = do
    strCls <- withCString "NSString" objc_lookUpClass 
    allocName <- withCString "alloc" sel_registerName
    alloced <- objc_msgSend strCls allocName
    putStrLn "Alloced it"
    initName <- withCString "init" sel_registerName 
    objc_msgSend alloced initName

showClass buffer pos = 
  do
    putStrLn "One"
    ptr <- peekElemOff buffer pos 
    putStrLn "Two"
    cstr <- class_getName ptr
    putStrLn "Three"
    str <- peekCString cstr
    putStrLn $ "Name " ++ str ++ " at " ++ show pos

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

main = 
	do
    makeAString
    classList
    putStrLn "I'm the Haskell Objective-C Exchange"



{-

instance Storable Bar where
    sizeOf _ = ((8))
{-# LINE 47 "Hoe.hsc" #-}
    alignment _ = alignment (undefined :: CInt)
    peek _ = error "peek is not implemented"
    poke ptr (Bar a' b') = 
        do
            ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr a'
-}