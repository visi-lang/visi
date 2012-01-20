{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

frog :: CInt -> CInt
frog n = 
	let z = fromIntegral n in
	fromIntegral $ z + 1

data ObjCId_struct
type ObjCId = Ptr ObjCId_struct

data ObjCSEL_struct
type ObjCSEL = Ptr ObjCSEL_struct

foreign import ccall safe "objc_msgSend" objc_msgSend :: ObjCId -> ObjCSEL -> IO ObjCId
foreign import ccall safe "objc_msgSend" objc_msgSendInt :: ObjCId -> ObjCSEL -> Int -> IO ObjCId
foreign import ccall safe "sel_registerName" sel_registerName :: CString -> IO ObjCSEL
foreign import ccall safe "objc_lookUpClass" objc_lookUpClass :: CString -> IO ObjCId


makeAString :: IO ObjCId
makeAString = do
    strCls <- withCString "NSString" objc_lookUpClass 
    allocName <- withCString "alloc" sel_registerName
    alloced <- objc_msgSend strCls allocName
    putStrLn "Alloced it"
    initName <- withCString "init" sel_registerName 
    objc_msgSend alloced initName

main = 
	do
		makeAString
		putStrLn "I'm the Haskell Objective-C Extentions"