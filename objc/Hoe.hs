{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.Directory
import Data.List
import Text.Regex.Posix
import System.IO
import Control.Exception

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

-- rootDir = "/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.0.sdk/System/Library/Frameworks/"
rootDir = "/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS4.2.sdk/System/Library/Frameworks/"

fileList filterFunc todo root = 
  if (isSuffixOf "/." root) || (isSuffixOf "/.." root) then
    return []
  else
    do
      isFile <- doesFileExist root
      if isFile then
        do 
          ret <- mapM todo $ filter filterFunc [root]
          return ret -- $ length ret `seq` ret
      else
        do
          isDir <-  doesDirectoryExist root
          if isDir then
            do
              path <- canonicalizePath root
              kids' <- getDirectoryContents path
              let kids = map (\name -> path ++ "/" ++ name) kids'
              subPaths <- mapM (fileList filterFunc todo) kids
              let ret = concat subPaths
              return ret -- $ length ret `seq` ret

          else
            return []

findThem :: String -> [String]
findThem str =
  case str =~ "\\@interface +([A-Za-z0-9]+) +\\:" :: (String, String, String, [String]) of
    (_, _, _, ret) -> ret

hGetContents' h  = hGetContents h >>= \s -> length s `seq` return s

readFile' name =
  do
    h <- openFile name ReadMode
    hSetEncoding h latin1
    hGetContents' h

loadAndFind fileName =
  do
    contents <- readFile' $! fileName
    let ret = (findThem $! contents)
    -- putStrLn $ "Found " ++ show ret
    return ret

allClassNames =
  do
    res <- fileList (isSuffixOf ".h") (loadAndFind $!) rootDir
    let ret = concat res
    return $ length ret `seq` ret
{-
allClassNames =
  do
    files <- fileList (isSuffixOf ".h") rootDir
    programText <- mapM readFile files
    let matches = concat $ map findThem programText
    putStrLn $ "found: " ++ show matches
-}


makeAString :: IO ObjCId
makeAString = do
    strCls <- withCString "NSString" objc_lookUpClass 
    allocName <- withCString "alloc" sel_registerName
    alloced <- objc_msgSend strCls allocName
    putStrLn $ "Alloced it " ++ show alloced
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

getClass name =
  do
    clz <- withCString name objc_lookUpClass
    putStrLn $ "Looking for "++ name ++ " got " ++ show clz

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
    names <- allClassNames
    putStrLn $ "Names " ++ show names
    mapM_ getClass names
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