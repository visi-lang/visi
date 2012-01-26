{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}


import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

#include "testly.h"

sizeofclass = #size Class


-- isa x = #peek isa x

main = putStrLn "Nothing to see here"