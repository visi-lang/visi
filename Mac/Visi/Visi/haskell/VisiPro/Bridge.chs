module VisiPro.Bridge where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include "Test.h"


#c
typedef struct test test_t;
#endc

{-
{#pointer *test_t as Test#}

getA :: Test -> IO Int
getA t = {#get test_t->a#} t >>= return . fromIntegral

setA :: Test -> Int -> IO ()
setA t i = {#set test_t->a#} t (fromIntegral i)

-}