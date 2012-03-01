module Visi.Util (flatten, VisiError(TypeError, ParsingError), ThrowsError, vtrace, justOr, (|-)) where

import Control.Monad.Error
import Text.Parsec
import Debug.Trace

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

-- | flatten a List
flatten :: [[a]] -> [a]
flatten = concat

-- | Trace that you can disable
vtrace :: String -> a -> a
-- vtrace _ a = a
vtrace msg a = trace msg a

data VisiError = 
    TypeError String 
    | ParsingError ParseError
    | DefaultError String
    -- deriving (Eq)

type ThrowsError = Either VisiError

-- | Convert the error to a String
showError :: VisiError -> String
showError (TypeError str) = "Type error: " ++ str
showError (DefaultError str) = "Uncategorized error: " ++ str
showError (ParsingError pe) = show pe

instance Show VisiError where show = showError

instance Error VisiError where
    noMsg = DefaultError "An error has occurred"
    strMsg = DefaultError
    
justOr thing err = case thing of
                    (Just v) -> return v
                    _ -> err
                    
(|-) :: Either b a -> (a -> Either b c) -> Either b c
(Right a) |- f = f a
(Left b) |- _ = Left b