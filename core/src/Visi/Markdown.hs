module Visi.Markdown (markdown) where

{-
Copyright (c) 2003-2004 John Gruber
<http://daringfireball.net/>
Copyright (c) 2012 David Pollak
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

* Neither the name "Markdown" nor the names of its contributors may
  be used to endorse or promote products derived from this software
  without specific prior written permission.

This software is provided by the copyright holders and contributors "as
is" and any express or implied warranties, including, but not limited
to, the implied warranties of merchantability and fitness for a
particular purpose are disclaimed. In no event shall the copyright owner
or contributors be liable for any direct, indirect, incidental, special,
exemplary, or consequential damages (including, but not limited to,
procurement of substitute goods or services; loss of use, data, or
profits; or business interruption) however caused and on any theory of
liability, whether in contract, strict liability, or tort (including
negligence or otherwise) arising in any way out of the use of this
software, even if advised of the possibility of such damage.
-}

import Text.Regex.PCRE
import Data.Array
import Visi.Util
import Control.Monad.State.Lazy
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (toString, fromString)
import Data.Tuple (swap)

type LocalState = State (M.Map B.ByteString B.ByteString)

escChars = map fromString $ map (\c -> [c]) "\\`*_{}[]()#+-.!"
escList = map (\s -> (s, fromString $ hexHash s)) escChars

htmlList = map (\(a,b) -> (fromString a, fromString b)) [(">", "&gt;"), ("<", "&lt;"), ("&", "&amp;")]

escapeMap = M.fromList $ htmlList ++ escList

buildDefaultMap = M.fromList $ map swap escList

markdown :: String -> String
markdown s = (toString (evalState (xform $ fromString s) buildDefaultMap)) ++ "\n"


xform s = (return s) >>= (gsub "\r\n" "\n") >>= (gsub "\r" "\n") >>=
                (\s -> return $ s `B.append` doubleNewline) >>= eachLine scrubTabs >>=
                hashHtmlBlocks >>=
                stripLinkDefinitions >>=
                runBlockGamut >>=
                unescapeSpecialChars


blockTags_a = "p|div|h[1-6]|blockquote|pre|table|dl|ol|ul|script|noscript|form|fieldset|iframe|math|ins|del"
blockTags_b = "p|div|h[1-6]|blockquote|pre|table|dl|ol|ul|script|noscript|form|fieldset|iframe|math"
lessThanTab = tabWidth - 1

firstHtmlMatch = "(^<("++ blockTags_a ++ ")\\b(.*\n)*?</\\2>[ \t]*(?=\n+|\\Z))"
secondHtmlMatch = "(^<("++ blockTags_b ++ ")\\b(.*\n)*?.*</\2>[ \t]*(?=\n+|\\Z))"
hrHtmlMatch = "(?:(?<=\n\n)|\\A\n?)([ ]{0,"++(show lessThanTab)++"}<(hr)\\b([^<>])*?/?>[ \t]*(?=\n{2,}|\\Z))"
commentHtmlMatch = "(?:(?<=\n\n)|\\A\n?)([ ]{0,"++(show lessThanTab)++"}(?s:<!(--.*?--\\s*)+>)[ \t]*(?=\n{2,}|\\Z))"


doubleNewline = fromString "\n\n"
singleNewline = fromString "\n"

hashHtmlBlocks s = (return s) >>= gsub firstHtmlMatch subber >>=
  gsub secondHtmlMatch subber >>= gsub hrHtmlMatch subber >>= gsub commentHtmlMatch subber
  where
    subber :: B.ByteString -> LocalState B.ByteString
    subber str = do
                    let digest = fromString $ hexHash str
                    st <- get
                    put $ M.insert digest str st
                    return $ B.concat [doubleNewline, digest, doubleNewline]



stripLinkDefinitions s = return s -- FIXME

runBlockGamut ::  B.ByteString -> LocalState B.ByteString
runBlockGamut s = (return s) >>= doHeaders >>= gsub "^[ ]{0,2}([ ]?\\*[ ]?){3,}[ \t]*$\n" "\n<hr>\n" >>=
  gsub "^[ ]{0,2}([ ]? -[ ]?){3,}[ \t]*$\n" "\n<hr>\n" >>=
  gsub "^[ ]{0,2}([ ]? _[ ]?){3,}[ \t]*$\n" "\n<hr>\n" >>=
  doLists >>= doCodeBlocks >>= doBlockQuotes >>= hashHtmlBlocks >>= formParagraphs

doLists s = return s -- FIXME

doBlockQuotes s = return s -- FIXME



endPara = fromString "</p>\n\n"

paragraphReplace :: B.ByteString -> LocalState B.ByteString
paragraphReplace s = split "\n{2,}" doParagraph s
  where doParagraph :: B.ByteString -> LocalState B.ByteString
        doParagraph str = do
                            st <- get
                            case M.lookup str st of
                              Just repl -> return $ repl `B.append` doubleNewline
                              _ -> (return str) >>= runSpanGamut >>= sub1 "^([ \t])*" "<p>" >>= (\s -> return $ s `B.append` endPara)

formParagraphs s = (return s) >>= gsub "\\A\n+" "" >>= gsub "\n+\\z" "" >>= paragraphReplace

runSpanGamut :: B.ByteString -> LocalState B.ByteString
runSpanGamut s = (return s) >>= doCodeSpans >>= doEscapeSpecialChars >>=
                   doImages >>= doAnchors >>= doAutoLinks >>=  encodeAmpsAndAngles >>= doItalicsAndBold >>=
                    gsub " {2,}\n" "<br>"

doCodeSpans = return -- FIXME

doEscapeSpecialChars = return -- FIXME

doImages = return -- FIXME

doAnchors = return -- FIXME

doAutoLinks = return -- FIXME

encodeAmpsAndAngles s = return s >>= gsub "&(?!#?[xX]?(?:[0-9a-fA-F]+|\\w+);)" "&amp;" >>= gsub "<(?![a-zA-Z/?\\$!])" "&lt;"

doItalicsAndBold s = (return s) >>=
  gsub "(\\*\\*|__) (?=\\S) (.+?[*_]*) (?<=\\S) \1" (\s -> B.concat [preStrong, s, postStrong]) >>=
  gsub "(\\*|_) (?=\\S) (.+?) (?<=\\S) \1" (\s -> B.concat [preEm, s, postEm])

preStrong = fromString "<strong>"
postStrong = fromString "</strong>"

preEm = fromString "<em>"
postEm = fromString "</em>"

doHeaders s = return s -- FIXME

doCodeBlocks s = gsub ("(?:\n\n|\\A)((?:(?:[ ]{" ++ show tabWidth ++ "}|\t).*\n+)+)((?=^[ ]{0," ++ show tabWidth ++ "}\\S)|\\Z)") handleCode s
  where handleCode :: (B.ByteString, MatchArray, [(Int, Int)]) -> LocalState B.ByteString
        handleCode (str, ma, pairs) = return str >>= encodeCode >>= detab >>= gsub "\\A\n+" "" >>= gsub "\\s+\\z" "" >>=
                                        (\s -> return $ B.concat [preStart, s, preEnd])

detab = eachLine $ sub1 "^(\t|    )" ""

preStart = fromString "\n\n<pre><code>"
preEnd = fromString "\n</code></pre>\n\n"

encodeCode s = gsub "(&|>|<|\\*|_|{|}|\\[|\\]\\\\)" doCharThing s
  where doCharThing str = escapeMap M.! str

replaceThem str (k,v) = gsub (toString k) v str

unescapeSpecialChars s =
  do
    chars <- get
    foldM replaceThem s $ M.assocs chars

tabWidth = 4

scrubTabs :: B.ByteString -> B.ByteString
scrubTabs s =
  if s =~ "^[ \t]*$\n" then singleNewline
    else tabByTab s

grabSpaces pos = fromString "    "

tabByTab :: B.ByteString -> B.ByteString
tabByTab s =
  case elems (s =~ "(.*?)\t" :: MatchArray) of
    (start, len):(_, ms):_ -> tabByTab $ B.concat [(before ms s),
                                                  grabSpaces ms,
                                                  (after (start + len) s)]
    _ -> s

eachLine f = gsub "^.*$\n" runLine
  where runLine s = (f s)

split :: AppFunc a => String -> a -> B.ByteString -> LocalState B.ByteString
split regex f str =
  if B.null str then return B.empty
    else
         let ma = (str =~ regex :: MatchArray) in
         case elems $ ma of
          (0,0):_ -> return str
          whole@(pos@(start,len):_) -> do
            retStr <- applyFunc f (before start str, ma, whole)
            endStr <- split regex f (after (start + len) str)
            return $ retStr `B.append` endStr
          _ -> return str

sub1 :: AppFunc a => String -> a -> B.ByteString -> LocalState B.ByteString
sub1 regex f str =
  if B.null str then return B.empty
      else
          let ma = (str =~ regex :: MatchArray) in
          case elems $ ma of
            whole@(pos@(start,len):_) -> do
              retStr <- applyFunc f (extract pos str, ma, whole)
              let endStr = after (start + len) str
              return $ B.concat [(before start str), retStr, endStr]
            _ -> return $ str

gsub :: AppFunc a => String -> a -> B.ByteString -> LocalState B.ByteString
gsub regex f str =
  if B.null str then return B.empty
      else
          let ma = (str =~ regex :: MatchArray) in
          case elems $ ma of
            (0,0):_ -> return $ str
            whole@(pos@(start,len):_) -> do
              retStr <- applyFunc f (extract pos str, ma, whole)
              endStr <- gsub regex f $ after (start + len) str
              return $ B.concat [(before start str), retStr, endStr]
            _ -> return $ str

class AppFunc a where
  applyFunc :: a -> (B.ByteString, MatchArray, [(Int, Int)]) -> LocalState B.ByteString

instance AppFunc (B.ByteString -> B.ByteString) where
  applyFunc f (s, _, _) = return $ f s

instance AppFunc String where
  applyFunc a s = return $ fromString a

instance AppFunc B.ByteString where
  applyFunc a s = return $ a

instance AppFunc ([(Int, Int)] -> B.ByteString) where
  applyFunc f (s, ma, whole) = return $ f whole

instance AppFunc ((B.ByteString, MatchArray, [(Int, Int)]) -> LocalState B.ByteString) where
  applyFunc f v = f v

instance AppFunc (B.ByteString -> LocalState B.ByteString) where
  applyFunc f (s, _, _) = (f s)



