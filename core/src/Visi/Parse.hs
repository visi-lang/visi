module Visi.Parse (parseLine, parseLines, mkGroup) where

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
 * Portions created by the Initial Developer are Copyright (C) 2011-2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** -}

import Control.Applicative ((<$>),(<*>),(*>),(<*),(<$))
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Data.Text as T
import Data.Char ( isAlpha, toLower, toUpper, isSpace, digitToInt )
import Data.List ( nub, sort, foldl' )
import Data.Maybe ( catMaybes )
import Visi.Util
import Visi.Expression
import qualified Data.Map as Map
import Data.Array

data TIState = TIState {tiSupply :: Int, tiDepth :: Int, 
                        tiSourceUrl :: Maybe String,
                        tiSource :: (Array Int Int, T.Text),
                        tiHasLitCode :: Bool, tiInLiterate :: Bool} deriving (Show)
type MParser = Parsec String TIState

textLines str =
  let txt = T.pack str in
  let noCrLf = T.replace (T.pack "\r\n") (T.pack "\n") txt in
  let noCr = T.replace (T.pack "\r") (T.pack "\n") noCrLf in
  let doChar (pos, offsets) char = if char == '\n' then (pos + 1, (pos + 1):offsets) else (pos + 1, offsets) in
  let (_,offsets) = T.foldl' doChar (0, [0]) noCr in
  let revOff = reverse offsets in
  let offArray = listArray (0, length revOff) revOff in
  (offArray, noCr)


-- | parse a line of input
parseLine :: String -> Either VisiError Expression
parseLine str = case runParser line TIState {tiSupply = 0, tiDepth = 0, 
                                             tiSourceUrl = Nothing,
                                             tiSource = textLines str,
                                             tiHasLitCode = False, tiInLiterate = False} str str of
                  Left err -> Left $ ParsingError err
                  Right res -> Right res

-- | parse many lines of input and return a List of expressions
parseLines :: String -> Either VisiError [Expression]
parseLines str = case runParser doLines TIState{tiSupply = 0, tiDepth = 0,
                                                tiSourceUrl = Nothing,
                                                tiSource = textLines str,
                                                tiHasLitCode = False, tiInLiterate = True} str str of
                    Left err -> Left $ ParsingError err
                    Right res -> Right res
                    

mkGroup :: [Expression] -> Expression
mkGroup expLst = Group (foldr (\a -> \b -> getSourceLoc a) NoSourceLoc expLst) (Map.fromList $ expLst >>= funcName) (TPrim PrimDouble) 
                       (ValueConst NoSourceLoc $ DoubleValue 1.0)

funcName :: Expression -> [(FuncName, Expression)]
funcName exp@(LetExp _ _ name _ _ _) = [(name, exp)]
funcName exp@(BuiltIn _ name _ _) = [(name, exp)]
funcName exp@(SinkExp _ _ name _ _) = [(name, exp)]
funcName exp@(SourceExp _ _ name _) = [(name, exp)]
funcName _ = []


visiDef = emptyDef{ commentStart = "/*"
                  , commentEnd = "*/"
                  , commentLine = "//"
                  , caseSensitive = True
                  , nestedComments = True
                  , identStart = letter <|> char '_'
                  , identLetter = alphaNum <|> char '_'
                  , opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                  , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                  , reservedOpNames = []
                  , reservedNames = ["if", "then", "else", "struct"]
                  }

TokenParser{ parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , semiSep1 = m_semiSep1
            , stringLiteral = m_stringLiteral
            , whiteSpace = m_whiteSpace } = m_makeTokenParser visiDef


line :: MParser Expression
line = do info <- try dataDefinition <|> try funcDef <|> try letDef
          mySpaces
          try eol <|> try eof
          return info

narc id = id

getHasLitCode = tiHasLitCode <$> getState

setHasLitCode b =
  do
    st <- getState
    setState st{tiHasLitCode = b}

inLiterate = tiInLiterate <$> getState

setInLiterate b =
  do
    st <- getState
    setState st{tiInLiterate = b}


testForLitPair =
  do
    findTicks
    findTicks    
    return True

toCol1 :: MParser ()
toCol1 =
  eof <|> testCol1
  where testCol1 = do
                    col <- curColumn
                    if col == 1 then return () else anyChar >> toCol1

findTicks :: MParser ()
findTicks =
  do
    col <- curColumn
    if col == 1 then (try threeMarksToEnd) <|> (anyChar >> findTicks) else anyChar >> findTicks

findTicksOrEOF :: MParser ()
findTicksOrEOF = eof <|>
  do
    col <- curColumn
    if col == 1 then (try threeMarksToEnd) <|> (anyChar >> findTicksOrEOF) else anyChar >> findTicksOrEOF

threeMarksToEnd =
  do
    threeMarks <?> "Three markes to end"
    toCol1


findCodeBlockLine :: MParser ()
findCodeBlockLine =
  do
    col <- curColumn
    if col == 1 then threeMarksToEnd else anyChar >> findCodeBlockLine

whileNotEnd = whileNot (eol <|> eof)

testForLiterate =
  do
    which <- lookAhead (try testForLitPair) <|> return False
    setHasLitCode which




doLines = testForLiterate >> blankLines >> stmtparser <* blankLines <* eof
    where
      stmtparser :: MParser [Expression]
      stmtparser = many $ blankLines >> line <* blankLines

mySpaces = try m_whiteSpace

tryEnd = try (eol <|> eof)

threeMarks :: MParser ()
threeMarks =
  do
    char '`'
    char '`'
    char '`'
    return ()

blankLine :: MParser ()
blankLine =
  do
    hasLit <- getHasLitCode
    inLit <- inLiterate
    if not hasLit then (try mySpaces >> eol) <?> "Blank Line"
      else
        if inLit then (try findTicksOrEOF >> setInLiterate False) 
        else eof <|>
             (try findCodeBlockLine >> setInLiterate True) <|>
             (try mySpaces >> tryEnd) <?> "Literate Sep Line or Blank Line"


whileNot end =  scan
                where scan  = () <$ lookAhead (try end)
                                 <|>
                              (anyChar >> scan)

blankLines :: MParser ()
blankLines = eof <|> optional (try blankLine >> optional blankLines)

eol :: MParser ()
eol = do char '\n'
         toGrab <- curDepth
         consumeN (toGrab - 1) $ char ' '

consumeN n _ | n <= 0 = return ()
consumeN n exp = exp *> consumeN (n - 1) exp

curDepth = tiDepth <$> getState

funcDef :: MParser Expression
funcDef = try sinkFunc <|> try normalFunc <|> try sourceFunc <?> "Function Definition"

-- | An upper case character followed by an identifier
typeName = (:) <$> upper <*> [] `option` m_identifier

-- | a type parameter
typeParam = (:) <$> (mySpaces *> lower)
                <*> [] `option` m_identifier <* mySpaces

-- | a list of type parameters
typeParams = many typeParam

typeWithOptionalParams = typeName <* mySpaces
                                  <* many typeOrTypeParam
                                  <* mySpaces

-- | get a type with optional type parameters or an identifier
typeOrTypeParam =
  typeWithOptionalParams <|> m_identifier

structDataDef =
  do
    mySpaces
    argName <- typeParam
    mySpaces
    char ':'
    mySpaces
    typeName <- typeOrTypeParam
    mySpaces

structParams =
  do
    char '('
    params <- sepBy1 structDataDef (char ',')
    mySpaces
    char ')'
    mySpaces
    return params

-- | the inner part of a struct definition
structInner =
  do
    mySpaces
    name <- typeName
    params <- option [] structParams
    return ()


-- | Did we find a 
dataDefinition =
  do
    startPos <- currentPos
    m_reserved "struct"
    mySpaces
    name <- typeName
    tparams <- typeParams <?> "Type parameters"
    mySpaces
    char '='
    defs <- sepBy1 structInner (char '|')
    sourceLoc <- calcSourceLoc startPos
    return $ ValueConst sourceLoc $ BoolValue False -- FIXME finish data definition

sourceOrSinkName = try m_identifier <|> try m_stringLiteral

sinkFunc =
    do
      startPos <- currentPos
      sinkName <- m_stringLiteral
      mySpaces
      char '='
      mySpaces
      exp <- expressionOrLetAndExp
      mySpaces
      sourceLoc <- calcSourceLoc startPos
      rt <- newTyVar "Sink"
      letId <- newLetId "SinkLet"
      return $ SinkExp sourceLoc letId (FuncName $ T.pack sinkName) rt exp

consumeUntilNotWhitespaceOrEOL :: MParser ()
consumeUntilNotWhitespaceOrEOL = try $ consumeUntilNotWhitespaceOrEOL' <|> mySpaces

consumeUntilNotWhitespaceOrEOL' :: MParser ()
consumeUntilNotWhitespaceOrEOL' =
  do
    mySpaces
    eol
    many consumeUntilNotWhitespaceOrEOL
    mySpaces

expressionOrLetAndExp :: MParser Expression
expressionOrLetAndExp = 
  do
    consumeUntilNotWhitespaceOrEOL
    col <- curColumn
    dep <- curDepth
    if col > dep 
      then runDepth col (letAndThenExp col <|> expression <?> "Looking for let + exp or exp") 
      else parserFail "Incorrect indentation"

letAndThenExp atCol =
  do
    startPos <- currentPos
    letExp <- try normalFunc <|> try letDef
    try eol
    consumeUntilNotWhitespaceOrEOL
    curCol <- curColumn
    expr <- if curCol /= atCol 
            then parserFail "Expressions not lined up"
            else try (letAndThenExp atCol) <|> expression
    tpe <- newTyVar "innerlet"
    sourceLoc <- calcSourceLoc startPos
    return $ InnerLet sourceLoc tpe letExp expr



sourceFunc =
    do
      startPos <- currentPos
      char '?'
      sourceName <- m_identifier
      mySpaces
      tpe <- newTyVar "source"
      letId <- newLetId "SourceLet"
      seLetId <- newLetId "SourceExp"
      sourceLoc <- calcSourceLoc startPos
      return $ LetExp sourceLoc letId (FuncName $ T.pack sourceName) False tpe $ SourceExp sourceLoc seLetId (FuncName $ T.pack sourceName) tpe

normalFunc = do 
              startPos <- currentPos
              name <- m_identifier
              param <- many1 m_identifier
              mySpaces
              char '='
              mySpaces
              exp <- expressionOrLetAndExp
              mySpaces
              rt <- newTyVar "r"
              letId <- newLetId "normal"
              sourceLoc <- calcSourceLoc startPos
              pTypes <- mapM makeVars param
              let foldFunc (funcName, pt) (exp, rt) = (FuncExp sourceLoc funcName pt exp, tFun pt rt)
              let (wholeExp, ft) = foldr foldFunc (exp, rt) pTypes
              return $ LetExp sourceLoc letId (FuncName $ T.pack name) True ft wholeExp
              where makeVars param = do pt <- newTyVar "p"
                                        return (FuncName $ T.pack param, pt)
                    

letDef :: MParser Expression
letDef = 
  do
    startPos <- currentPos
    curCol <- curColumn
    name <- m_identifier
    mySpaces
    char '='
    mySpaces
    exp <- expressionOrLetAndExp
    mySpaces
    t1 <- newTyVar "L"
    letId <- newLetId "Let"
    sourceLoc <- calcSourceLoc startPos
    return $ LetExp sourceLoc letId (FuncName $ T.pack name) False t1 exp

buildType t = tFun (TPrim PrimBool) $ ifType t
ifType t = tFun t $ tFun t t


functionInvocationName =
  try m_identifier <|> try (('#':) <$> (char '#' *> m_identifier))
                   <|> try (("#=" ++) <$> (char '#' *> char '=' *> m_identifier))
  {- <|> try(
    do
      var <- m_identifier
      char '.'
      prop <- m_identifier
      return var -- FIXME compound thingy
  ) -}


expression = 
   try(ifElseExp) <|>
   try(oprFuncExp) <|>
   try(term) <?> "Looking for an expression"
   where 
    term =
      try(funcParamExp) <|>
      try(methodInv) <|>
      try(identifier) <|>
      try(parenExp) <|>
      try(constExp) <?> "Looking for Term"
    funcParamExp = 
      do
        startPos <- currentPos
        funcExp <- try(parenExp) <|> try(methodInv) <|> try(identifier)
        mySpaces
        rest <- many1(try(parenExp) <|> try(ifElseExp) <|> try(methodInv) <|> try(identifier)  <|> try(constExp) <?> "parameter")
        mySpaces
        restWithVars <- mapM makeVars rest
        letId <- newLetId "func"
        sourceLoc <- calcSourceLoc startPos
        let buildApply exp (exp2, t2) =  Apply sourceLoc letId t2 exp exp2
        return $ foldl' buildApply funcExp restWithVars
        where makeVars exp = do t2 <- newTyVar "RetType"
                                return (exp, t2)
    parenExp = 
      do
        mySpaces
        char '('
        mySpaces
        exp <- expression
        mySpaces
        char ')'
        mySpaces
        return exp
    constExp =
      do
        mySpaces
        ret <- try(strConstExp) <|> try(numConstExp) <?> "Constant"
        mySpaces
        return ret
    strConstExp = 
      do
        startPos <- currentPos
        str <- m_stringLiteral
        sourceLoc <- calcSourceLoc startPos
        return $ ValueConst sourceLoc $ StrValue $ T.pack str
    decMore = (:) <$> char '.' <*> many1 (oneOf "0123456789")
    numConstExp = 
      do
        mySpaces
        startPos <- currentPos
        optMin <- optionMaybe(char '-')
        digits <- many1 $ oneOf "0123456789"
        optDec <- optionMaybe $ decMore
        sourceLoc <- calcSourceLoc startPos
        mySpaces
        return $ ValueConst sourceLoc $ DoubleValue $ 
          read $ case (optMin, digits, optDec) of
                   (Nothing, d, Nothing) -> d
                   (Just(_), d, Nothing) -> '-' : d
                   (Just(_), d, Just(rest)) -> '-' : (d ++ rest)
                   (_, d, Just(rest)) -> (d ++ rest)
    identifier = 
      do
        mySpaces
        startPos <- currentPos
        ret <- try functionInvocationName
        sourceLoc <- calcSourceLoc startPos
        mySpaces
        return $ Var sourceLoc $ FuncName $ T.pack ret
    methodInv =
      do
        startPos <- currentPos
        target <- try(parenExp) <|> try(identifier) <|> try(constExp)
        mySpaces
        char '.'
        mySpaces
        sourceLoc <- calcSourceLoc startPos
        (Var _ method@(FuncName methText)) <- identifier
        letId <- newLetId $ "Method_" ++ T.unpack methText
        retType <- newTyVar $ "Method_" ++ T.unpack methText
        let targetType = StructuralType (Map.singleton methText retType)
        return $ Apply sourceLoc letId retType (InvokeMethod sourceLoc letId method $ tFun targetType retType) target
    ifElseExp = 
      do
        startPos <- currentPos
        m_reserved "if"
        mySpaces
        e1s <- currentPos
        boolExp <- expression
        e1l <- calcSourceLoc e1s
        mySpaces
        m_reserved "then"
        mySpaces
        e2s <- currentPos
        trueExp <- expression
        e2l <- calcSourceLoc e2s
        mySpaces
        m_reserved "else"
        mySpaces
        e3s <- currentPos
        falseExp <- expression
        e3l <- calcSourceLoc e3s
        sourceLoc <- calcSourceLoc startPos
        mySpaces
        theType <- newTyVar "IfElse"
        letId <- newLetId "IfElse"
        return $ Apply e3l letId theType 
                   (Apply e2l letId (tFun theType theType)
                    (Apply e1l letId (ifType theType) 
                               (Var sourceLoc (FuncName $ T.pack "$ifelse"))
                               boolExp) trueExp) falseExp
    oprFuncExp =
      do
        mySpaces
        startPos <- currentPos
        left <- term
        mySpaces
        opr <- many1 $ oneOf "+-*/&|=><!?"
        mySpaces
        t1 <- newTyVar "OAt1"
        t2 <- newTyVar "OAt2"
        t3 <- newTyVar "OAt3"
        t4 <- newTyVar "OAt4"
        letId <- newLetId $ "binary" ++ opr
        right <- expression <?> "Looking for right side of exp"
        sourceLoc <- calcSourceLoc startPos
        return $ Apply sourceLoc letId t2 (Apply sourceLoc letId t4 (Var sourceLoc (FuncName $ T.pack opr)) left) right

currentPos :: MParser SourcePoint
currentPos = 
  do
    col <- curColumn
    line <- curLine
    return $ (line, col)

getSourceInfo :: SourceSpan -> MParser SourceInfo
getSourceInfo off@((startLine, startOff), (endLine, endOff)) =
  do
    (arr, txt) <- fmap tiSource getState
    let first = (startOff - 1) + (arr ! (startLine - 1))
    let last = endOff + (arr ! (endLine - 1))
    let len = last - first
    return $ (off, T.take len $ T.drop first txt)

calcSourceLoc :: SourcePoint -> MParser SourceLoc
calcSourceLoc startPos =
  do
    endPos <- currentPos
    sourceUrl <- fmap tiSourceUrl getState
    let span = (startPos, endPos)
    info <- getSourceInfo span
    return $ (maybe (SourceLoc info) (\u -> SourceFromURL u info) sourceUrl) NoSourceLoc

curColumn :: MParser Int
curColumn = sourceColumn <$> getPosition

curLine :: MParser Int
curLine = sourceLine <$> getPosition

newLetId prefix =
    do
      s <- getState
      setState s{tiSupply = tiSupply s + 1}
      return $ LetId $ T.pack $ prefix ++ show (tiSupply s)

runDepth d c =
  do
    curr <- getState
    let cd = tiDepth curr
    setState curr{tiDepth = d}
    res <- c
    nCurr <- getState
    setState nCurr{tiDepth = cd}
    return res

newTyVar prefix = do s <- getState
                     setState s{tiSupply = tiSupply s + 1}
                     return $ TVar $ T.pack $ prefix ++ show (tiSupply s)


-----------------------------------------------------------
-- Given a LanguageDef, create a token parser.
-----------------------------------------------------------

-- | The expression @makeTokenParser language@ creates a 'GenTokenParser'
-- record that contains lexical parsers that are
-- defined using the definitions in the @language@ record.
--
-- The use of this function is quite stylized - one imports the
-- appropiate language definition and selects the lexical parsers that
-- are needed from the resulting 'GenTokenParser'.
--
-- >  module Main where
-- >
-- >  import Text.Parsec
-- >  import qualified Text.Parsec.Token as P
-- >  import Text.Parsec.Language (haskellDef)
-- >
-- >  -- The parser
-- >  ...
-- >
-- >  expr  =   parens expr
-- >        <|> identifier
-- >        <|> ...
-- >       
-- >
-- >  -- The lexer
-- >  lexer       = P.makeTokenParser haskellDef    
-- >      
-- >  parens      = P.parens lexer
-- >  braces      = P.braces lexer
-- >  identifier  = P.identifier lexer
-- >  reserved    = P.reserved lexer
-- >  ...

m_makeTokenParser :: (Stream s m Char)
                => GenLanguageDef s u m -> GenTokenParser s u m
m_makeTokenParser languageDef
    = TokenParser{ identifier = identifier
                 , reserved = reserved
                 , operator = operator
                 , reservedOp = reservedOp

                 , charLiteral = charLiteral
                 , stringLiteral = stringLiteral
                 , natural = natural
                 , integer = integer
                 , float = float
                 , naturalOrFloat = naturalOrFloat
                 , decimal = decimal
                 , hexadecimal = hexadecimal
                 , octal = octal

                 , symbol = symbol
                 , lexeme = lexeme
                 , whiteSpace = whiteSpace

                 , parens = parens
                 , braces = braces
                 , angles = angles
                 , brackets = brackets
                 , squares = brackets
                 , semi = semi
                 , comma = comma
                 , colon = colon
                 , dot = dot
                 , semiSep = semiSep
                 , semiSep1 = semiSep1
                 , commaSep = commaSep
                 , commaSep1 = commaSep1
                 }
    where

    -----------------------------------------------------------
    -- Bracketing
    -----------------------------------------------------------
    parens        = between (symbol "(") (symbol ")")
    braces        = between (symbol "{") (symbol "}")
    angles        = between (symbol "<") (symbol ">")
    brackets      = between (symbol "[") (symbol "]")

    semi            = symbol ";"
    comma           = symbol ","
    dot             = symbol "."
    colon           = symbol ":"

    commaSep p      = sepBy p comma
    semiSep p       = sepBy p semi

    commaSep1 p     = sepBy1 p comma
    semiSep1 p      = sepBy1 p semi


    -----------------------------------------------------------
    -- Chars & Strings
    -----------------------------------------------------------
    charLiteral     = lexeme (between (char '\'')
                                      (char '\'' <?> "end of character")
                                      characterChar )
                    <?> "character"

    characterChar   = charLetter <|> charEscape
                    <?> "literal character"

    charEscape      = char '\\' >> escapeCode
    charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))



    stringLiteral   = lexeme (catMaybes <$> stringBody)
                      <?> "literal string"
      where stringBody = between (char '"')
                                 (char '"' <?> "end of string")
                                 (many stringChar)

    stringChar      =  Just <$> stringLetter
                    <|> stringEscape
                    <?> "string character"

    stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape    = char '\\' *> choice [ Nothing <$ escapeGap
                                          , Nothing <$ escapeEmpty
                                          , Just <$> escapeCode
                                          ]

    escapeEmpty     = char '&'
    escapeGap       = many1 space >> char '\\'
                    <?> "end of string gap"



    -- escape codes
    escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                    <?> "escape code"

    charControl     = do char '^'
                         code <- upper
                         return $ toEnum $ fromEnum code - fromEnum 'A'

    charNum         = toEnum . fromInteger <$> code
      where code = choice [ decimal
                          , char 'o' *> number 8 octDigit
                          , char 'x' *> number 16 hexDigit
                          ]

    charEsc         = choice $ parseEsc <$> escMap
      where parseEsc (c,code)     = code <$ char c

    charAscii       = choice $ parseAscii <$> asciiMap
      where parseAscii (asc,code) = code <$ string asc


    -- escape code tables
    escMap          = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
    asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

    ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                       "FS","GS","RS","US","SP"]
    ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                       "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                       "CAN","SUB","ESC","DEL"]

    ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                       '\EM','\FS','\GS','\RS','\US','\SP']
    ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                       '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                       '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


    -----------------------------------------------------------
    -- Numbers
    -----------------------------------------------------------
    naturalOrFloat  = lexeme natFloat <?> "number"

    float           = lexeme floating   <?> "float"
    integer         = lexeme int        <?> "integer"
    natural         = lexeme nat        <?> "natural"


    -- floats
    floating        = decimal >>= fractExponent

    natFloat        = (char '0' *> zeroNumFloat)
                   <|> decimalFloat

    zeroNumFloat    = Left <$> (hexadecimal <|> octal)
                    <|> decimalFloat
                    <|> fractFloat 0
                    <|> return (Left 0)

    decimalFloat    = do n <- decimal
                         option (Left n) (fractFloat n)

    fractFloat n    = Right <$> fractExponent n

    fractExponent n = do fract <- fraction
                         expo  <- option 1.0 exponent'
                         return $ (fromInteger n + fract) * expo
                    <|> (* fromInteger n) <$> exponent'
    
    fraction        = foldr op 0.0 <$> (char '.' *> many1 digit <?> "fraction")
                      <?> "fraction"
                    where
                      op d f = (f + fromIntegral (digitToInt d))/10.0

    exponent'       = do oneOf "eE"
                         f <- sign	  	
                         e <- decimal <?> "exponent"                       
                         return $ power (f e)
                      <?> "exponent"
      where
        power e  | e < 0      = 1.0/power(-e)
                 | otherwise  = fromInteger (10^e)


    -- integers and naturals
    int             = ($) <$> lexeme sign <*> nat

    sign            =   (negate <$ char '-')
                    <|> (id <$ char '+')
                    <|> return id

    nat             = zeroNumber <|> decimal

    zeroNumber      = char '0' *> choice [ hexadecimal, octal, decimal, return 0 ]
                      <?> ""

    decimal         = number 10 digit
    hexadecimal     = oneOf "xX" *> number 16 hexDigit
    octal           = oneOf "oO" *> number 8 octDigit

    number base baseDigit
        = do digits <- many1 baseDigit
             let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
             seq n $ return n

    -----------------------------------------------------------
    -- Operators & reserved ops
    -----------------------------------------------------------
    reservedOp name =
        lexeme $ try $ do
          string name
          notFollowedBy (opLetter languageDef) <?> ("end of " ++ show name)

    operator =
        lexeme $ try $ do
          name <- oper
          if isReservedOp name
            then unexpected $ "reserved operator " ++ show name
            else return name

    oper = (:) <$> opStart languageDef <*> many (opLetter languageDef)
        <?> "operator"

    isReservedOp =
        isReserved $ sort $ reservedOpNames languageDef


    -----------------------------------------------------------
    -- Identifiers & Reserved words
    -----------------------------------------------------------
    reserved name =
        lexeme $ try $ do
          caseString name
          notFollowedBy (identLetter languageDef) <?> ("end of " ++ show name)

    caseString name
        | caseSensitive languageDef  = string name
        | otherwise               = name <$ walk name
        where
          walk []     = return ()
          walk (c:cs) = (caseChar c <?> msg) *> walk cs

          caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                      | otherwise  = char c

          msg         = show name


    identifier =
        lexeme $ try $ do
          name <- ident
          if isReservedName name
            then unexpected $ "reserved word " ++ name
            else return name


    ident
        = (:) <$> identStart languageDef <*> many (identLetter languageDef)
        <?> "identifier"

    isReservedName name
        = isReserved theReservedNames caseName
        where
          caseName      | caseSensitive languageDef  = name
                        | otherwise               = map toLower name


    isReserved names name
        = scan names
        where
          scan []       = False
          scan (r:rs)   = case compare r name of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False

    theReservedNames
        | caseSensitive languageDef  = sort reserved
        | otherwise                  = sort . map (map toLower) $ reserved
        where
          reserved = reservedNames languageDef



    -----------------------------------------------------------
    -- White space & symbols
    -----------------------------------------------------------
    symbol name
        = lexeme (string name)

    lexeme p
        = p <* whiteSpace


    --whiteSpace
    whiteSpace
        | noLine && noMulti  = skipMany (simpleSpace <?> "")
        | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
        | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
        | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
        where
          noLine  = null (commentLine languageDef)
          noMulti = null (commentStart languageDef)

    m_isSpace c = c `elem` [' ', '\t']

    simpleSpace =
        skipMany1 $ satisfy m_isSpace

    oneLineComment = (try . string $ commentLine languageDef) *> skipMany (satisfy (/='\n'))

    multiLineComment = (try . string $ commentStart languageDef) *> inComment

    inComment
        | nestedComments languageDef  = inCommentMulti
        | otherwise                   = inCommentSingle

    inCommentMulti
        =   () <$ try (string $ commentEnd languageDef)
        <|> multiLineComment            *> inCommentMulti
        <|> skipMany1 (noneOf startEnd) *> inCommentMulti
        <|> oneOf startEnd              *> inCommentMulti
        <?> "end of comment"
        where
          startEnd   = nub $ commentEnd languageDef ++ commentStart languageDef

    inCommentSingle
        =   () <$ try (string $ commentEnd languageDef)
        <|> skipMany1 (noneOf startEnd) *> inCommentSingle
        <|> oneOf startEnd              *> inCommentSingle
        <?> "end of comment"
        where
          startEnd   = nub $ commentEnd languageDef ++ commentStart languageDef
