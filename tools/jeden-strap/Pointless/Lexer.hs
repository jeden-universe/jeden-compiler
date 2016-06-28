{-# LANGUAGE BangPatterns #-}

module Pointless.Lexer (
    Lexeme(..), lexer1,
    Token(..), lexer2
) where

import Text.ByoParser.LexerPos

import Control.Applicative      ( Applicative(..), Alternative((<|>), some, many), (<$>) )
import Data.ByteString.Short    ( ShortByteString )
import Data.Char                ( Char, isLetter )
import Data.List                ( elem )
import Data.Map                 ( Map, empty, insert, lookup )
import Data.String              ( String, fromString )

import Prelude ( ($), Monad(..), Maybe(..), Eq(..), (||), Show )


{-  Recognized Grammar:
      Pointless simply typed calculus

start     ::= module

module    ::= ( typedef | termdecl termdef )*

typedef   ::= typevar typevar* '::=' type

termdecl  ::= IDENT '::' type
termdef   ::= IDENT ':=' term

type      ::= typevar typevar* ( '->' type )?
           |  '(' type ')'
typevar   ::= VAR

term      ::= lambda
           |  ( termvar | parterm )+
lambda    ::= '\' termvar+ '.' term
parterm   ::= '(' term ')'

termvar   ::= VAR

-}


-- utf8decode :: ByteString -> [Char]
-- lexer1 :: Lexer Char Lexeme
-- lexer2 :: LexerState ([String],Set ShortByteString) Lexeme Token

-- for efficiency, these could store 'ByteString', i.e., pointers into the
-- stream being parsed
data Lexeme
  = LexIndent       String
  | LexName         String SrcLoc
  | LexPragma       String SrcLoc
  | LexOpenParen    SrcLoc
  | LexCloseParen   SrcLoc
  | LexSymbol       String SrcLoc
  | LexEOF
  | LexError        String SrcLoc
  deriving (Eq,Show)

lexer1 :: String -> [Char] -> [Lexeme]
lexer1 fname = lex fname
    (lexIndent <|> lexName <|> lexOpenParen <|> lexCloseParen <|> lexSymbol <|> lexEOF)

lexIndent :: Lexer Char Lexeme
lexIndent = LexIndent <$> ( newline *> many (satisfy (\c -> (c == ' ') || (c == '\t'))) )

lexSpace :: Lexer Char ()
lexSpace = do
    many $ satisfy $ \c -> (c == ' ') || (c == '\t')
    return ()

lexName :: Lexer Char Lexeme
lexName = do
    loc <- location
    name <- (:) <$> letter <*> many (letter <|> digit)
    lexSpace
    return $ LexName name loc

lexOpenParen :: Lexer Char Lexeme
lexOpenParen = LexOpenParen <$> ( char '(' >> lexSpace *> location )

lexCloseParen :: Lexer Char Lexeme
lexCloseParen = LexCloseParen <$> ( char ')' >> lexSpace *> location )

lexSymbol :: Lexer Char Lexeme
lexSymbol = do
    loc <- location
    sym <- some (satisfy (\c -> elem c symbols))
    lexSpace
    return $ LexSymbol sym loc
    where symbols = "\\->:=()."

lexEOF :: Lexer Char Lexeme
lexEOF = do
    endOfInput
    return LexEOF



{- A token is one of:

  * a chunk of leading white-space, possibly empty (= newline)
  * a name:  letter (letter | digit)*
  * a number:  digit+
  * '('
  * ')'
  * a chunk of special characters  !(letter | digit | space)+
  * end
  * error

  * other white-space is stripped

-}
data Token
  = TokIndent
  | TokUnindent
  | TokName         ShortByteString SrcLoc
  | TokPragma       ShortByteString SrcLoc
  | TokOpenParen    SrcLoc
  | TokCloseParen   SrcLoc
  | TokSymbol       ShortByteString SrcLoc
  | TokEOF
  | TokError        String SrcLoc
  deriving (Eq,Show)

{-
Stage 2 lexer. Transforms lexemes into tokens.
  - indentation is analyzed and converted into INDENT, NEWLINE and UNINDENT tokens
  - strings are captured as shared ShortByteString objects
-}
lexer2 :: [Lexeme] -> [Token]
lexer2 = go [] empty
    where
        go :: [String] -> Map String ShortByteString -> [Lexeme] -> [Token]
        go indents dict [] = []
        go indents dict (LexName name loc : ls) = case lookup name dict of
            Just name   -> (TokName name loc) : go indents dict ls
            Nothing     -> let !name' = fromString name in
                (TokName name' loc) : go indents (insert name name' dict) ls
        go indents dict (LexOpenParen loc : ls) =
            TokOpenParen loc : go indents dict ls
        go indents dict (LexCloseParen loc : ls) =
            TokCloseParen loc : go indents dict ls
        go indents dict (LexSymbol sym loc : ls) = case lookup sym dict of
            Just sym    -> (TokSymbol sym loc) : go indents dict ls
            Nothing     -> let !sym' = fromString sym in
                (TokSymbol sym' loc) : go indents (insert sym sym' dict) ls
        go indents dict (LexEOF : ls) = [TokEOF]
