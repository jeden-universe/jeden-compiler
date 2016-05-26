{-# LANGUAGE RankNTypes #-}

module Pointless.Parse (

)
where

import Pointless.Lambda         ( SLambda(..) )
import Pointless.Type

import Text.ByoParser
import Text.ByoParser.Stream

import Data.ByteString          ( ByteString )
import Data.ByteString.Short    ( ShortByteString, toShort, pack )
import Data.List                ( null, foldl, foldr )
import Data.Word                ( Word8 )

import Prelude (
    ($), String, Eq(..), Bool(..), (&&), (<=),
    return, otherwise )

type Lam = SLambda
var    = SVar
lambda = SAbs
app    = SApp

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

type Parser a = forall r. ParserPrim ByteString String () r a

termdef :: Parser (ShortByteString, Lam)
--      ::= IDENT '::=' term
termdef = do
    name <- _IDENT
    token 0x3A -- ':'
    token 0x3D -- '='
    somespace
    trm <- term
    return (name,trm)

term :: Parser Lam
--   ::= lambda | ( termvar | '(' term ')' )+
term =
    plambda <|> ( do
            trm <- termvar <|> parterm
            args <- many ( termvar <|> parterm )
            if null args
                then return trm
                else return (foldl app trm args)
        )

parterm :: Parser Lam
--      ::= '(' term ')'
parterm = do
    token 0x28 -- '('
    manyspace
    e <- term
    token 0x29 {- ) -} <?> "closing parethensis"
    manyspace
    return e

plambda :: Parser Lam
--     ::= '\' VAR+ '.' term
plambda = do
    token 0x5C -- '\'
    vars <- _VAR `sepBy1` somespace
    token 0x2E {- . -} <?> "lambda-separating dot"
    manyspace
    body <- term
    return $ foldr (\v e -> lambda v e) body vars

termvar :: Parser Lam
--      ::= VAR
termvar = do
    v <- _VAR
    return (var v)

_VAR :: Parser ShortByteString
--   ::= <lower-case ASCII letter>
_VAR = do
    v <- takeWhile1 isAsciiLower
    -- TODO: check for word termination
    manyspace
    return (toShort v)

_IDENT :: Parser ShortByteString
--     ::= <ASCII letter>+
_IDENT = do
    name <- takeWhile1 isAsciiLetter
    manyspace
    return (toShort name)

-----
-- Module elements
-----

pmodule :: Parser [(ShortByteString, Lam)]
pmodule =
    many termdef

-----
-- White space
-----

somespace :: Parser ()
somespace = do
    satisfy (== 0x20)
    manyspace

manyspace :: Parser ()
manyspace =
    skipWhile (== 0x20)

-----
-- Utility functions
-----

isAsciiLower :: Word8 -> Bool
isAsciiLower c
    | 0x61 <= c && c <= 0x7A = True
    | otherwise              = False

isAsciiLetter :: Word8 -> Bool
isAsciiLetter c
    | 0x41 <= c && c <= 0x5A = True
    | 0x61 <= c && c <= 0x7A = True
    | otherwise              = False
