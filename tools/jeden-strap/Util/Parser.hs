{-# LANGUAGE
    GADTs,
    Arrows,
    RankNTypes,
    FlexibleInstances,
    MultiParamTypeClasses
  #-}

module Util.Parser (
        Parser, Lexer,
        PResult(..), ParseError,
        parse, feed,
        lex, lex_,
        try, option, sepBy, between, pause,
        getPos
    )
where

import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.State
import Control.Category
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF
import Data.Char
import Data.Monoid
import Data.Tuple (fst)

import Arrow.ArrowMany
import Arrow.Lexer
import Arrow.Parser
import Util.TextPos

import Prelude (($), Eq(..), Num(..), (>=), Show(..), showParen, showString,
    String, const)

newtype Parser i o = Parser {
        theParser :: forall r. ParserArrow (PResult r) ParseError (StateArrow PState (->)) i o
                  -- ParserPlate * -> StateArrow { (i,PState) -> (PResult r,PState) }
    }

type Lexer = LexerArrow () ()
type ParseError = String

data PResult a where
    PDone   :: a -> PResult a
    PFail   :: ParseError -> PResult a
    PMore   :: (ByteString -> (PResult a, PState))
            -> PResult a

parse :: Parser () r -> ByteString -> PResult r
parse (Parser p) input =
    fst $ runState
        (runParser p (arr PFail) (arr PDone) (arr PFail) (arr PDone))
        ((), PState mempty input [])

-- | Feed additional input to a partial result.
feed :: ByteString -> PResult r -> PResult r
feed input (PMore f) =
    fst $ f input
feed _ res = res

lex_ :: LexerArrow () e -> Parser () ()
lex_ theLexer = Parser $ ParserArrow $ \noC okS noS okC ->
    StateArrow $
        let this = \((), st) -> case lexer theLexer ((),stream st) of {
            LDone (_,s') ->
                let len = BS.length (stream st) - BS.length s'
                in  if len == 0
                    then runState okS ((), st { stream = s'})
                    else runState okC ((), st {
                        pos = pos st `append` BS.take len (stream st),
                        stream = s'})
          ; LFail e ->
                if BS.null (stream st)
                then (PMore $ \s' -> this ((), st { stream = s'}), st)
                else runState noS (e,st)
            }
        in this

-- | Insert the given lexer, capturing whatever it matches.
lex :: LexerArrow () e -> Parser () ByteString
lex theLexer = Parser $ ParserArrow $ \noC okS noS okC ->
    StateArrow $
        let this = \((),st) -> case lexer theLexer ((),stream st) of {
            LDone (_,s') ->
                let len = BS.length (stream st) - BS.length s'
                in  if len == 0
                    then runState okS (BS.empty, st { stream = s'})
                    else runState
                            okC
                            ( let out = BS.take len (stream st)
                              in (out, st { pos = pos st `append` out , stream = s' }) )
          ; LFail e ->
                if BS.null (stream st)
                then (PMore $ \s' -> this ((),st { stream = s'}), st)
                else runState noS (e,st)
            }
        in this

-- Combinators

-- | Turn a Parser into a look-ahead Parser. A failure occurring after consuming
--   input is recovered from.
--   ## TODO: can be implemented on top of ArrowError class
try :: Parser i o -> Parser i o
try p = Parser $ ParserArrow $ \noC okS noS okC ->
    StateArrow $ \(x,st) ->
        runState
            (runParser (theParser p)
                (noS . StateArrow (\(e,_) -> (e,st)))
                okS noS okC
            )
            (x,st)

option :: ArrowPlus a => o -> a () o -> a () o
option x try =
    try <+> (proc () -> returnA -< x)

sepBy :: Parser () i -> Parser () e -> Parser () [i]
sepBy pat sep = proc () -> do
    x  <- pat -< ()
    xs <- many $ try (sep >>> (arr $ const ()) >>> pat) -< ()
    returnA -< x : xs

between :: Parser () b -> Parser () e -> Parser () o
        -> Parser () o
between begin end inside = proc () -> do
    begin -< ()
    out <- inside -< ()
    end -< ()
    returnA -< out

-- | Pause parsing if input stream is empty. This is useful when
--   placed immediately before 'many', preventing the latter from
--   matching the empty string.
pause :: Parser i i
pause = Parser $ ParserArrow $ \noC okS noS okC ->
    StateArrow $
        let this = \(x, st) ->
                    if BS.null (stream st)
                    then (PMore $ \s' -> this (x, st {stream = s'}), st)
                    else runState okS (x, st)
        in this

getPos :: Parser () TextPos
getPos = proc () -> do
    PState pos _ _ <- fetch -< ()
    returnA -< pos

-- Internals

data PState = PState {
        pos         :: !TextPos,
        stream      :: !ByteString,
        prefix      :: [ByteString]
    }

instance Show a => Show (PResult a) where
    showsPrec p (PDone x)   =
        showParen (p >= 10) (showString "PDone " . showsPrec 11 x)
    showsPrec p (PFail e)   =
        showParen (p >= 10) (showString "PFail " . showsPrec 11 e)
    showsPrec p (PMore _)   =
        showParen (p >= 10) (showString "PMore <...>")

-- Parser instances
-- connot be newtype-derived by GHC because of the inner Rank2 polymorphism

instance Category Parser where
    id = Parser id
    Parser g . Parser f = Parser $ g . f

instance Arrow Parser where
    arr f = Parser $ arr f
    first (Parser f) = Parser (first f)

instance ArrowZero Parser where
    zeroArrow = Parser zeroArrow

instance ArrowPlus Parser where
    Parser f <+> Parser g = Parser $ f <+> g

instance ArrowMany Parser where
    many (Parser f) = Parser $ many f
    some (Parser f) = Parser $ some f

instance ArrowState PState Parser where
    fetch = Parser fetch
    store = Parser store
