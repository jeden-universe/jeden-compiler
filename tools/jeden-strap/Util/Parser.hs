{-# LANGUAGE
    GADTs,
    Arrows,
    GeneralizedNewtypeDeriving
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

newtype Parser r i o = Parser {
        theParser :: ParserArrow (PResult r) ParseError (StateArrow PState (->)) i o
                  -- ParserPlate * -> StateArrow { (i,PState) -> (PResult r,PState) }
    } deriving (Category, Arrow, ArrowChoice,
        ArrowZero, ArrowPlus, ArrowMany, ArrowLoop,
        ArrowError ParseError, ArrowState PState)

type Lexer = LexerArrow () ()
type ParseError = String

data PResult a where
    PDone   :: a -> PResult a
    PFail   :: ParseError -> PResult a
    PMore   :: (ByteString -> (PResult a, PState))
            -> PResult a

parse :: Parser r () r -> ByteString -> PResult r
parse (Parser p) input =
    fst $ runState
        (runParser p ParserPlate {
            failConsumed    = arr PFail,
            okSkip          = arr PDone,
            failSkip        = arr PFail,
            okConsumed      = arr PDone
        })
        ((), PState mempty input)

-- | Feed additional input to a partial result.
feed :: ByteString -> PResult r -> PResult r
feed input (PMore f) =
    fst $ f input
feed _ res = res

lex_ :: LexerArrow () e -> Parser r () ()
lex_ theLexer = Parser $ ParserArrow $ \plate ->
    StateArrow $
        let this = \((), PState pos s) -> case lexer theLexer ((),s) of {
            LDone (_,s') ->
                let len = BS.length s - BS.length s'
                in  if len == 0
                    then runState
                            (okSkip plate)
                            ((), PState pos s')
                    else runState
                            (okConsumed plate)
                            ((), PState (append pos $ BS.take len s) s')
          ; LFail e ->
                if BS.null s
                then (PMore $ \s' -> this ((), PState pos s'), PState pos s)
                else runState
                        (failSkip plate)
                        (e, PState pos s)
            }
        in this

-- | Insert the given lexer, capturing whatever it matches.
lex :: LexerArrow () e -> Parser r () ByteString
lex theLexer = Parser $ ParserArrow $ \plate ->
    StateArrow $
        let this = \((),PState pos s) -> case lexer theLexer ((),s) of {
            LDone (_,s') ->
                let len = BS.length s - BS.length s'
                in  if len == 0
                    then runState
                            (okSkip plate)
                            (BS.empty, PState pos s')
                    else runState
                            (okConsumed plate)
                            (let out = BS.take len s
                              in (out, PState (append pos out) s'))
          ; LFail e ->
                if BS.null s
                then (PMore $ \s' -> this ((),PState pos s'), PState pos s)
                else runState
                        (failSkip plate)
                        (e, PState pos s)
            }
        in this

-- Combinators

-- | Turn a Parser into a look-ahead Parser. A failure occurring after consuming
--   input is recovered from.
--   ## TODO: can be implemented on top of ArrowError class
try :: Parser r i o -> Parser r i o
try p = Parser $ ParserArrow $ \plate ->
    StateArrow $ \(x,s) ->
        runState
            (runParser (theParser p) plate {
                failConsumed = failSkip plate . StateArrow (\(e,_) -> (e,s))
            })
            (x,s)

option :: ArrowPlus a => o -> a () o -> a () o
option x try =
    try <+> (proc () -> returnA -< x)

sepBy :: Parser r () i -> Parser r () e -> Parser r () [i]
sepBy pat sep = proc () -> do
    x  <- pat -< ()
    xs <- many $ try (sep >>> (arr $ const ()) >>> pat) -< ()
    returnA -< x : xs

between :: Parser r () b -> Parser r () e -> Parser r () o
        -> Parser r () o
between begin end inside = proc () -> do
    begin -< ()
    out <- inside -< ()
    end -< ()
    returnA -< out

-- | Pause parsing if input stream is empty. This is useful when
--   placed immediately before 'many', preventing the latter from
--   matching the empty string.
pause :: Parser r i i
pause = Parser $ ParserArrow $ \plate ->
    StateArrow $
        let this = \(x, PState pos s) ->
                    if BS.null s
                    then (PMore $ \s' -> this (x, PState pos s'), PState pos s)
                    else runState (okSkip plate) (x, PState pos s)
        in this

getPos :: Parser r () TextPos
getPos = proc () -> do
    PState pos _ <- fetch -< ()
    returnA -< pos

-- Internals

data PState = PState {
        pos         :: !TextPos,
        stream      :: !ByteString
    }

instance Show a => Show (PResult a) where
    showsPrec p (PDone x)   =
        showParen (p >= 10) (showString "PDone " . showsPrec 11 x)
    showsPrec p (PFail e)   =
        showParen (p >= 10) (showString "PFail " . showsPrec 11 e)
    showsPrec p (PMore _)   =
        showParen (p >= 10) (showString "PMore <...>")
