{-# LANGUAGE
    GADTs,
    Arrows,
    FlexibleInstances,
    MultiParamTypeClasses,
    GeneralizedNewtypeDeriving
  #-}

module Arrow.Lexer where

import Control.Applicative hiding (some,many)
import Control.Arrow (Arrow, ArrowChoice, ArrowApply, ArrowZero, ArrowPlus,
    Kleisli(..), (<+>), returnA)
import Control.Arrow.Operations
import Control.Arrow.Transformer.State
import Control.Category
import Control.Monad
import Data.ByteString
import qualified Data.ByteString.UTF8 as UTF
import Data.Char
import Data.Word
import Text.Show

import Arrow.ArrowMany

import Prelude (
        ($),
        Char, Bool, (||), String,
        Eq(..), Ord(..), Maybe(..), Either(..), Show(..), (++), Enum(..),
    )

-- | The @LexerArrow@ efficiently matches the head of the input string.
-- It does not, however, capture the match results. This should be performed
-- by the caller, typically an outer Arrow.
newtype LexerArrow i o = LexerArrow {
        runLexer :: StateArrow ByteString (Kleisli LResult) i o
                 -- (i, BS) -> LResult (o, BS)
    } deriving (Category, Arrow, ArrowChoice, ArrowApply, ArrowZero, ArrowPlus)

instance ArrowMany LexerArrow where
    some f = proc i -> do
        x  <- f -< i
        xs <- many f -< i
        returnA -< x : xs

    many f = proc i -> do
        some f -< i
       <+> do
        returnA -< []

-- | Lexing results either in
--    * success with input of lexer copied in output
--    * failure with a short error string
data LResult a where
    LDone   :: a -> LResult a
    LFail   :: String -> LResult a

instance Show a => Show (LResult a) where
    showsPrec p (LDone x)   =
        showParen (p >= 10) (showString "LDone " . showsPrec 11 x)
    showsPrec p (LFail e)   =
        showParen (p >= 10) (showString "LFail " . showsPrec 11 e)

lexer :: LexerArrow i o -> (i,ByteString) -> LResult (o,ByteString)
lexer lex =
    runKleisli $ runState $ runLexer lex

(<?>) :: LexerArrow i o -> String -> LexerArrow i o
lex <?> e =
    lex <+> (LexerArrow $ StateArrow $ Kleisli $ \_ -> LFail e)

infix 0 <?>

-- Functions

alpha :: LexerArrow i i
alpha = satisfy isAlpha <?> "alpha"

digit :: LexerArrow i i
digit = satisfy isDigit <?> "digit"

alphaNum :: LexerArrow i i
alphaNum = satisfy isAlphaNum <?> "alphaNum"

space :: LexerArrow i i
space = satisfy isSpace <?> "space"

lower :: LexerArrow i i
lower = satisfy isLower <?> "lower-case letter"

upper :: LexerArrow i i
upper = satisfy isUpper <?> "upper-case letter"

char :: Char -> LexerArrow i i
char c = satisfy (== c) <?> "character '" ++ (c : "'")

word8 :: Word8 -> LexerArrow i i
word8 c = LexerArrow $ StateArrow $ Kleisli $ \(x,s) ->
    case uncons s of
        Just (d,s') | c == d ->
            LDone (x,s')
        _ ->
            LFail $ "word8 '" ++ (chr $ fromEnum c) : "'"

newline :: LexerArrow i i
newline = (satisfy $ \c -> (c == '\n') || (c == '\r')) <?> "newline"

satisfy :: (Char -> Bool) -> LexerArrow i i
satisfy test = LexerArrow $ StateArrow $ Kleisli $ cont test
    where
    cont test = \(x,s) ->
        case UTF.uncons s of
            Just (c,s') ->
                if test c then LDone (x,s') else LFail "satisfy"
            Nothing ->
                LFail "EOF"

takeWhile :: (Char -> Bool) -> LexerArrow i i
takeWhile test = LexerArrow $ StateArrow $ Kleisli $ cont test
    where
    cont test = \(x,s) ->
        let (_,s') = UTF.span test s
        in LDone (x,s')

string :: ByteString -> LexerArrow i i
string str = LexerArrow $ StateArrow $ Kleisli $ cont str
    where
    cont str = \(x,s) ->
        if str `isPrefixOf` s
            then LDone (x, drop (length str) s)
            else LFail $ "string: " ++ show str

lookAhead :: LexerArrow i i -> LexerArrow i i
lookAhead lex = LexerArrow $ StateArrow $ Kleisli $ \(x,s) ->
    case lexer lex (x,s) of
        LDone (x',s') -> LDone (x',s)
        LFail e -> LFail $ "lookAhead " ++ e

notFollowedBy :: LexerArrow i i -> LexerArrow i i
notFollowedBy lex = LexerArrow $ StateArrow $ Kleisli $ \(x,s) ->
    case lexer lex (x,s) of
        LDone _ -> LFail "notFollowedBy"
        LFail e -> LDone (x,s)

-- Instance declarations

instance ArrowError String LexerArrow where
    raise   = LexerArrow $ StateArrow $ Kleisli $ \(e,_) -> LFail e

    tryInUnless try succ err =
        LexerArrow $ StateArrow $ Kleisli $ \(e,s) ->
            case (lexer try) (e,s) of
                LDone (b,s') ->
                    (lexer succ) ((e,b),s')
                LFail ex ->
                    (lexer err) ((e,ex),s)

instance Functor LResult where
    fmap f (LDone a)    = LDone (f a)
    fmap _ (LFail s)    = LFail s

instance Applicative LResult where
    pure    = return
    (<*>)   = ap

instance Monad LResult where
    return  = LDone

    (LDone a)   >>= f     = f a
    (LFail s)   >>= _     = LFail s

instance Alternative LResult where
    empty   = mzero
    (<|>)   = mplus

instance MonadPlus LResult where
    mzero           = LFail "mzero"

    (LDone a) `mplus` _   = LDone a
    (LFail s) `mplus` g   = g
