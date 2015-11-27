{-# LANGUAGE
    Arrows,
    TupleSections,
    FlexibleInstances,
    UndecidableInstances,
    MultiParamTypeClasses
  #-}

module Arrow.Parser ( ParserPlate(..), ParserArrow(..) )
where

import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Category
import Data.Either

import Arrow.ArrowMany

import Prelude (($), error, undefined)

data ParserPlate e a o r = ParserPlate {
        failConsumed    :: a e r,
        okSkip          :: a o r,
        failSkip        :: a e r,
        okConsumed      :: a o r
    }

-- GHC would much prefer a long lambda expression
data ParserArrow r e a i o = ParserArrow {
        runParser :: ParserPlate e a o r -> a i r
    }

instance Category (ParserArrow r e a) where
    id      = ParserArrow $ okSkip
    g . f   = ParserArrow $ \plate ->
        runParser f $ plate {
            okSkip      = runParser g plate,
            okConsumed  = runParser g $ plate {
                okSkip      = okConsumed plate,
                failSkip    = failConsumed plate
            }
        }

instance ArrowApply a => Arrow (ParserArrow r e a) where
    arr f   = ParserArrow $ \plate -> okSkip plate . arr f

    first f = ParserArrow $ \plate -> proc (x,y) -> do
        runParser f $ plate {
            okSkip      = okSkip plate . arr (,y),
            okConsumed  = okConsumed plate . arr (,y)
        } -<< x

instance (ArrowApply a, ArrowChoice a) => ArrowChoice (ParserArrow r e a) where
    left f  = ParserArrow $ \plate -> proc x -> do
        case x of
            Left x  ->
                runParser f $ plate {
                    okSkip      = okSkip plate . arr Left,
                    okConsumed  = okConsumed plate . arr Left
                } -< x
            Right x ->
                okSkip plate -< Right x

instance ArrowApply a => ArrowZero (ParserArrow r e a) where
    zeroArrow = ParserArrow $ \plate -> proc _ ->
        failSkip plate -< undefined

instance ArrowApply a => ArrowPlus (ParserArrow r e a) where
    f <+> g = ParserArrow $ \plate -> proc i ->
        runParser f $ plate {
            failSkip = proc _ -> runParser g plate -< i
        } -<< i

instance ArrowApply a => ArrowMany (ParserArrow r e a) where
    some p = ParserArrow $ \plate -> proc i -> do
        runParser p $ plate {
            okSkip      = error "'some' accepted empty input",
            okConsumed  = proc x -> do
                runParser (many p) plate {
                    okConsumed  = okConsumed plate . arr (x:)
                } -<< i
        } -<< i

    many p = ParserArrow $ \plate -> proc i -> do
        runParser p $ plate {
            failSkip        = proc _ -> okSkip plate -< [],
            okSkip          = error "'many' accepted empty input",
            okConsumed      = proc x -> do
                runParser (many p) plate {
                    okSkip      = okConsumed plate . arr (x:),
                    okConsumed  = okConsumed plate . arr (x:)
                } -<< i
        } -<< i

instance (ArrowLoop a, ArrowApply a) => ArrowLoop (ParserArrow r e a) where
    loop f  = ParserArrow $ \plate ->
        runParser (loop f) plate

instance ArrowApply a => ArrowTransformer (ParserArrow r e) a where
    lift f  = ParserArrow $ \plate -> okSkip plate . f

instance ArrowApply a => ArrowError e (ParserArrow r e a) where
    raise   = ParserArrow $ failSkip

    tryInUnless try succ fail =
        ParserArrow $ \plate -> proc e -> do
            runParser try $ plate {
                okSkip          = runParser succ plate . arr (e,),
                okConsumed      = runParser succ plate {
                        okSkip = okConsumed plate
                    } . arr (e,),
                failSkip        = runParser fail plate . arr (e,),
                failConsumed    = runParser fail plate . arr (e,)
            } -<< e

-- this requires UndecidableInstances, we are conjuring a hidden state 's'
instance (ArrowApply a, ArrowState s a) => ArrowState s (ParserArrow r e a) where
    fetch = lift fetch
    store = lift store
