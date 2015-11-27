{-# LANGUAGE
    Arrows,
    TupleSections,
    FlexibleInstances,
    UndecidableInstances,
    MultiParamTypeClasses
  #-}

module Arrow.Parser ( ParserArrow(..) )
where

import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Category
import Data.Either

import Arrow.ArrowMany

import Prelude (($), error, undefined)

data ParserArrow r e a i o = ParserArrow {
        runParser   :: a e r    -- failConsumed
                    -> a o r    -- okSkip
                    -> a e r    -- failSkip
                    -> a o r    -- okConsumed
                    -> a i r
    }

instance Category (ParserArrow r e a) where
    id      = ParserArrow $ \noC okS noS okC -> okS
    g . f   = ParserArrow $ \noC okS noS okC ->
                runParser f
                    noC
                    (runParser g noC okS noS okC)
                    noS
                    (runParser g noC okC noC okC)

instance ArrowApply a => Arrow (ParserArrow r e a) where
    arr f   = ParserArrow $ \noC okS noS okC ->
                okS . arr f

    first f = ParserArrow $ \noC okS noS okC -> proc (x,y) -> do
                runParser f noC (okS . arr (,y)) noS (okC . arr (,y)) -<< x

instance (ArrowApply a, ArrowChoice a) => ArrowChoice (ParserArrow r e a) where
    left f  = ParserArrow $ \noC okS noS okC -> proc x -> do
        case x of
            Left x ->
                runParser f noC (okS . arr Left) noS (okC . arr Left) -< x
            Right x ->
                okS -< Right x

instance ArrowApply a => ArrowZero (ParserArrow r e a) where
    zeroArrow = ParserArrow $ \noC okS noS okC -> proc _ ->
                    noS -< undefined

instance ArrowApply a => ArrowPlus (ParserArrow r e a) where
    f <+> g = ParserArrow $ \noC okS noS okC -> proc i ->
        runParser f
            noC
            okS
            (proc _ -> runParser g noC okS noS okC -< i)
            okC
            -<< i

instance ArrowApply a => ArrowMany (ParserArrow r e a) where
    some p = ParserArrow $ \noC okS noS okC -> proc i -> do
        runParser p
            noC
            (error "'some' accepted empty input")
            noS
            (proc x -> do
                runParser (many p)
                    noC
                    okS
                    noS
                    (okC . arr (x:))
                    -<< i
            )
            -<< i

    many p = ParserArrow $ \noC okS noS okC -> proc i -> do
        runParser p
            noC
            (error "'many' accepted empty input")
            (proc _ -> okS -< [])
            (proc x -> do
                runParser (many p)
                    noC
                    (okS . arr (x:))
                    noS
                    (okC . arr (x:))
                    -<< i
            )
            -<< i

instance (ArrowLoop a, ArrowApply a) => ArrowLoop (ParserArrow r e a) where
    loop f  = ParserArrow $ \noC okS noS okC ->
        runParser (loop f) noC okS noS okC

instance ArrowApply a => ArrowTransformer (ParserArrow r e) a where
    lift f  = ParserArrow $ \noC okS noS okC ->
        okS . f

instance ArrowApply a => ArrowError e (ParserArrow r e a) where
    raise   = ParserArrow $ \noC okS noS okC -> noS

    tryInUnless try succ fail =
        ParserArrow $ \noC okS noS okC -> proc e -> do
            runParser try
                (runParser fail noC okS noS okC . arr (e,))
                (runParser succ noC okS noS okC . arr (e,))
                (runParser fail noC okS noS okC . arr (e,))
                (runParser succ noC okC noS okC . arr (e,))
                -<< e

-- this requires UndecidableInstances, we are conjuring a hidden state 's'
instance (ArrowApply a, ArrowState s a) => ArrowState s (ParserArrow r e a) where
    fetch = lift fetch
    store = lift store
