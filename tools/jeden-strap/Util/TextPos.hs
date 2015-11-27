
module Util.TextPos (
        TextPos(..), append
    )
where

import qualified Data.ByteString.UTF8 as BS
import Data.Char (Char)
import Data.Monoid

import Prelude (Int, Num(..), Eq(..), Show)

type Line           = Int
type Column         = Int

data TextPos =
    TextPos !Line !Column
    deriving (Eq,Show)

instance Monoid TextPos where
    mempty = TextPos 0 0
    mappend (TextPos l1 c1) (TextPos l2 c2) =
        TextPos
            (l1 + l2)
            (if l2 == 0 then c1+c2 else c2)

append :: TextPos -> BS.ByteString -> TextPos
append = BS.foldr f
    where
    f :: Char -> TextPos -> TextPos
    f '\n' (TextPos l c) =
        TextPos (l + 1) 0
    f _    (TextPos l c) =
        TextPos l (c + 1)
