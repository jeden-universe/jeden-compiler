
module Arrow.ArrowMany where

import Control.Arrow

class ArrowPlus a => ArrowMany a where
    some :: a i o -> a i [o]
    many :: a i o -> a i [o]
