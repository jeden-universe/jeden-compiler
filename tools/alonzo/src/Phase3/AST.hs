
module Jeden.Phase3.AST (
    module Jeden.AST ( Module, Term, Type, Symbol )
    -- * Type-checking
    Context(..),
    Typing(..)
) where

import Jeden.AST        ( Global, Type )

import Data.Map         ( Map )
import Prelude ()

newtype Context = Context (Map Global Typing)

data Typing     = Typing {
    locals      :: Map Local Type,
    itype       :: Type
}
