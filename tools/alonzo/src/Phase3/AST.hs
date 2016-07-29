{-# LANGUAGE DeriveGeneric #-}

module Phase3.AST (
    Term(..), Type(..), Position(..),
    Global(..), Globals(..), Meaning(..),
    -- * Type-checking
    Local(..),
    Context(..),
    Typing(..)
) where

import Phase2           ( Global, Term, Position )

import Data.Map         ( Map )
import GHC.Generics
import Text.PrettyPrint.GenericPretty ( Out )


data Type = TyVar String
          | TyFun Type Type
          | TyType
    deriving (Eq,Show,Generic)

newtype Globals = Globals (Map Global Meaning) deriving (Show,Generic)

data Meaning    = MTypeDef  Type Position
                | MTermDecl Type Position
                | MTermDef  Type Term Position
    deriving (Show,Generic)

instance Out Globals
instance Out Meaning
instance Out Type

type Local      = String

newtype Context = Context (Map Global (Position,Typing))

data Typing     = Typing {
    locals      :: Map Local Type,
    itype       :: Type
}
