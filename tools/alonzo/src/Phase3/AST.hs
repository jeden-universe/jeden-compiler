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
import Text.PrettyPrint.HughesPJClass ( Pretty(..), (<+>), text, maybeParens )


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

-- TODO
--   add location information
--   tag bad typings to delay error reporting
data Typing     = Typing {
    locals      :: Map Local Type,
    itype       :: Type
}


instance Pretty Type where
    pPrintPrec _ p (TyVar v) =
        text v
    pPrintPrec lvl p (TyFun t1 t2) =
        maybeParens
            (p >= 1)
            (pPrintPrec lvl 1 t1 <+> text "->" <+> pPrintPrec lvl 0 t2)
