{-# LANGUAGE DeriveGeneric #-}

module Phase3.AST (
    Globals(..), Global(..), Local(..),
    Meaning(..), Typing(..),
    Type(..), Term(..), Position(..)
) where

import Phase2           ( Global, Position )

import Data.Map         ( Map )
import GHC.Generics
import Text.PrettyPrint.GenericPretty ( Out )
import Text.PrettyPrint.HughesPJClass ( Pretty(..), (<+>), text, maybeParens )


newtype Globals = Globals (Map Global Meaning)
  deriving (Show,Generic)

data Meaning = Meaning Typing Position Term Position
    deriving (Show,Generic)

-- TODO
--   add location information
--   tag bad typings to delay error reporting
data Typing     = Typing {
    locals      :: Map Local (Type, [(Position,Type)]),
    itype       :: Type
} deriving (Show,Generic)

type Local      = String

data Type
    = TyFun Type Type
    | TyApp Type Type
    | TyType
    | TyVar Local
    | TyQVar Local Type
  deriving (Eq, Ord, Show, Read, Generic)

data Term
    = Abs Local Term
    | Arr Term Term
    | App Term Term
    | Var Local
    | QVar Local Term
  deriving (Eq, Ord, Show, Read, Generic)


instance Out Globals
instance Out Meaning
instance Out Typing
instance Out Type
instance Out Term


instance Pretty Type where
    pPrintPrec _ p TyType =
        text "Type"
    pPrintPrec _ p (TyVar v) =
        text v
    pPrintPrec lvl p (TyFun t1 t2) =
        maybeParens
            (p >= 1)
            (pPrintPrec lvl 1 t1 <+> text "->" <+> pPrintPrec lvl 0 t2)
