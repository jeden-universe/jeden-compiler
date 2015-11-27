
module Jeden.Types
where

import Data.ByteString (ByteString)

import Prelude (
        Int,
        Eq, Show
    )

type Ident = ByteString

data Type
  = TypType
  | TypVar Int
  | TypConst Ident
  | TypUnit
  | TypSigma Type Type
  | TypPi Type Type
  | TypApp Type Type
  deriving (Eq, Show)


-- Abstract Syntax Tree, intermediate representation after parsing

data Module = Module {
        types   :: [(Decl,[Constr])],
        terms   :: [(Decl,[Clause])]
    }

data Decl
  = Decl Context Ident Type
  deriving (Eq, Show)

type Context = [(Ident,Type)]

data Pat
  = PatAtom Ident
  | PatApp Ident [Pat]
  deriving (Eq, Show)

data Constr
  = Constr Type Ident Type
  deriving (Eq, Show)

data Clause
  = Clause [Pat] Ident [Pat] [Action]
  deriving (Eq, Show)

data Action
  = Action [Pat] Ident [Pat]
  deriving (Eq, Show)
