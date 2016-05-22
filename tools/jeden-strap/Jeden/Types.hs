
module Jeden.Types
where

import Data.ByteString (ByteString)

import Prelude (
        Int,
        Eq, Show
    )

type Ident = ByteString
type Level = Int            -- plug before we have a genuine Level system

data Type
  = TypVar Int
  | TypConst Ident
  | TypUnit
  | TypForall Type Type
  | TypExists Type Type
  | TypW Type Type
  | TypApp Type Type
  | TypeU Level
  | TypeEl Level Type
  deriving (Eq, Show)

-- type Term = Clause

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
