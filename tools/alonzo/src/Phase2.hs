{-# LANGUAGE DeriveGeneric #-}

module Phase2 (
    Module(..), Statement(..), Type(..), Term(..), Symbol(..),
    Globals(..), Global(..), Meaning(..),
    phase2
) where


import Phase1 (
    Module(..), Statement(..), Type(..), Term(..), Symbol(..) )

import Data.Map         ( Map, empty, lookup, insert, assocs )
import GHC.Generics ( Generic )
import Text.PrettyPrint.GenericPretty ( Out(..) )
import Prelude ( ($), (.), Either(..), Int, Maybe(..), Show, String )

-----
-- begin AST

newtype Globals = Globals (Map Global Meaning) deriving (Show,Generic)
type Global     = String

type Position   = (Int,Int)

data Meaning    = MTypeDef  Type Position
                | MTermDecl Type Position
                | MTermDef  Type Term Position
    deriving (Show,Generic)

instance Out Globals
instance Out Meaning
instance (Out k, Out a) => Out (Map k a) where
    docPrec p = docPrec p . assocs
    doc = docPrec 0

-- end AST
-----

phase2 :: Module -> Either String Globals
phase2 (Module modName stmts) = go empty stmts
    where
        go :: Map Global Meaning -> [Statement] -> Either String Globals

        go globals (TypeDef (Symbol (pos,ident)) typ : stmts) =
            case lookup ident globals of
                Nothing  ->
                    go (insert ident (MTypeDef typ pos) globals) stmts

        go globals (TermDecl (Symbol (pos,ident)) typ : stmts) =
            case lookup ident globals of
                Nothing  ->
                    go (insert ident (MTypeDef typ pos) globals) stmts

        go globals (TermDef (Symbol (pos,ident)) trm : stmts) =
            case lookup ident globals of
                Just (MTypeDef typ _) ->
                    go (insert ident (MTermDef typ trm pos) globals) stmts
                Nothing ->
                    Left $ "Term defined but not declared"

        go globals [] = Right (Globals globals)
