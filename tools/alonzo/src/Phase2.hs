{-# LANGUAGE DeriveGeneric #-}

module Phase2 (
    module Phase1,
    -- Module(..), Statement(..), Type(..), Term(..), Symbol(..),
    Position(..),
    Globals(..), Global(..), Meaning(..),
    phase2
) where


import Phase1 (
    Module(..), Statement(..),
    Typing(..), CtxElem(..),
    Type(..), Term(..), Symbol(..) )
import Data.Map         ( Map, empty, lookup, insert, assocs )
import GHC.Generics     ( Generic )
import Text.PrettyPrint.GenericPretty ( Out(..) )
import Text.Show        ( Show(..), shows, showString )
import Prelude ( ($), (.), Either(..), Int, Maybe(..), String )

-----
-- begin AST

newtype Globals = Globals (Map Global Meaning) deriving (Show,Generic)
type Global     = String

type Position   = (Int,Int)

data Meaning    = MDecl Typing Position
                | MBoth Typing Position Term Position
    deriving (Show,Generic)

instance Out Globals
instance Out Meaning
instance (Out k, Out a) => Out (Map k a) where
    docPrec p = docPrec p . assocs
    doc = docPrec 0

-- end AST
-----

{- | Phase 2 transformation.  Gather all symbols into a dictionary.
Check that defined terms are previously declared.
-}
phase2 :: Module -> Either String Globals
phase2 (Module modName stmts) = go empty stmts
    where
        go :: Map Global Meaning -> [Statement] -> Either String Globals

        go globals (TermDecl (Symbol (pos,ident)) typing : stmts) =
            case lookup ident globals of
                Nothing  ->
                    go (insert ident (MDecl typing pos) globals) stmts
                -- TODO Just

        go globals (TermDef (Symbol (pos2,ident)) term : stmts) =
            case lookup ident globals of
                Just (MDecl typing pos1) ->
                    go (insert ident (MBoth typing pos1 term pos2) globals) stmts
                Nothing ->
                    Left $ shows pos2 . showString ":\n"
                         . showString "  Term defined but not declared\n"
                         $ ""

        go globals [] = Right (Globals globals)
