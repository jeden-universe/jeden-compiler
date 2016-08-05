{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}


module Phase1 (
    -- | * Abstract Syntax
    Module(..), ModName(..), Statement(..),
    Typing(..), CtxElem(..),
    Type(..), Term(..), Symbol(..),
    -- | * Functions
    phase1
) where

import LexJeden     ( tokens )
import ParJeden     ( pModule )
-- import PrintJeden
import AbsJeden     (
    Module(..), ModName(..), Statement(..),
    Typing(..), CtxElem(..),
    Type(..), Term(..), Symbol(..) )
import ErrM         ( Err(..) )

import GHC.Generics
import Text.PrettyPrint.GenericPretty ( Out )

deriving instance Generic (Module)
deriving instance Generic (ModName)
deriving instance Generic (Statement)
deriving instance Generic (Typing)
deriving instance Generic (CtxElem)
deriving instance Generic (Type)
deriving instance Generic (Term)
deriving instance Generic (Symbol)

instance Out Module
instance Out ModName
instance Out Statement
instance Out Typing
instance Out CtxElem
instance Out Type
instance Out Term
instance Out Symbol


phase1 :: String -> Either String Module
phase1 input = case pModule $ tokens input of
    Ok m  -> Right m
    Bad e -> Left e
