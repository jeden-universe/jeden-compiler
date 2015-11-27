{-# LANGUAGE
    NoImplicitPrelude,
    Arrows,
    OverloadedStrings
  #-}

module Jeden.Parse (
        parseJeden
    ) where

import Arrow.ArrowMany
import qualified Arrow.Lexer as L
import Jeden.Types
import Util.Parser

import Control.Arrow (returnA, (>>>), (<<<), (<+>))
import Control.Category (Category(..))
import Data.ByteString (ByteString)
import Data.List (last, init, reverse, foldl, foldr)

import Prelude (($), (==))



{- Grammar
    module      ::= defblock*
    defblock    ::= decl def
    decl        ::= context? ':-' ident ':' type
    def         ::= typclause* | trmclause*

    trmclause   ::= clhead cltail
    clhead      ::= (pat+ '>=')? ident '=>' pat+
    cltail      ::= action*
    action      ::= (pat+ '>-')? ident ('->' pat+)?

    typclause   ::= (typlist '>:')? ident ':>' type
    typlist     ::= ( type | '(' type ')' )+

    context     ::= ctxelem (',' ctxelem)*
    ctxelem     ::= var ':' type

    # priorities [associativity]:
    #   type type [left]
    #   type -> type [right]
    #   type * type [right]
    typeexpr    ::= typeprod
    typeprod    ::= typefun ('*' typeprod)*
    typefun     ::= typeapp ('->' typefun)*
    typeapp     ::= typeatom (typeatom typeatom*)?
    typeatom    ::= 'Type' | ident | '(' typeexpr ')'

    var         ::= ident
    pat         ::= ident | '(' pat* ')'
    ident       ::= alphanum+
-}


parseJeden :: ByteString -> PResult Module
parseJeden = parse jmodule

-- Productions

type Prod a = Parser () a

jmodule :: Prod Module
jmodule = proc () -> do
    returnA -< Module [] []

decl :: Prod Decl
decl = proc () -> do
    ctx <- option [] context -< ()
    lex_ $ L.string ":-" -< ()
    trm <- ident -< ()
    lex_ $ L.char ':' -< ()
    typ <- typeexpr -< ()
    returnA -< Decl ctx trm typ

deftype :: Prod [Constr]
deftype =
    many typclause

defterm :: Prod [Clause]
defterm =
    many trmclause

typclause :: Prod Constr
typclause = proc () -> do
    src <- option TypUnit (try $ proc () -> do
        src <- typeexpr -< ()
        lex_ $ L.string ">:" -< ()
        returnA -< src
        ) -< ()
    name <- ident -< ()
    lex_ $ L.string ":>" -< ()
    tgt <- typeexpr -< ()
    returnA -< Constr src name tgt

trmclause :: Prod Clause
trmclause = proc () -> do
    src <- option [] (try $ proc () -> do
        src <- some pattern -< ()
        lex_ $ L.string ">=" -< ()
        returnA -< src
        ) -< ()
    name <- ident -< ()
    lex_ $ L.string "=>" -< ()
    tgt <- some pattern -< ()
    acts <- many action -< ()
    returnA -< Clause src name tgt acts

action :: Prod Action
action = proc () -> do
    src <- option [] (try $ proc () -> do
        src <- some pattern -< ()
        lex_ $ L.string ">-" -< ()
        returnA -< src
        ) -< ()
    name <- ident -< ()
    tgt <- option [] (proc () -> do
        lex_ $ L.string "->" -< ()
        some pattern -< ()
        ) -< ()
    returnA -< Action src name tgt

context :: Prod [(Ident,Type)]
context = proc () -> do
    c <- ctxelem -< ()
    cs <- many $ (lex_ $ L.char ',') >>> ctxelem -< ()
    returnA -< reverse $ c:cs

ctxelem :: Prod (Ident,Type)
ctxelem = proc () -> do
    v <- ident -< ()
    lex_ $ L.char ':' -< ()
    t <- typeexpr -< ()
    returnA -< (v,t)


typeexpr :: Prod Type
typeexpr =
    typeprod

typeprod :: Prod Type
typeprod = proc () -> do
    t <- typefun -< ()
    ts <- many $ (lex_ $ L.char '*') >>> typefun -< ()
    let typs = t:ts
    returnA -< foldr TypSigma (last typs) (init typs)

typefun :: Prod Type
typefun = proc () -> do
    t <- typeapp -< ()
    ts <- many $ (lex_ $ L.string "->") >>> typeapp -< ()
    let typs = t:ts
    returnA -< foldr TypPi (last typs) (init typs)

typeapp :: Prod Type
typeapp = proc () -> do
    t:ts <- some typeatom -< ()
    returnA -< foldl TypApp t ts

typeatom :: Prod Type
typeatom = proc () ->
        ( do
        lex $ L.string "Type" >>> L.notFollowedBy L.alphaNum -< ()
        returnA -< TypType
        )
    <+> ( do
        atom <- ident -< ()
        returnA -< TypConst atom
        )
    <+> ( do
        parens typeexpr -< ()
        )

pattern :: Prod Pat
pattern = proc () ->
        ( do
        atom <- ident -< ()
        returnA -< PatAtom atom
        )
    <+> (do
        (lex_ $ L.char '(') >>> somews -< ()
        ctr <- ident -< ()
        args <- pattern `sepBy` somews -< ()
        lex $ L.char ')' -< ()
        returnA -< PatApp ctr args
        )

parens :: Prod a -> Prod a
parens p =
    between
        (lex $ L.char '(')
        (lex $ L.char ')')
        p

-- Terminals: tokens captured as ByteString

type Terminal = Parser () ByteString

ident :: Terminal
ident =
    lex $ some L.alphaNum

-- Seperators: different kinds of white-space

type Separator = Parser () ()

manyws :: Separator
manyws =
    lex_ $ L.takeWhile (== ' ')

somews :: Separator
somews =
    lex_ $ some $ L.char ' '

manyvs :: Separator
manyvs =
    lex_ $ many $ L.word8 32 <+> L.word8 10

vskip :: Separator
vskip = proc () -> do
    manyws -< ()
    many ((lex_ $ L.word8 10) >>> pause >>> manyws) -< ()
    returnA -< ()
