
{-|
Module          : Pointless.Lambda
Description     : The terms of pointless type theory
Copyright       : (c) 2016 Nicolas Godbout
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : experimental

The well-formed terms of pointless type theory are closed typed lambda terms.
To build a lambda term, one first presents it as a _syntactical_ object of
type 'SLambda'. In litterature, such an object is often called a _pre-term_,
to convey the fact that it is not guaranteed to be a well-formed term.

Well-formed terms are generated using the function 'plambda' which, presented
with a syntactical 'SLambda' term, produces a well-formed 'PLambda' term or
generates an error message.

-}
module Pointless.Lambda (
    Var,
    SLambda(..),
    PLambda,
    plambda
)
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Map (Map)
import qualified Data.Map as M

import Data.Ratio ((%))
import qualified Text.PrettyPrint.HughesPJClass as P

import Pointless.Type

{-| A term variable is just a 'String'.
-}
type Var = String

{-| The type of syntactic lambda expressions, also called pre-terms.
-}
data SLambda
  = SVar Var
  | SAbs Var SLambda
  | SApp SLambda SLambda
  | SBnd SLambda [(Var,SLambda)]
  deriving (Show)

-- internal definition for whnf reduction
-- should only be called on terms known to be closed
--   otherwise this can spin or crash
whnf :: SLambda -> SLambda
whnf (SAbs x e) = SAbs x e
whnf (SApp f c) = whnf (beta (SApp f c))
whnf (SBnd e s) = whnf (sigma (SBnd e s))

-- single-step sigma reduction
sigma :: SLambda -> SLambda
sigma (SBnd m [])
    = m
sigma (SBnd (SVar v) s)
    = case lookup v s of
        Just p  -> p
        Nothing -> SVar v
sigma (SBnd (SAbs v m) s)
    = SAbs v (SBnd m (filter (f v) s)) where f v (x,_) = x /= v
sigma (SBnd (SApp m n) s)
    = SApp (SBnd m s) (SBnd n s)
sigma (SBnd (SBnd m s) t)
    = (SBnd m (s ++ t))
sigma m
    = m

-- single-step beta reduction
beta :: SLambda -> SLambda
beta (SApp (SAbs v m) n) = SBnd m [(v,n)]
beta m = m


{-| The type of pointless lambda expressions, well-typed closed Lambda
expressions.
-}
data PLambda
  = PLambda SLambda PType
  deriving (Show)

pterm :: PLambda -> SLambda
pterm (PLambda e _) = e

ptype :: PLambda -> PType
ptype (PLambda _ t) = t

{-| Constructor for 'PLambda' expressions, i.e., closed typeable lambda terms, from
a syntactic lambda expression of type 'SLambda'.
-}
plambda :: SLambda -> Either String PLambda
plambda e = do
    tp <- runReader (evalStateT (runExceptT (algoC e)) 0) emptyCtx
    if M.null (fvars tp)
    then return $ PLambda e (itype tp)
    else Left $
        "undefined variable"
        ++ (if M.size (fvars tp) == 1 then ":\n" else "s:\n")
        ++ M.foldMapWithKey (\k a -> "  " ++ k ++ " : " ++ show a ++ "\n") (fvars tp)

psrc :: PLambda -> PType
psrc (PLambda _ (TFun src _)) = src

ptgt :: PLambda -> PType
ptgt (PLambda _ (TFun _ tgt)) = tgt

punit :: PLambda
punit = PLambda (SAbs "x" (SVar "x")) (TFun (TVar "a") (TVar "a"))

pid :: PType -> PLambda
pid t = PLambda (SAbs "x" (SVar "x")) (TFun t t)

infixr 9 ⚬
(⚬) = pcompose

-- TODO: fix the type variable scope issue
pcompose :: PLambda -> PLambda -> Either String PLambda
pcompose g f =
    case morphism (psrc g) (ptgt f) of
        Just s  -> Right $ PLambda -- \g f x. g (f x)
            (SAbs "g" (SAbs "f" (SAbs "x" (SApp (pterm g) (SApp (pterm f) (SVar "x"))))))
            (TFun (psrc f) (appSubst s $ ptgt g))
        Nothing -> Left "fail!"

infixr 7 ∙
(∙) = peval

-- TODO: fix the type variable scope issue
peval :: PLambda -> PLambda -> Either String PLambda
peval f x =
    case morphism (psrc f) (ptype x) of
        Just s  -> Right $ PLambda
            (whnf $ SApp (pterm f) (pterm x))
            (appSubst s $ ptgt f)
        Nothing -> Left "fail!"

data Typing = Typing {
    fvars       :: Map Var PType,
    itype       :: PType
}

newtype Context
  = Context (Map Var Typing)

emptyCtx :: Context
emptyCtx = Context M.empty

type AlgoC a = ExceptT String (StateT Int (Reader Context)) a

algoC :: SLambda -> AlgoC Typing
algoC (SVar x) = do
    (Context ctx) <- lift $ lift $ ask
    case M.lookup x ctx of
        Nothing -> do
            a <- new
            return $ Typing (M.singleton x a) a
        Just tp -> do
            inst tp
algoC (SAbs x e) = do
    tp <- algoC e
    case M.lookup x (fvars tp) of
        Just t' ->
            return $ Typing
                (M.delete x (fvars tp))
                (TFun t' (itype tp))
        Nothing -> do
            a <- new
            return $ tp { itype = TFun a (itype tp) }
algoC (SApp f e) = do
    tp1 <- algoC f
    tp2 <- algoC e
    beta <- new
    case morphism (TFun (itype tp2) beta) (itype tp1) of
        Just s ->
            case mjoin (fvars tp1) (fmap (appSubst s) (fvars tp2)) of
                Just mr ->
                    return $ Typing mr (appSubst s beta)
                Nothing ->
                    throwE "fail!"
        Nothing ->
            throwE "fail!"

new :: AlgoC PType
new = do
    n <- lift get
    lift $ modify incr
    return $ TVar $ "t" ++ show n
    where
        incr :: Int -> Int
        incr n = case n + 1 of r -> r

inst :: Typing -> AlgoC Typing
inst tp =
    return $ Typing undefined (itype tp)

mjoin :: Map Var PType -> Map Var PType -> Maybe (Map Var PType)
mjoin m1 m2 = do
    mu <- sequence $ M.intersectionWith
        (\s t -> case coproduct s t of
            Just (_,st) -> Just $ appSubst st t
            Nothing     -> Nothing
        ) m1 m2
    return $ m1 M.\\ m2 `M.union` m2 M.\\ m1 `M.union` mu


instance P.Pretty SLambda where
    pPrintPrec lvl prec (SVar x) = P.text x
    pPrintPrec lvl prec (SAbs x e) = P.maybeParens (prec > 0 % 1) $
        P.char '\\' P.<> P.text x P.<> P.char '.' P.<+> P.pPrintPrec lvl (0 % 1) e
    pPrintPrec lvl prec (SApp m n) = P.maybeParens (prec > 0 % 1) $
        P.pPrintPrec lvl (0 % 1) m P.<+> P.pPrintPrec lvl (1 % 1) n
    pPrintPrec lvl prec (SBnd e s) =
        P.pPrintPrec lvl (1 % 1) e P.<> printBinds lvl s
        where
            printBinds lvl [] = P.text "[]"
            printBinds lvl [(x,e)] =
                P.char '[' P.<> P.pPrintPrec lvl (0 % 1) e P.<> P.char '/' P.<> P.text x P.<> P.char ']'
            printBinds lvl ((x,e) : bs) =
                P.char '[' P.<> P.pPrintPrec lvl (0 % 1) e P.<> P.char '/' P.<> P.text x P.<> P.char ']'
                P.<> printBinds lvl bs

instance P.Pretty PLambda where
    pPrintPrec lvl prec (PLambda slam typ) = P.maybeParens (prec > 0 % 1) $
        P.pPrintPrec lvl (0 % 1) slam P.<+> P.char ':' P.<+> P.pPrintPrec lvl (0 % 1) typ
