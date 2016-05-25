
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
  deriving (Show,Eq)

{-| The type of pointless lambda expressions, well-typed closed Lambda
expressions.
-}
data PLambda
  = PLambda SLambda PType
  deriving (Show,Eq)

{-| Constructor for 'PLambda' expressions, i.e., closed typeable lambda terms, from
a syntactic lambda expression of type 'SLambda'.
-}
plambda :: SLambda -> Either String PLambda
plambda e = do
    tp <- runReader (evalStateT (runExceptT (algoC e)) 0) emptyCtx
    if M.null (fvars tp)
    then return $ PLambda e (ptype tp)
    else Left $
        "undefined variable"
        ++ (if M.size (fvars tp) == 1 then ":\n" else "s:\n")
        ++ M.foldMapWithKey (\k a -> "  " ++ k ++ " : " ++ show a ++ "\n") (fvars tp)

data Typing = Typing {
    fvars       :: Map Var PType,
    ptype       :: PType
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
                (TFun t' (ptype tp))
        Nothing -> do
            a <- new
            return $ tp { ptype = TFun a (ptype tp) }
algoC (SApp f e) = do
    tp1 <- algoC f
    tp2 <- algoC e
    beta <- new
    case morphism (TFun (ptype tp2) beta) (ptype tp1) of
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
    return $ Typing undefined (ptype tp)

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

instance P.Pretty PLambda where
    pPrintPrec lvl prec (PLambda slam typ) = P.maybeParens (prec > 0 % 1) $
        P.pPrintPrec lvl (0 % 1) slam P.<+> P.char ':' P.<+> P.pPrintPrec lvl (0 % 1) typ
        
