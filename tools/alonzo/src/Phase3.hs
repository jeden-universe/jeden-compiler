
module Phase3 (
    phase3
) where

import Phase2 as P2
import Phase3.AST as P3

import Control.Monad                ( Monad(..) )
import Control.Monad.Trans.Class    ( lift )
import Control.Monad.Trans.Except   ( ExceptT(..), throwE )
import Control.Monad.Trans.Reader   ( Reader, ask )
import Control.Monad.Trans.State    ( StateT(..), get, modify )
import Data.Either                  ( Either(..) )
import Data.Map as M (
    Map, (\\), empty, singleton, lookup, insert, delete, union
    )

import Prelude ( ($), Int )


phase3 :: P2.Globals -> Either String P3.Globals
-- check type definitions
-- gather term declarations and type definitions in a Context
-- type-check terms



{-| Monadic environment for type-checking using Algorithm C.

  * State: an integer to generate fresh variables
  * Except: an error String
  * Reader: Context containing global declarations
-}
type AlgoC a = ExceptT String (StateT Int (Reader Context)) a

{-| Infer the type of the given expression using Algorithm C.
-}
algoC :: Term -> AlgoC Typing
algoC (Var x) = do
    (Context ctx) <- lift $ lift $ ask
    case lookup x ctx of
        Nothing -> do
            a <- new
            return $ Typing (singleton x a) a
        Just tp -> do
            inst tp
algoC (Abs x e) = do
    tp <- algoC e
    case lookup x (locals tp) of
        Just t' ->
            return $ Typing
                (delete x (locals tp))
                (TFun t' (itype tp))
        Nothing -> do
            a <- new
            return $ tp { itype = TFun a (itype tp) }
algoC (App f e) = do
    tp1 <- algoC f
    tp2 <- algoC e
    beta <- new
    case morphism (TFun (itype tp2) beta) (itype tp1) of
        Just s ->
            case mjoin (locals tp1) (fmap (appSubst s) (locals tp2)) of
                Just mr ->
                    return $ Typing mr (appSubst s beta)
                Nothing ->
                    throwE "fail: algoC"
        Nothing ->
            throwE $
                "Type error:\n"
                ++ "  in application\n"
                ++ "    of: " ++ P.render (P.pPrint f)
                ++ "\n       type: " ++ P.render (P.pPrint $ itype tp1)
                ++ "\n    on: " ++ P.render (P.pPrint e)
                ++ "\n       type: " ++ P.render (P.pPrint $ TFun (itype tp2) beta)
                ++ "\n\n"

{-| Create a fresh variable.

Currently, such variables have names such as `t23`.
-}
new :: AlgoC Type
new = do
    n <- lift get
    lift $ modify incr
    return $ TVar $ fromString $ "t" ++ show n
    where
        incr :: Int -> Int
        incr n = case n + 1 of r -> r

{-| Instantiate a typing, i.e., replace all free variables with
fresh variables in the local context. This operation is also called
skolemization.
-}
inst :: Typing -> AlgoC Typing
inst tp =
    return $ Typing undefined (itype tp)

{-| Attempt to join two maps together, provided they agree on their
intersection. Agreements means that multiple types associated with
local names are unifiable. The generated map contains such unified
types. The operation is left-biased.
-}
mapjoin :: Map Local Type -> Map Local Type -> Maybe (Map Local Type)
mapjoin m1 m2 = do
    mu <- sequence $ M.intersectionWith
        (\s t -> case coproduct s t of
            Just (_,st) -> Just $ appSubst st t
            Nothing     -> Nothing
        ) m1 m2
    return $ m1 \\ m2 `M.union` m2 \\ m1 `M.union` mu
