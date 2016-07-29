
module Phase3 (
    phase3
) where

import Phase2 as P2 hiding ( Type(..) )
import qualified Phase2 as P2
import Phase3.AST as P3 hiding ( Globals(..) )
import Phase3.Type

import Control.Monad                ( Monad(..) )
import Control.Monad.Trans.Class    ( lift )
import Control.Monad.Trans.Except   ( ExceptT(..), throwE, runExceptT )
import Control.Monad.Trans.Reader   ( Reader, ask, runReader )
import Control.Monad.Trans.State    ( StateT(..), get, modify, evalStateT )
import Data.Map as M (
    Map, (\\), empty, singleton, lookup, findWithDefault,
    insert, delete,
    union, intersectionWith, mergeWithKey,
    fromAscList, toAscList, assocs
    )
import Text.Show                    ( Show(..), shows, showString )
import Text.PrettyPrint.GenericPretty ( pretty )

import Prelude hiding ( lookup )
-- ( ($), Int, Maybe(..), Num(..), String, (++), undefined )


phase3 :: Globals -> Either String Globals
-- check type definitions
-- gather term declarations and type definitions in a Context
-- type-check terms
phase3 globals = do
    checkTypeDefs globals
    checkTermDefs globals
    return globals


checkTypeDefs :: Globals -> Either String ()
checkTypeDefs (Globals globals) =
    Right ()

checkTermDefs :: Globals -> Either String ()
checkTermDefs (Globals globals) = do
    ctx <- buildContext (Globals globals)
    sequence_ $ map (check ctx) $ assocs globals
    where
        check ctx (ident, P2.MTypeDef _ _) = Right ()
        check ctx (ident, P2.MTermDef typ trm pos) = do
            Typing loc ityp <- runAlgoC ctx trm
            let dtyp = type2to3 typ
            case (morphism dtyp ityp, morphism ityp dtyp) of
                (Just _, Just _) -> Right ()
                _                -> Left
                    $ shows pos . showString ":\n"
                    . showString "  • Type mismatch in definition of " . showString ident . showChar '\n'
                    . showString "      declared :: " . shows dtyp . showChar '\n'
                    . showString "      inferred :: " . shows ityp . showChar '\n'
                    $ ""

buildContext :: Globals -> Either String Context
buildContext (Globals globals) = do
    globals' <- sequence $ map convert $ toAscList globals
    return $ Context $ fromAscList globals'
    where
        convert (ident, P2.MTypeDef typ pos) =
            Right (ident, (pos,Typing empty TyType))

        convert (ident, P2.MTermDef typ trm pos) =
            Right (ident, (pos,Typing empty (type2to3 typ)))

        convert (ident, P2.MTermDecl typ pos) =
            Left $ shows pos . showString ":\n"
                 . showString "  Term declaration " . showString ident
                 . showString " has no associated definition.\n"
                 $ ""

type2to3 :: P2.Type -> P3.Type
type2to3 (P2.TyVar (Symbol (pos,v))) = P3.TyVar v
type2to3 (P2.TyFun t1 t2) = P3.TyFun (type2to3 t1) (type2to3 t2)

{-| Monadic environment for type-checking using Algorithm C.

  * State: an integer to generate fresh variables
  * Except: an error String
  * Reader: Context containing global declarations
-}
type AlgoC a = ExceptT String (StateT Int (Reader Context)) a

runAlgoC :: Context -> Term -> Either String Typing
runAlgoC ctx trm =
    runReader (evalStateT (runExceptT (algoC trm)) 0) ctx

{-| Infer the type of the given expression using Algorithm C.
-}
algoC :: Term -> AlgoC Typing
algoC (Var (Symbol (pos,x))) = do
    (Context ctx) <- lift $ lift $ ask
    case lookup x ctx of
        Nothing -> do
            a <- new
            return $ Typing (M.singleton x a) a
        Just (pos,tp) -> do
            inst tp
algoC (Abs (Symbol (pos,x)) e) = do
    tp <- algoC e
    case lookup x (locals tp) of
        Just t' ->
            return $ Typing
                (delete x (locals tp))
                (TyFun t' (itype tp))
        Nothing -> do
            a <- new
            return $ tp { itype = TyFun a (itype tp) }
algoC (App f e) = do
    tp1 <- algoC f
    tp2 <- algoC e
    beta <- new
    case morphism (TyFun (itype tp2) beta) (itype tp1) of
        Just s ->
            case mapjoin (locals tp1) (fmap (appSubst s) (locals tp2)) of
                Just mr ->
                    return $ Typing mr (appSubst s beta)
                Nothing ->
                    throwE "fail: algoC"
        Nothing ->
            throwE $
                "Type error:\n"
                ++ "  in application\n"
                ++ "    of: " ++ pretty f
                ++ "\n       type: " ++ pretty (itype tp1)
                ++ "\n    on: " ++ pretty e
                ++ "\n       type: " ++ pretty (TyFun (itype tp2) beta)
                ++ "\n\n"

{-| Create a fresh variable.

Currently, such variables have names such as `t23`.
-}
new :: AlgoC Type
new = do
    n <- lift get
    lift $ modify incr
    return $ TyVar $ "t" ++ show n
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
