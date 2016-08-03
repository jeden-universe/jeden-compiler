
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
    Map, (\\), empty, singleton,
    null, lookup, findWithDefault,
    insert, delete,
    union, intersectionWith, intersectionWithKey, mergeWithKey,
    fromAscList, toAscList, assocs
    )
import Text.PrettyPrint.HughesPJClass  ( Pretty(..), render )
-- import Text.Show                    ( Show(..), ShowS, shows, showString )
-- import Text.PrettyPrint.GenericPretty ( pretty )

import Debug.Trace                  ( trace, traceM )

import Prelude hiding ( null, lookup )
-- ( ($), Int, Maybe(..), Num(..), String, (++), undefined )


pretty :: Pretty a => a -> ShowS
pretty = showString . render . pPrint


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
                    . showString "      declared :: " . pretty dtyp . showChar '\n'
                    . showString "      inferred :: " . pretty ityp . showChar '\n'
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

-- TODO  replace by this correct(?) algorithm
-- 1) on the intersection:
--   take coproducts
--   gather all substitutions into a single coherent pair
-- 2) on the differences:
--   apply the corresponding substitution

-- TODO add a constructor for /bad/ typings, to delay error reporting
--   when comparing to the declaration
unifyTypings :: Map Local Type -> Map Local Type -> Either String (Map Local Type)
unifyTypings m1 m2 = do
    -- the whole following if WRONG
    mu <- sequence $ M.intersectionWithKey
        -- (\k s t -> case coproduct s t of
        --     Just (sub1,_) -> Right $ appSubst sub1 s
        --     Nothing     -> Left
        (\k s t -> if s == t then Right t else Left
                $ showString "Type error:\n"
                . showString "  • Incompatible types for " . showString k . showChar '\n'
                . showString "    :: " . pretty s . showChar '\n'
                . showString "    :: " . pretty t . showChar '\n'
                . showChar '\n'
                $ ""
        ) m1 m2
    return $ (m1 \\ m2) `M.union` (m2 \\ m1) `M.union` mu


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
-- TODO: Typings should contain a position so that unification error
--   messages can provide locations
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
    -- TODO unify typings
    case coproduct (itype tp1) (TyFun (itype tp2) beta) of
        Just (sub1,sub2) -> do
            traceM $ showString "coproduct "
                   . showParen True (pretty $ itype tp1) . showChar ' '
                   . showParen True (pretty $ TyFun (itype tp2) beta) . showChar '\n'
                   . shows sub1 . showChar '\n'
                   . shows sub2 . showChar '\n'
                   $ ""
            -- unify typings
            case unifyTypings
                    (fmap (appSubst sub1) $ locals tp1)
                    (fmap (appSubst sub2) $ locals tp2) of
                Right locs  -> do
                    let typ = appSubst sub2 beta
                    return $ Typing locs typ
                Left err ->
                    throwE err
        Nothing ->
            throwE
                $ showString "Type error:\n"
                . showString "  • Incompatible types in function application\n"
                . showString "      of" . showChar '\n'
                . showString "      on" . showChar '\n'
                $ ""

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
inst (Typing locals itype)
    | null locals = do
        (_,otype) <- go empty itype
        return $ Typing empty otype
    | otherwise   = error
        $ showString "Internal error:\n"
        . showString "  No support for instantiating associated locals variables.\n"
        $ ""
    where
        go :: Map TVar Type -> Type -> AlgoC (Map TVar Type,Type)
        go subst (TyVar a) =
            case lookup a subst of
                Just b  -> return (subst, b)
                Nothing -> do
                    b <- new
                    return (insert a b subst, b)
        go subst (TyFun s t) = do
            (subst,u) <- go subst s
            (subst,v) <- go subst t
            return (subst, TyFun u v)



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
