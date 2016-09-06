
module Phase3 (
    phase3
) where

import Phase2 as P2 hiding ( Typing(..), Type(..), Globals(..) )
import qualified Phase2 as P2
import Phase3.AST as P3
import Phase3.Type

import Control.Monad                ( Monad(..) )
import Control.Monad.Trans.Class    ( lift )
import Control.Monad.Trans.Except   ( ExceptT(..), throwE, runExceptT )
import Control.Monad.Trans.Reader   ( Reader, ask, runReader )
import Control.Monad.Trans.State    ( StateT(..), get, modify, evalStateT )
import Data.Map as M (
    Map, (!), (\\), empty, singleton,
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


phase3 :: Module -> Either String Globals
phase3 (Module modName stmts) =
    checkStatements (Globals empty) stmts

checkStatements :: Globals -> [Statement] -> Either String Globals
checkStatements globals [] = return globals
checkStatements (Globals globals) (stmt : stmts) = do
    checked <- checkStatement globals stmt
    checkStatements
        (Globals $ insert * checked globals)
        stmts

checkStatement :: Globals -> Statement -> Either String Globals
checkStatement (Globals globals) (TermDecl sym dtyping) =
    case lookup globals of
        Just _ ->
            Left "symbol already declared"
        Nothing -> do
            ityping <- inferKind globals (ityp dtyping)
            -- reduce both before comparing?
            case equivTyping ityping dtyping of
                Just _ _ ->
                    return $ Globals $ insert sym (MDecl dtyping pos)
                Nothing ->
                    Left "invalid typing"

checkStatement (Globals globals) (TermDef sym trm) =
    case lookup globals of
        Just (MDecl typ dpos) -> do
            Typing loc ityp <- inferType globals trm
            let dtyp = type2to3 typ
            case subtype ityp dtyp of
                Just _  -> Right
                    $ Globals $ insert sym (reduce globals trm)
                Nothing -> Left
                    $ shows pos . showString ":\n"
                    . showString "  • Type mismatch in definition of " . showString ident . showChar '\n'
                    . showString "      declared :: " . pretty dtyp . showChar '\n'
                    . showString "      inferred :: " . pretty ityp . showChar '\n'
                    $ ""
        Just (MBoth _ _ _ dpos) ->
            Left "symbol already defined"
        Nothing ->
            Left "symbol defined but not declared"

reduce :: Globals -> Term -> Term
reduce = undefined


checkTermDefs :: P2.Globals -> Either String ()
checkTermDefs (P2.Globals globals) = do
    ctx <- buildContext (Globals globals)
    sequence_ $ map (check ctx) $ assocs globals
    where
        check ctx (ident, P2.MDecl _ _) = Right ()
        check ctx (ident, P2.MBoth typ posd trm pos) = do
            Typing loc ityp <- runAlgoC ctx trm
            let dtyp = reduce ctx (type2to3 typ)
            case subtype ityp dtyp of
                Just _  -> Right ()
                Nothing -> Left
                    $ shows pos . showString ":\n"
                    . showString "  • Type mismatch in definition of " . showString ident . showChar '\n'
                    . showString "      declared :: " . pretty dtyp . showChar '\n'
                    . showString "      inferred :: " . pretty ityp . showChar '\n'
                    $ ""

buildContext :: P2.Globals -> Either String Context
buildContext (P2.Globals globals) = do
    globals' <- sequence $ map convert $ toAscList globals
    return $ Context $ fromAscList globals'
    where
        convert (ident, P2.MBoth typ posd trm pos) =
            Right (ident, (pos,Typing empty (type2to3 typ)))

        convert (ident, P2.MDecl typ pos) =
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
    unifyTypings tp1 tp2 { itype = TyFun (itype tp2) beta }

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
        . showString "  No support for instantiating associated local variables.\n"
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

{-| Unification of typings. It is assumed that the arguments
represent a lambda application.
-}
unifyTypings :: Typing -> Typing -> AlgoC Typing
unifyTypings tp1 tp2 = do
    -- 1) on the intersection of the typing contexts:
    --   gather substitution pairs from coproducts
    -- subs :: Map String (Subst,Subst)
    subs <- sequence $ M.intersectionWithKey
            (\k s t -> case coproduct s t of
                Just (sub1,sub2) ->
                    return (sub1,sub2)
                Nothing -> throwE
                    $ showString "Type error:\n"
                    . showString "  • Incompatible types for " . showString k . showChar '\n'
                    . showString "    :: " . pretty s . showChar '\n'
                    . showString "    :: " . pretty t . showChar '\n'
                    . showChar '\n'
                    $ ""
            ) (locals tp1) (locals tp2)

    -- 2) on the types:
    --      build a substitution pair from the coproduct
    itpair <- case coproduct (itype tp1) (itype tp2) of
        Just p  -> return p
        Nothing -> throwE
            $ showString "Type error:\n"
            . showString "  • Incompatible types in function application\n"
            . showString "      of" . showChar '\n'
            . showString "      on" . showChar '\n'
            $ ""

    -- 3) on all the collected substitution pairs
    --      fold coherently
    let (sub1,sub2) = foldr
            (\(Subst s1,Subst s2) (Subst t1,Subst t2) ->
                    ( Subst $ t1 `M.union` fmap (appSubst $ Subst t1) s1
                    , Subst $ t2 `M.union` fmap (appSubst $ Subst t2) s2
                    )
            )
            itpair
            subs

    -- 4) on the differences of the typing contexts:
    --      apply the corresponding substitution
    let locals1 = fmap (appSubst sub1) (locals tp1 \\ locals tp2)
    let locals2 = fmap (appSubst sub2) (locals tp2 \\ locals tp1)

    -- 5) produce the left-biased unified typing
    let localsU = M.intersectionWith
            (\_ t -> appSubst sub2 t)
            (locals tp1) (locals tp2)
    return $ Typing
        (locals1 `M.union` locals2 `M.union` localsU)
        (appSubst sub2 (case itype tp2 of TyFun _ b -> b))  -- quick hack
