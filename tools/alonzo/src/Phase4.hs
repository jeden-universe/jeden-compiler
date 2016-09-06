
module Phase4 where

import Control.Monad                ( guard )
import Control.Monad.State          ( State, get, modify )
import Control.Monad.Trans.Class    ( lift )
import Control.Monad.Trans.Maybe    ( MaybeT(..) )
import Data.Foldable                ( foldrM, foldlM )
import Data.Map as M
import Data.Set as S

type Symbol = String

data ProtoType
  = Kind Int
  | Var  Symbol
  | Pi   Symbol ProtoType ProtoType
  | Lam  Symbol ProtoType
  | App  ProtoType ProtoType
  deriving (Eq)

freeVars :: ProtoType -> Set Symbol
freeVars (Kind _)     = S.empty
freeVars (Var a)      = S.singleton a
freeVars (Pi _ t1 t2) = freeVars t1 `S.union` freeVars t2
freeVars (Lam a t)    = S.delete a (freeVars t)
freeVars (App t1 t2)  = freeVars t1 `S.union` freeVars t2

newtype Subst
  = Subst { unSubst :: Map Symbol ProtoType }

domSubst :: Subst -> Set Symbol
domSubst = M.keysSet . unSubst

codSubst :: Subst -> Set Symbol
codSubst = foldMap freeVars . unSubst

idSubst :: Set Symbol -> Subst
idSubst = Subst . M.fromAscList . fmap (\a -> (a,Var a)) . S.toAscList

compSubst :: Subst -> Subst -> Subst
compSubst s2 s1 = Subst $ fmap (typeSubst s2) (unSubst s1)

joinSubst :: Subst -> Subst -> Maybe Subst
joinSubst (Subst s1) (Subst s2) = do
    guard $ and $ M.intersectionWith (==) s1 s2
    return $ Subst $ s1 `M.union` s2

typeSubst :: Subst -> ProtoType -> ProtoType
typeSubst s (Kind n)             = Kind n
typeSubst (Subst s) (Var a)      = s ! a
typeSubst s (Pi a t1 t2)         =
    let s' = s \$ a in             Pi a (typeSubst s' t1) (typeSubst s' t2)
typeSubst s (Lam a t)            =
    let s' = s \$ a in             Lam a (typeSubst s' t)
typeSubst s (App t1 t2)          = App (typeSubst s t1) (typeSubst s t2)

(\$) :: Subst -> Symbol -> Subst
(Subst s) \$ a = Subst (M.delete a s)

subType :: ProtoType -> ProtoType -> Maybe Subst
subType (Var a) t2 =
    return $ Subst (M.singleton a t2)
subType (Kind m) (Kind n) =
    error "I am not done yet!"
subType (Pi a a1 a2) (Pi b b1 b2) = do
    s1 <- subType a1 b1
    s2 <- subType a2 b2
    s <- s1 `joinSubst` (s2 \$ a)
    return s
subType (Lam a a1) (Lam b b1) = do
    s <- subType a1 b1
    s `joinSubst` Subst (M.singleton a (Var b))
    return $ s \$ a
subType (App a1 a2) (App b1 b2) = do
    s1 <- subType a1 b1
    s2 <- subType a2 b2
    s <- s1 `joinSubst` s2
    return s

coproductType :: ProtoType -> ProtoType -> Maybe (Subst,Subst)
coproductType (Kind m) (Kind n) =
    error "I am not done yet!"
coproductType (Var a) t2 =
    return (Subst $ M.singleton a t2, idSubst (freeVars t2))
coproductType t1 (Var b) =
    return (idSubst (freeVars t1), Subst $ M.singleton b t1)
coproductType (Pi a a1 a2) (Pi b b1 b2) = do
    (s1,s3) <- coproductType a1 b1
    (s2,s4) <- coproductType a2 b2
    s5 <- joinSubst s1 s2
    s6 <- joinSubst s3 s4
    return (s5,s6)
coproductType (App a1 a2) (App b1 b2) = do
    (s1,s3) <- coproductType a1 b1
    (s2,s4) <- coproductType a2 b2
    s5 <- joinSubst s1 s2
    s6 <- joinSubst s3 s4
    return (s5,s6)


newtype Context
  = Ctx { unCtx :: Map Symbol ProtoType }
  deriving (Eq)

domCtx :: Context -> Set Symbol
domCtx = M.keysSet . unCtx

codCtx :: Context -> Set Symbol
codCtx = foldMap freeVars . unCtx

joinCtx :: Context -> Context -> Maybe Context
joinCtx (Ctx c1) (Ctx c2) = do
    guard $ and $ M.intersectionWith (==) c1 c2
    return $ Ctx $ c1 `M.union` c2

emptyCtx :: Context
emptyCtx = Ctx M.empty

insertCtx :: Symbol -> ProtoType -> Context -> Maybe Context
insertCtx a t (Ctx ctx) = do
    guard $ M.notMember a ctx
    guard $ freeVars t `isSubsetOf` domCtx (Ctx ctx)
    return $ Ctx (M.insert a t ctx)

ctxSubst :: Subst -> Context -> Context
ctxSubst sigma =
    Ctx . fmap (typeSubst sigma) . unCtx

coproductCtx :: Context -> Context -> Maybe Context
coproductCtx ctx1 ctx2 = do
    -- let dom = domCtx ctx1 `S.intersection` domCtx ctx2
    common <- sequence $ M.intersectionWith coproductType (unCtx ctx1) (unCtx ctx2)
    (s1,s2) <- foldrM
        (\(s1,t1) (s2,t2) -> do
            s <- joinSubst s1 s2
            t <- joinSubst t1 t2
            return (s,t)
        )
        (idSubst S.empty, idSubst S.empty)
        common
    return $ Ctx $ M.unionWith
        (\k1 k2 -> typeSubst s1 k1)
        (unCtx ctx1) (unCtx ctx2)

data Kinding = Kinding {
    kCtx        :: Context,
    kClass      :: ProtoType
}

coproductKinding :: Kinding -> Kinding -> Maybe Kinding
coproductKinding k1 k2 = do
    (s1,s2) <- coproductType (kClass k1) (kClass k2)
    ctx <- coproductCtx (ctxSubst s1 $ kCtx k1) (ctxSubst s2 $ kCtx k2)
    return $ Kinding ctx (typeSubst s1 (kClass k1))

inferKind :: ProtoType -> Maybe Kinding
inferKind (Kind n) =
    -- 'case' is for strictness
    case n+1 of m -> return $ Kinding emptyCtx (Kind m)
inferKind (Var a) =
    return $ Kinding (Ctx $ M.singleton a (Kind 0)) (Kind 0)
inferKind (Pi a t1 t2) = do
    k1 <- inferKind t1
    k2 <- inferKind t2
    guard $ M.notMember a (unCtx (kCtx k1))
    k <- coproductKinding k1 k2
    return $ Kinding ((Ctx . M.delete a . unCtx) (kCtx k)) (kClass k)
inferKind (App t1 t2) = do
    k1 <- inferKind t1
    k2 <- inferKind t2
    k <- coproductKinding k1 k2
    return k


data Typing = Typing {
    tCtx        :: Context,
    tObject     :: ProtoType,
    tClass      :: ProtoType
}

-- typingType :: Int -> Typing
-- typingType n = Typing emptyCtx (Kind n) (Kind (n+1))
--
-- typingVar :: Symbol -> Typing -> Maybe Typing
-- typingVar a (Typing ctx t (Kind _)) = do
--     guard $ S.notMember a (domCtx ctx)
--     return $ Typing (Ctx $ M.insert a t (unCtx ctx)) (Var a) t
--
-- typingPi :: Symbol -> Typing -> Maybe Typing
-- typingPi a (Typing ctx t k) = do
--     undefined

newtype Global = Global { unGlobal :: Map Symbol Typing }

type Check = MaybeT (State Int)

liftMaybe :: Maybe a -> Check a
liftMaybe = MaybeT . return

fresh :: Check ProtoType
fresh = do
    n <- lift get
    lift $ modify (+ 1)
    return $ Var $ "t" ++ show n

skolem :: Typing -> Check Typing
skolem typing = do
    let dom = domCtx (tCtx typing)
    assoc <- mapM (\s -> do
                a <- fresh
                return (s,a)
                ) (S.toAscList dom)
    let sigma = Subst $ M.fromAscList assoc
    return $ Typing
        (Ctx $ fmap (typeSubst sigma) (unCtx $ tCtx typing))
        (typeSubst sigma (tObject typing))
        (typeSubst sigma (tClass typing))

inferType :: Global -> ProtoType -> Check Typing
inferType (Global glob) (Var a)
    | Just typ <- M.lookup a glob =
        skolem typ
    | otherwise = do
        Var alpha <- fresh
        return $ Typing
            (Ctx $ M.fromList [(alpha,Kind 0), (a,Var alpha)] )
            (Var a) (Var alpha)
inferType _ pi@(Pi _ _ _) = do
    k <- liftMaybe $ inferKind pi
    return $ Typing (kCtx k) pi (kClass k)
inferType glob (App t1 t2) = do
    typ1 <- inferType glob t1
    typ2 <- inferType glob t2
    k <- liftMaybe $ coproductKinding
        (Kinding (tCtx typ1) (tClass typ1))
        (Kinding (tCtx typ2) (tClass typ2))
    return $ Typing
        (kCtx k)
        (App (tObject typ1) (tObject typ2))
        (kClass k)
inferType (Global glob) (Lam a t) = do
    typ <- inferType (Global $ M.delete a glob) t
    case M.lookup a (unCtx $ tCtx typ) of
        Just alpha ->
            return $ Typing
                (Ctx $ M.delete a (unCtx $ tCtx typ))
                (Lam a (tObject typ))
                (Pi a alpha (tClass typ))
        Nothing -> do
            alpha <- fresh
            return $ Typing
                (tCtx typ)
                (Lam a (tObject typ))
                (Pi a alpha (tClass typ))

data Module = Module {
    modName     :: String,
    stmts       :: [(Symbol,Typing)]
}

checkModule :: Module -> Check Global
checkModule mod =
    foldlM checkStmt (Global M.empty) (stmts mod)
    where
        checkStmt :: Global -> (Symbol,Typing) -> Check Global
        checkStmt global (sym,typ) = do
            guard $ M.notMember sym (unGlobal global)
            kinding <- liftMaybe $ inferKind (tClass typ)
            typing <- inferType global (tObject typ)
            -- TODO check !!
            return $ Global $ M.insert sym typing (unGlobal global)
