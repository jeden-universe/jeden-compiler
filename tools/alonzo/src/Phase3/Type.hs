
module Phase3.Type (
    Type(..), TVar(..),
    typeVars,
    Subst(..),
    idSubst, compSubst, appSubst,
    morphism, coproduct
) where

import Phase3.AST       ( Type(..) )

import Data.Map as M (
    Map, (\\), empty, singleton, lookup, findWithDefault,
    insert, delete,
    union, intersectionWith, mergeWithKey,
    fromAscList
    )
import Data.Set as S (
    Set, singleton, union, toAscList
    )

import Prelude hiding ( lookup )


type TVar = String

--
{-| Produce the set of all type variables appearing in the given type.
-}
typeVars :: Type -> Set TVar
typeVars (TyVar a) = S.singleton a
typeVars (TyFun s t) = typeVars s `S.union` typeVars t

{-| The type of type variable substitutions.
-}
newtype Subst = Subst (Map TVar Type)

{-| Build the identity substitutions over the given set of variables.
-}
idSubst :: Set TVar -> Subst
idSubst = Subst . M.fromAscList . map (\v -> (v,TyVar v)) . S.toAscList

{-| Composition of substitutions.

Composition is performed in 'Category' order, i.e.,

> compSubst g f

produces a substition that applies first @f@, then @g@.
-}
compSubst :: Subst -> Subst -> Subst
compSubst g (Subst f) =
    Subst $ fmap (appSubst g) f

{-| Apply a substitution to a type, producing another type. This
effectively turns a substitution into a morphism of types.
-}
appSubst :: Subst -> Type -> Type
appSubst (Subst m) (TyVar a) =
    M.findWithDefault (TyVar a) a m
appSubst m (TyFun s t) =
    TyFun (appSubst m s) (appSubst m t)

{-| "More general than" binary relation between types. If the first type
is indeed more general than the other, this function returns a type
variable substitution map as a witness of the relation.

This relation defines a pre-ordering that may be denoted @<=@, since
more general types look smaller. However, it cannot serve as a definition of
a Haskell 'Ord' instance since that would require a total order. Two types may
be non-comparable, i.e., neither is more general than the other.
-}
morphism :: Type -> Type -> Maybe Subst
morphism (TyVar a) t = Just $ Subst $ M.singleton a t
morphism (TyFun s1 t1) (TyFun s2 t2) = do
    Subst s12 <- morphism s1 s2
    Subst t12 <- morphism t1 t2
    if and $ M.intersectionWith (==) s12 t12
    then Just $ Subst (s12 `M.union` t12)
    else Nothing
morphism _ _ = Nothing

{-| The coproduct of two types @a@ and @b@ is the smallest type @u@ such that

> a <= u && b <= u

The witness to the existence of this coproduct is a pair of substitutions
into @u@, one from @a@ and one from @b@.
-}
coproduct :: Type -> Type -> Maybe (Subst,Subst)
coproduct (TyVar a) t =
    Just (Subst $ M.singleton a t, idSubst $ typeVars t)
coproduct s (TyVar b) =
    Just (idSubst $ typeVars s, Subst $ M.singleton b s)
coproduct (TyFun s1 t1) (TyFun s2 t2) = do
    (s12,s21) <- coproduct s1 s2
    (t12,t21) <- coproduct t1 t2
    r1 <- unionWith s21 t21 s12 t12
    r2 <- unionWith s12 t12 s21 t21
    return (r1,r2)
    where
        unionWith :: Subst -> Subst -> Subst -> Subst -> Maybe Subst
        unionWith d c (Subst a) (Subst b) = fmap Subst $ sequence $
            M.mergeWithKey
                (\_ x y -> Nothing) -- TODO  to be continued...
                (fmap Just)
                (fmap Just)
                a b
