module Infer where

import Syntax
import Type

import Control.Monad.Except
import Control.Monad.State

import Data.Foldable
import Data.List
import Data.Monoid

import qualified Data.Map as Map
import qualified Data.Set as Set

------------------------------------------------------------------------
newtype TypeEnv =
  TypeEnv (Map.Map Var Scheme)

------------------------------------------------------------------------
extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

------------------------------------------------------------------------
--- Inference Monad
------------------------------------------------------------------------
type Infer a = ExceptT TypeError (State Unique) a

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m =
  case evalState (runExceptT m) initUnique of
    Left err -> Left err
    Right res -> Right $ closeOver res

------------------------------------------------------------------------
--- Substitution
------------------------------------------------------------------------
type Subst = Map.Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

------------------------------------------------------------------------
--- Substitutable
------------------------------------------------------------------------
class Substitutable where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set Var

instance Substitutable Type where
  apply _ (TCon a) = TCon a
  apply s t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
  ftv TCon {} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where
      s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

------------------------------------------------------------------------
letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TVar $ TV (letters !! count s)

------------------------------------------------------------------------
--- Unification
------------------------------------------------------------------------
occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unify :: Type -> Type -> Infer Subst
unify (l `TArr` r) (l' `TArr` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b)
  | a == b = return nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: TVar -> Type -> Infer Subst
bind a t
  | t == TVar a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ Map.singleton a t

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env
