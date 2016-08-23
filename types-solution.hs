module Unifier where
import qualified Data.HashMap.Strict as H
import Lib

{---------------------------------------------
 - Given Demo Environments
 -   You might not want to modify this. ;)
 ---------------------------------------------}
phi :: SubstEnv
phi = (H.insert 5 (FnTy BoolTy (TyVar 2)) H.empty)

rho :: EqnSet
--rho = [((TyVar 1), (FnTy BoolTy (TyVar 7))), ((TyVar 5), (FnTy BoolTy (TyVar 2))), (ListTy BoolTy, (TyVar 7)), ((TyVar 9), (TyVar 1))]
--rho = [(TyVar 100, TyVar 200), (TyVar 200, TyVar 100), ((TyVar 1), (FnTy BoolTy (TyVar 7))), ((TyVar 5), (FnTy BoolTy (TyVar 2))), (ListTy BoolTy, (TyVar 7)), ((TyVar 9), (TyVar 1))]
--rho  = [((TyVar 1), (TyVar 1)), ((TyVar 2), PairTy (TyVar 4) (TyVar 6)), ((TyVar 4), (TyVar 1)), ((TyVar 11, TyVar 1))]
--rho  = [((TyVar 1), (TyVar 3)), ((TyVar 3), (TyVar 5)), ((TyVar 4), (TyVar 3)), ((TyVar 9, FnTy (TyVar 1) (FnTy (TyVar 2) (TyVar 2)))), ((TyVar 11), (TyVar 1)), ((TyVar 5), (TyVar 1)), ((TyVar 6), (TyVar 4))]
--rho = [((TyVar 1), (TyVar 2)), ((TyVar 2), (TyVar 3)), ((TyVar 3), (TyVar 1))]
rho = [((TyVar 1), (TyVar 2)), ((TyVar 2), (TyVar 3)), ((TyVar 3), (TyVar 1)), ((TyVar 7), (FnTy (FnTy (TyVar 3) (TyVar 2)) (TyVar 3))), ((TyVar 8), (TyVar 1)), ((TyVar 9), (TyVar 1))]

{-
rho  = [((TyVar 1), ListTy (TyVar 8)),
        ((TyVar 2), (TyVar 1)),
        ((TyVar 8), (PairTy StringTy (TyVar 2)))]
-}

{---------------------------------------------
 - Problem 1: substFun
 ---------------------------------------------}
substFun :: SubstEnv -> TyCon -> TyCon
substFun tyenv t@(TyVar v) = case (H.lookup v tyenv) of
                               Just r -> r
                               Nothing -> t
substFun tyenv _ = error "Should not happen."

{---------------------------------------------
 - Problem 2: monoTyLiftSubst
 ---------------------------------------------}
monoTyLiftSubst :: SubstEnv -> TyCon -> TyCon
monoTyLiftSubst tyenv t@(TyVar s) = substFun tyenv t
monoTyLiftSubst tyenv (PairTy a b) = PairTy t1 t2
  where
    t1 = monoTyLiftSubst tyenv a
    t2 = monoTyLiftSubst tyenv b
monoTyLiftSubst tyenv (FnTy a b) = FnTy t1 t2
  where
    t1 = monoTyLiftSubst tyenv a
    t2 = monoTyLiftSubst tyenv b
monoTyLiftSubst tyenv (ListTy a) = ListTy t1
  where
    t1 = monoTyLiftSubst tyenv a
monoTyLiftSubst _ ty = ty

{---------------------------------------------
 - Problem 3: occurs
 ---------------------------------------------}
occurs :: TyCon -> TyCon -> Bool
occurs (TyVar p) (TyVar q) = p == q
occurs v@(TyVar _) (PairTy p q) = occurs v p || occurs v q
occurs v@(TyVar _) (FnTy p q) = occurs v p || occurs v q
occurs v@(TyVar _) (ListTy p) = occurs v p
occurs _ _ = False

{---------------------------------------------
 - Problem 4: unify
 ---------------------------------------------}
unify :: EqnSet -> Maybe SubstEnv
unify [] = Just H.empty
unify ((v@(TyVar n), t):fs) -- Eliminate
  | v == t = unify fs
  | occurs v t = Nothing
  | otherwise = case unify fs' of Nothing -> Nothing
                                  (Just phi) -> (Just (H.insert n (monoTyLiftSubst phi t) phi))
                where
                  fs' = map (\(t1, t2) -> (monoTyLiftSubst (H.insert n t H.empty) t1, monoTyLiftSubst (H.insert n t H.empty) t2)) fs
unify ((ty, TyVar s):fs) = unify ((TyVar s, ty):fs) -- Orient
unify ((lhs, rhs):fs)
  | lhs == rhs = unify fs -- Delete
  | otherwise = unifyDecompose lhs rhs fs -- Decomposition

unifyDecompose :: TyCon -> TyCon -> EqnSet -> Maybe SubstEnv
unifyDecompose (PairTy a b) (PairTy c d) eqns = unify ((a, c) : (b, d) : eqns)
unifyDecompose (FnTy a b) (FnTy c d) eqns = unify ((a, c) : (b, d) : eqns)
unifyDecompose (ListTy a) (ListTy b) eqns = unify ((a, b) : eqns)
unifyDecompose _ _ _ = Nothing
