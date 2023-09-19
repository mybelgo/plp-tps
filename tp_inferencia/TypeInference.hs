module TypeInference (TypingJudgment, Result(..), inferType, inferNormal, normalize)

where

import Data.List(intersect, union, nub, sort)
import Exp
import Type
import Unification

------------
-- Errors --
------------
data Result a = OK a | Error String


--------------------
-- Type Inference --
--------------------
type TypingJudgment = (Context, AnnotExp, Type)

typeVarsT :: Type -> [Int]
typeVarsT = foldType (:[]) [] [] union

typeVarsE :: Exp Type -> [Int]
typeVarsE = foldExp (const []) [] id id id [] [] (\r1 r2 r3 ->nub (r1++r2++r3)) (const setAdd) union
  where setAdd t r = union (typeVarsT t) r

typeVarsC :: Context -> [Int]
typeVarsC c = nub (concatMap (typeVarsT . evalC c) (domainC c))

typeVars :: TypingJudgment -> [Int]
typeVars (c, e, t) = sort $ union (typeVarsC c) (union (typeVarsE e) (typeVarsT t))

normalization :: [Int] -> [Subst]
normalization ns = foldr (\n rec (y:ys) -> extendS n (TVar  y) emptySubst : (rec ys)) (const []) ns [0..]

normalize :: TypingJudgment -> TypingJudgment
normalize j@(c, e, t) = let ss = normalization $ typeVars j in foldl (\(rc, re, rt) s ->(s <.> rc, s <.> re, s <.> rt)) j ss

inferType :: PlainExp -> Result TypingJudgment
inferType e = case infer' e 0 of
    OK (_, tj) -> OK tj
    Error s -> Error s

inferNormal :: PlainExp -> Result TypingJudgment
inferNormal e = case infer' e 0 of
    OK (_, tj) -> OK $ normalize tj
    Error s -> Error s

conPairGoals :: Context -> Context -> [UnifGoal]
conPairGoals c_1 c_2 = [(t_1, t_2) | s <- domainC c_1 `intersect` domainC c_2, let t_1 = evalC c_1 s, let t_2 = evalC c_2 s]

infer' :: PlainExp -> Int -> Result (Int, TypingJudgment)

infer' (SuccExp e)    n =
  case infer' e n of
    err@(Error _) -> err
    OK (n', (c', e', t')) ->
      case mgu [(t', TNat)] of
        UError u1 u2 -> uError u1 u2
        UOK subst -> OK (n', (subst <.> c',
                              subst <.> SuccExp e',
                              TNat))


-- COMPLETAR DESDE AQUI

infer' ZeroExp                n = OK (n, (emptyContext, ZeroExp, TNat))
infer' (VarExp x)             n = OK (n+1, (extendC emptyContext x (TVar n), VarExp x, TVar n))
infer' (AppExp u v)           n =
  case infer' u n of
    err@(Error _)             -> err
    OK (n_u, (c_u, e_u, t_u)) ->
      case infer' v n_u of
        err@(Error _)             -> err
        OK (n_v, (c_v, e_v, t_v)) ->
          case mgu ((t_u, TFun t_v (TVar n_v)) : conPairGoals c_u c_v) of
            UError u1 u2 -> uError u1 u2
            UOK subst    -> OK (n_v+1,
                                (joinC [subst <.> c_u, subst <.> c_v],
                                 subst <.> AppExp e_u e_v,
                                 subst <.> TVar n_v
                                )
                               )
infer' (LamExp x _ e)         n = 
  case infer' e n of
    err@(Error _) -> err
    OK (n', (c', e', t')) ->
      if elem x (domainC c') then OK (n', (removeC c' x, LamExp x t'' e', TFun t'' t'))
      else OK (n'+1, (extendC (removeC c' x) x (TVar n'), LamExp x (TVar n') e', TFun (TVar n') t'))
      where t'' = evalC c' x

-- OPCIONALES

infer' (PredExp e)            n =
  case infer' e n of
    err@(Error _)         -> err
    OK (n', (c', e', t')) ->
      case mgu [(t', TNat)] of
        UError u1 u2 -> uError u1 u2
        UOK subst    -> OK (n',
                            (subst <.> c',
                             subst <.> PredExp e',
                             TNat)
                           )
infer' (IsZeroExp e)          n =
  case infer' e n of
    err@(Error _)         -> err
    OK (n', (c', e', t')) ->
      case mgu [(t', TNat)] of
        UError u1 u2 -> uError u1 u2
        UOK subst    -> OK(n',
                           (subst <.> c',
                            subst <.> IsZeroExp e',
                            TBool))
infer' TrueExp                n = OK(n,
                                     (emptyContext,
                                      TrueExp,
                                      TBool))
infer' FalseExp                n = OK(n,
                                     (emptyContext,
                                      FalseExp,
                                      TBool))
infer' (IfExp u v w)          n =
  case infer' u n of
    err@(Error _)             -> err
    OK (n_u, (c_u, e_u, t_u)) ->
      case infer' v n_u of
        err@(Error _) -> err
        OK (n_v, (c_v, e_v, t_v)) ->
          case infer' w n_v of
            err@(Error _) -> err
            OK (n_w, (c_w, e_w, t_w)) ->
              case mgu ([(t_v, t_w), (t_u, TBool)] ++ conPairGoals c_u c_v) of
                UError u1 u2 -> uError u1 u2
                UOK subst    -> OK (n_w,
                                    (joinC (map (subst <.>) [c_u, c_v, c_w]),
                                    subst <.> IfExp e_u e_v e_w,
                                    subst <.> t_v
                                    )
                                   )

--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
