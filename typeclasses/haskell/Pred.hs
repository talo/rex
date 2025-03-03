-----------------------------------------------------------------------------
-- Pred:		Predicates
-- 
-- Part of `Typing Haskell in Haskell', version of November 23, 2000
-- Copyright (c) Mark P Jones and the Oregon Graduate Institute
-- of Science and Technology, 1999-2000
-- 
-- This program is distributed as Free Software under the terms
-- in the file "License" that is included in the distribution
-- of this software, copies of which may be obtained from:
--             http://www.cse.ogi.edu/~mpj/thih/
-- 
-----------------------------------------------------------------------------

module Pred where
import List(union,(\\))
import Monad(msum)
import Id
import Kind
import Type
import Subst
import Unify
import PPrint

data Qual t = [Pred] :=> t
              deriving Eq

instance PPrint t => PPrint (Qual t) where
  pprint (ps  :=> t) = (pprint ps <+> text ":=>") $$ nest 2 (parPprint t)

data Pred   = IsIn Id Type
              deriving Eq

instance PPrint Pred where
  pprint (IsIn i t) = text "isIn1" <+> text ("c" ++ i) <+> parPprint t

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t)      = tv ps `union` tv t

instance Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn i t)      = tv t

mguPred, matchPred :: Pred -> Pred -> Maybe Subst
mguPred             = lift mgu
matchPred           = lift match

lift m (IsIn i t) (IsIn i' t')
         | i == i'   = m t t'
         | otherwise = fail "classes differ"

type Class    = ([Id], [Inst])
type Inst     = Qual Pred

-----------------------------------------------------------------------------

data ClassEnv = ClassEnv { classes  :: Id -> Maybe Class,
                           defaults :: [Type] }

super     :: ClassEnv -> Id -> [Id]
super ce i = case classes ce i of Just (is, its) -> is

insts     :: ClassEnv -> Id -> [Inst]
insts ce i = case classes ce i of Just (is, its) -> its

defined :: Maybe a -> Bool
defined (Just x) = True
defined Nothing  = False

modify       :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce{classes = \j -> if i==j then Just c
                                           else classes ce j}

initialEnv :: ClassEnv
initialEnv  = ClassEnv { classes  = \i -> fail "class not defined",
                         defaults = [tInteger, tDouble] }

type EnvTransformer = ClassEnv -> Maybe ClassEnv

infixr 5 <:>
(<:>)       :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do ce' <- f ce
                  g ce'

addClass                              :: Id -> [Id] -> EnvTransformer
addClass i is ce
 | defined (classes ce i)              = fail "class already defined"
 | any (not . defined . classes ce) is = fail "superclass not defined"
 | otherwise                           = return (modify ce i (is, []))

addPreludeClasses :: EnvTransformer
addPreludeClasses  = addCoreClasses <:> addNumClasses

addCoreClasses ::   EnvTransformer
addCoreClasses  =   addClass "Eq" []
                <:> addClass "Ord" ["Eq"]
                <:> addClass "Show" []
                <:> addClass "Read" []
                <:> addClass "Bounded" []
                <:> addClass "Enum" []
                <:> addClass "Functor" []
                <:> addClass "Monad" []

addNumClasses  ::   EnvTransformer
addNumClasses   =   addClass "Num" ["Eq", "Show"]
                <:> addClass "Real" ["Num", "Ord"]
                <:> addClass "Fractional" ["Num"]
                <:> addClass "Integral" ["Real", "Enum"]
                <:> addClass "RealFrac" ["Real", "Fractional"]
                <:> addClass "Floating" ["Fractional"]
                <:> addClass "RealFloat" ["RealFrac", "Floating"]

addInst                        :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
 | not (defined (classes ce i)) = fail "no class for instance"
 | any (overlap p) qs           = fail "overlapping instance"
 | otherwise                    = return (modify ce i c)
   where its = insts ce i
         qs  = [ q | (_ :=> q) <- its ]
         c   = (super ce i, (ps:=>p) : its)

overlap       :: Pred -> Pred -> Bool
overlap p q    = defined (mguPred p q)

exampleInsts ::  EnvTransformer
exampleInsts =   addPreludeClasses
             <:> addInst [] (IsIn "Ord" tUnit)
             <:> addInst [] (IsIn "Ord" tChar)
             <:> addInst [] (IsIn "Ord" tInt)
             <:> addInst [IsIn "Ord" (TVar (Tyvar "a" Star)),
                          IsIn "Ord" (TVar (Tyvar "b" Star))]
                         (IsIn "Ord" (pair (TVar (Tyvar "a" Star))
                                           (TVar (Tyvar "b" Star))))

-----------------------------------------------------------------------------

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t)
 = p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

byInst                   :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t)    = msum [ tryInst it | it <- insts ce i ]
 where tryInst (ps :=> h) = do u <- matchPred h p
                               Just (map (apply u) ps)

entail        :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
                 case byInst ce p of
                   Nothing -> False
                   Just qs -> all (entail ce ps) qs

-----------------------------------------------------------------------------

inHnf       :: Pred -> Bool
inHnf (IsIn c t) = hnf t
 where hnf (TVar v)  = True
       hnf (TCon tc) = False
       hnf (TAp t _) = hnf t

toHnfs      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)

toHnf                 :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p | inHnf p   = return [p]
           | otherwise = case byInst ce p of
                           Nothing -> fail "context reduction"
                           Just ps -> toHnfs ce ps

simplify   :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
 where loop rs []                            = rs
       loop rs (p:ps) | entail ce (rs++ps) p = loop rs ps
                      | otherwise            = loop (p:rs) ps

reduce      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do qs <- toHnfs ce ps
                  return (simplify ce qs)

scEntail        :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)

-----------------------------------------------------------------------------
