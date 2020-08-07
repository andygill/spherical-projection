{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Optimizer where

import Expr
import Types
--Return a Maybe (Expr a)
-- NEED THE EXTRA WRAPPER
opt :: Mu Expr -> Mu Expr
opt (Mu (ExpAdd (Mu (ExpScalar 0)) a)) = opt a
opt (Mu (ExpAdd a (Mu (ExpScalar 0)))) = opt a
--THERE IS NO DIFFERENCE IN THE NODES OF THE OPERATIONS GRAPH
{-}
opt (Mu (ExpAdd a (Mu (ExpMul b c))))   | a == b = opt $ Mu $ ExpMul b $ Mu $ ExpAdd c $ Mu $ ExpScalar 1
                                        | a == c = opt $ Mu $ ExpMul (Mu $ ExpAdd b $ Mu $ ExpScalar 1) c
                                        | otherwise = Mu $ ExpAdd (opt a) $ opt $ Mu (ExpMul b c)
opt (Mu (ExpAdd (Mu (ExpMul a b)) c))   | c == b = opt $ Mu $ ExpMul a $ Mu $ ExpAdd b $ Mu $ ExpScalar 1
                                        | c == a = opt $ Mu $ ExpMul (Mu $ ExpAdd a $ Mu $ ExpScalar 1) b
                                        | otherwise = Mu $ ExpAdd (opt $ Mu $ ExpMul a b) $ opt c
                                        -}
-- Decreases NODES OF THE OPERATIONS GRAPH by 1
opt (Mu (ExpAdd (Mu (ExpMul a b)) (Mu (ExpMul c d))))   | a == c = opt $ Mu $ ExpMul a $ (Mu $ ExpAdd b d)
                                                        | a == d = opt $ Mu $ ExpMul a $ (Mu $ ExpAdd b c)
                                                        | b == c = opt $ Mu $ ExpMul b $ (Mu $ ExpAdd a d)
                                                        | b == d = opt $ Mu $ ExpMul b $ (Mu $ ExpAdd a c)
                                                        | otherwise = Mu $ ExpAdd (opt $ Mu $ ExpMul a b) (opt $ Mu $ ExpMul c d)

opt (Mu (ExpSub (Mu (ExpScalar 0)) a)) = opt $ Mu $ ExpMul (Mu $ ExpScalar (-1.0)) a
opt (Mu (ExpSub a (Mu (ExpScalar 0)))) = opt a
opt (Mu (ExpSub a b))   | a == b = Mu $ ExpScalar 0
                        | otherwise = Mu $ ExpSub (opt a) $ opt b

opt (Mu (ExpSin (Mu (ExpScalar  0)))) = Mu $ ExpScalar 0
opt (Mu (ExpSin (Mu (ExpAsin    a)))) = opt a

opt (Mu (ExpAsin(Mu (ExpSin     a)))) = opt a
opt (Mu (ExpAsin(Mu (ExpScalar  1)))) = Mu $ ExpScalar $ num_pi / 2
opt (Mu (ExpAsin(Mu (ExpScalar  (-1))))) = Mu $ ExpScalar $ (-num_pi) / 2
opt (Mu (ExpAsin(Mu (ExpScalar  0)))) = Mu $ ExpScalar 0


opt (Mu (ExpCos (Mu (ExpScalar  0)))) = Mu $ ExpScalar 1.0
opt (Mu (ExpCos (Mu (ExpAcos    a)))) = opt a

opt (Mu (ExpAcos(Mu (ExpCos     a)))) = opt a
opt (Mu (ExpAcos(Mu (ExpScalar  1)))) = Mu $ ExpScalar 0
opt (Mu (ExpAcos(Mu (ExpScalar  (-1))))) = Mu $ ExpScalar num_pi
opt (Mu (ExpAcos(Mu (ExpScalar  0)))) = Mu $ ExpScalar $ num_pi / 2

opt (Mu (ExpMul (Mu (ExpScalar 0)) _)) = Mu $ ExpScalar 0
opt (Mu (ExpMul _ (Mu (ExpScalar 0)))) = Mu $ ExpScalar 0
opt (Mu (ExpMul (Mu (ExpScalar 1)) a)) = opt a
opt (Mu (ExpMul a (Mu (ExpScalar 1)))) = opt a
--opt (Mu (ExpMul a b))   | a == b = opt $ Mu $ ExpPower a 2
--                        | otherwise = Mu $ ExpMul (opt a) (opt b)
opt (Mu (ExpMul a (Mu (ExpPower b x)))) | a == b = opt $ Mu $ ExpPower b $ x + 1
                                        | otherwise = Mu $ ExpMul (opt a) $ opt (Mu (ExpPower b x))
opt (Mu (ExpMul (Mu (ExpPower a x)) b)) | a == b = opt $ Mu $ ExpPower a $ x + 1
                                        | otherwise = Mu $ ExpMul (opt $ Mu $ ExpPower a x) $ opt b

-- Will not work if something "above" in the tree is 0 || Fails to error
opt (Mu (ExpDiv (Mu (ExpScalar 0)) (Mu (ExpScalar 0)))) = error "Zero over Zero"
opt (Mu (ExpDiv (Mu (ExpScalar 0)) _)) = Mu $ ExpScalar 0
opt (Mu (ExpDiv a b))   | a == b = Mu $ ExpScalar 1.0
                        | otherwise = Mu $ ExpDiv (opt a) $ opt b
opt (Mu (ExpDiv (Mu (ExpMul a b)) c))   | a == c = opt b
                                        | b == c = opt a
                                        | otherwise = Mu $ ExpDiv (opt $ Mu $ ExpMul a b) $ opt c
opt (Mu (ExpDiv a (Mu (ExpMul b c))))   | b == a = opt c
                                        | c == a = opt b
                                        | otherwise = Mu $ ExpDiv (opt a) $ opt $ Mu $ ExpMul b c
opt (Mu (ExpDiv (Mu (ExpPower a x)) b)) | a == b = opt $ Mu $ ExpPower a $ x - 1
                                        | otherwise = Mu $ ExpDiv (opt $ Mu $ ExpPower a x) $ opt b
opt (Mu (ExpDiv _ (Mu (ExpScalar 0)))) = error "Divide by Zero"

opt (Mu (ExpSqrt (Mu (ExpPower a x))))  | even x = opt $ Mu $ ExpPower a $ div x 2
                                        | otherwise = error "don't know what to do about non-integer exponents"

opt (Mu (ExpPower a 0)) = Mu $ ExpScalar 1.0
opt (Mu (ExpPower a 1)) = opt a
opt (Mu (ExpPower (Mu (ExpSqrt a)) x))  | even x = opt $ Mu $ ExpPower a $ div x 2
                                        | otherwise = Mu $ ExpPower (Mu $ ExpSqrt $ opt a) x

opt (Mu a) = Mu $ fmap opt a

-- may need a contant number of runs for fix
fix :: (Eq a) => (a -> a) -> a -> a
fix f a = if a == b then a else fix f b
    where
        b = f a
