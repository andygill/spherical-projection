{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Optimizer where

import Data.List as L
import Data.Tuple as T
import Data.Maybe
import Control.Applicative

import Expr
import Types
--Return a Maybe (Expr a)
-- NEED THE EXTRA WRAPPER
opt :: Mu Expr -> Mu Expr
opt (Mu (ExpAdd (Mu (ExpScalar 0)) a)) = a
opt (Mu (ExpAdd a (Mu (ExpScalar 0)))) = a
--THERE IS NO DIFFERENCE IN THE NODES OF THE OPERATIONS GRAPH
{-}
opt (Mu (ExpAdd a (Mu (ExpMul b c))))   | a == b = Mu $ ExpMul b $ Mu $ ExpAdd c $ Mu $ ExpScalar 1
                                        | a == c = Mu $ ExpMul (Mu $ ExpAdd b $ Mu $ ExpScalar 1) c
                                        | otherwise = Mu $ ExpAdd a $ Mu (ExpMul b c)
opt (Mu (ExpAdd (Mu (ExpMul a b)) c))   | c == b = Mu $ ExpMul a $ Mu $ ExpAdd b $ Mu $ ExpScalar 1
                                        | c == a = Mu $ ExpMul (Mu $ ExpAdd a $ Mu $ ExpScalar 1) b
                                        | otherwise = Mu $ ExpAdd (Mu $ ExpMul a b) c
                                        -}
-- Decreases NODES OF THE OPERATIONS GRAPH by 1
opt (Mu (ExpAdd (Mu (ExpMul a b)) (Mu (ExpMul c d))))   | a == c = Mu $ ExpMul a $ (Mu $ ExpAdd b d)
                                                        | a == d = Mu $ ExpMul a $ (Mu $ ExpAdd b c)
                                                        | b == c = Mu $ ExpMul b $ (Mu $ ExpAdd a d)
                                                        | b == d = Mu $ ExpMul b $ (Mu $ ExpAdd a c)
                                                        | otherwise = Mu $ ExpAdd (Mu $ ExpMul a b) (Mu $ ExpMul c d)

opt (Mu (ExpSub (Mu (ExpScalar 0)) a)) = Mu $ ExpMul (Mu $ ExpScalar (-1.0)) a
opt (Mu (ExpSub a (Mu (ExpScalar 0)))) =  a
opt (Mu (ExpSub a b))   | a == b = Mu $ ExpScalar 0
                        | otherwise = Mu $ ExpSub a b

opt (Mu (ExpSin (Mu (ExpScalar  0)))) = Mu $ ExpScalar 0
opt (Mu (ExpSin (Mu (ExpAsin    a)))) = a

opt (Mu (ExpAsin(Mu (ExpSin     a)))) = a
opt (Mu (ExpAsin(Mu (ExpScalar  1)))) = Mu $ ExpScalar $ num_pi / 2
opt (Mu (ExpAsin(Mu (ExpScalar  (-1))))) = Mu $ ExpScalar $ (-num_pi) / 2
opt (Mu (ExpAsin(Mu (ExpScalar  0)))) = Mu $ ExpScalar 0


opt (Mu (ExpCos (Mu (ExpScalar  0)))) = Mu $ ExpScalar 1.0
opt (Mu (ExpCos (Mu (ExpAcos    a)))) = a

opt (Mu (ExpAcos(Mu (ExpCos     a)))) = a
opt (Mu (ExpAcos(Mu (ExpScalar  1)))) = Mu $ ExpScalar 0
opt (Mu (ExpAcos(Mu (ExpScalar  (-1))))) = Mu $ ExpScalar num_pi
opt (Mu (ExpAcos(Mu (ExpScalar  0)))) = Mu $ ExpScalar $ num_pi / 2

opt (Mu (ExpMul (Mu (ExpScalar 0)) _)) = Mu $ ExpScalar 0
opt (Mu (ExpMul _ (Mu (ExpScalar 0)))) = Mu $ ExpScalar 0
opt (Mu (ExpMul (Mu (ExpScalar 1)) a)) = a
opt (Mu (ExpMul a (Mu (ExpScalar 1)))) = a
--opt (Mu (ExpMul a b))   | a == b = Mu $ ExpPower a 2
--                        | otherwise = Mu $ ExpMul a (b)
opt (Mu (ExpMul a (Mu (ExpPower b x)))) | a == b = Mu $ ExpPower b $ x + 1
                                        | otherwise = Mu $ ExpMul a (Mu (ExpPower b x))
opt (Mu (ExpMul (Mu (ExpPower a x)) b)) | a == b = Mu $ ExpPower a $ x + 1
                                        | otherwise = Mu $ ExpMul (Mu $ ExpPower a x) b

-- Will not work if something "above" in the tree is 0 || Fails to error
opt (Mu (ExpDiv (Mu (ExpScalar 0)) (Mu (ExpScalar 0)))) = error "Zero over Zero"
opt (Mu (ExpDiv (Mu (ExpScalar 0)) _)) = Mu $ ExpScalar 0
opt (Mu (ExpDiv a b))   | a == b = Mu $ ExpScalar 1.0
                        | otherwise = Mu $ ExpDiv a b
opt (Mu (ExpDiv (Mu (ExpMul a b)) c))   | a == c = b
                                        | b == c = a
                                        | otherwise = Mu $ ExpDiv (Mu $ ExpMul a b) c
opt (Mu (ExpDiv a (Mu (ExpMul b c))))   | b == a = c
                                        | c == a = b
                                        | otherwise = Mu $ ExpDiv a $ Mu $ ExpMul b c
opt (Mu (ExpDiv (Mu (ExpPower a x)) b)) | a == b = Mu $ ExpPower a $ x - 1
                                        | otherwise = Mu $ ExpDiv (Mu $ ExpPower a x) b
opt (Mu (ExpDiv _ (Mu (ExpScalar 0)))) = error "Divide by Zero"

opt (Mu (ExpSqrt (Mu (ExpPower a x))))  | even x = Mu $ ExpPower a $ div x 2
                                        | otherwise = error "don't know what to do about non-integer exponents"

opt (Mu (ExpPower a 0)) = Mu $ ExpScalar 1.0
opt (Mu (ExpPower a 1)) = a
opt (Mu (ExpPower (Mu (ExpSqrt a)) x))  | even x = Mu $ ExpPower a $ div x 2
                                        | otherwise = Mu $ ExpPower (Mu $ ExpSqrt $ a) x

opt (Mu a) = Mu $ fmap opt a

findAndUpdate :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
findAndUpdate s u [] = []
findAndUpdate s u ((a,b):xs)    | a == s = (a,u):xs
                                | otherwise = (a,b):(findAndUpdate s u xs)

findNode :: [(V, Expr V)] -> V -> Expr V
findNode vs v = fromMaybe (error (show v ++ " not found")) $ lookup v vs
{-}
toMu :: [(V, Expr V)] -> Int -> Expr V -> Mu Expr
toMu _  0 e = Mu e
toMu vs n e = Mu $ fmap (toMu vs (n-1) . findNode vs) e

-- MAKE OPTIMIZER THAT DOESNT USE Mu

fromMu :: Mu Expr -> [(Expr V, V)] -> (V, Expr V)
fromMu (Mu e) vs' = do
    let e' = fmap (\x -> fst $ fromMu x vs') e
    case lookup e vs' of
        Nothing -> error "oof"--(fmap fromMu a, V l)
        Just v -> (v, e)
        --where
            --l = length vs'-}

argList :: Expr V -> [V]
argList = foldr (:) []

notExpId:: Expr V -> Bool
notExpId (ExpId _)= False
notExpId _        = True

checkNode :: V -> V -> [(V, Expr V)] -> [(V, Expr V)]
checkNode v_0 v1 xs = map (\(m,expr) -> (m, fmap (\n -> if n == v_0 then v1 else n) expr)) xs

replaceInstances :: [(V, Expr V)] -> [(V, Expr V)] -> [(V, Expr V)]
replaceInstances [] xs = xs
replaceInstances ((v_0, ExpId v1):ys) xs = replaceInstances ys $ checkNode v_0 v1 xs
replaceInstances (y:ys) xs = replaceInstances ys xs

removePointerNodes :: [(V, Expr V)] -> [(V, Expr V)]
removePointerNodes ns = filter (notExpId . snd) $ replaceInstances ns ns

-- Clean up leaves and inner workings
cleanExpr :: ExprFunction -> ExprFunction
cleanExpr (ExprFunction as vs r) = (ExprFunction as vs r)

muConversion :: V -> ExprFunction -> ExprFunction
muConversion (V n) (ExprFunction as vs r) = if n <= 0
    then
        -- run ExpId filter
        cleanExpr $ ExprFunction as vs r
    else do
        case lookup (V n) vs of
            Nothing ->  muConversion (V $ n-1) $ ExprFunction as vs r
            Just v -> case v of
                ExpScalar _ -> muConversion (V $ n-1) $ ExprFunction as vs r
                ExpVar _ -> muConversion (V $ n-1) $ ExprFunction as vs r
                _           -> do
                    let l = argList v
                    --let args = map (Mu . fmap (Mu . ExpVar) . findNode vs) l
                    let args = map (Mu . fmap (\(V x) -> Mu $ ExpVar x) . findNode vs) l
                    let m = if length l == 1
                        then
                            Mu $ fmap (\_ -> args !! 0) v
                        else
                            Mu $ fmap (\x-> case L.elemIndex x l of Just i -> args !! i) v
                    let o = opt m
                    if m == o then
                        muConversion (V $ n-1) $ ExprFunction as vs r
                    else do
                        let unMu x = case x of (Mu a) -> a
                        let o' = if os == 0
                            then
                                 (unMu o):[]
                            else
                                foldr ((:) . unMu) [] $ unMu o
                                where
                                    os = length $ foldr (:) [] $ unMu o
                        case o' of
                            ((ExpVar x):[]) -> ExprFunction as (removePointerNodes $ findAndUpdate (V n) (ExpId (V x)) vs) r
                            ((ExpScalar c):[]) -> muConversion (V $ n-1) $ ExprFunction as (removePointerNodes ns) r
                                            where
                                                ns = findAndUpdate (V n) (ExpId x) vs
                                                x = case lookup (ExpScalar c) $ map T.swap vs of Just v -> v
                            _ -> do
                                let l' = map (fmap (\(Mu (ExpVar x))-> V x)) o'
                                let swap_vs = map T.swap vs
                                let l'' = rev_find (1 + length vs) l'
                                        where
                                            rev_find :: Int -> [Expr V] -> [(V, Expr V)]
                                            rev_find _ [] = []
                                            rev_find vNew (e:es) = case lookup e swap_vs of
                                                Nothing -> (V vNew, e) : (rev_find (vNew + 1) es)
                                                Just v -> (v,e) : (rev_find vNew es)
                                let l''' = map fst l''
                                let vs' = foldr (:) vs $ filter (\((V x),e) -> x >= length vs) l''
                                let v' = if length l /= length l' then
                                            if length l''' == 1 then
                                                ExpId $ l''' !! 0
                                            else
                                                -- addition and multiplication changes
                                                error "idk"--fmap (\x-> case L.elemIndex x l of Just i -> l''' !! i) v
                                        else
                                            if length l''' == 1 then
                                                fmap (\_ -> l''' !! 0) v
                                            else
                                                fmap (\x-> case L.elemIndex x l of Just i -> l''' !! i) v
                                let vs'' = removePointerNodes $ findAndUpdate (V n) v' vs'
                                muConversion (V $ n-1) $ ExprFunction as vs'' r




--must optimize top down, then down up
{-partialEval :: ExprFunction -> ExprFunction
partialEval (ExprFunction as (v:vs) r) ys = do

    case L.find (==v) as of
        Just _ -> optimize (ExprFunction as vs r) ys
        Nothing -> do
            let find x = fromMaybe (error (show v ++ " not found")) $
                         lookup x ys
            case eval $ snd $ find v of
                Double a    -> optimize (ExprFunction as vs r) $ findAndUpdate v a ys
                Tuple as    -> optimize (ExprFunction as vs r) $ findAndUpdate v as ys
                Fail _      ->-}

findScalars :: [(V,Expr V)] -> [(V,Expr V)]
findScalars = filter (isScalar . snd)
    where
        isScalar (ExpScalar _)  = True
        isScalar _              = False

-- may need a contant number of runs for fix
fix :: (Eq a) => (a -> a) -> a -> a
fix f a = if a == b then a else fix f b
    where
        b = f a
