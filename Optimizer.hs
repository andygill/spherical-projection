{-# LANGUAGE FlexibleInstances #-}

module Optimizer where

import Data.List as L
import Data.Tuple as T
import Data.Maybe

import Expr
import Types

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

-------------------------------------------------------------------------------------------

findAndUpdate :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
findAndUpdate s u [] = []
findAndUpdate s u ((a,b):xs)    | a == s = (a,u):xs
                                | otherwise = (a,b):(findAndUpdate s u xs)

findNode :: [(V, Expr V)] -> V -> Expr V
findNode vs v = fromMaybe (error "Node not found") $ lookup v vs

notExpId:: Expr V -> Bool
notExpId (ExpId _)= False
notExpId _        = True

notExpVar :: Expr V -> Bool
notExpVar (ExpVar _ )   = False
notExpVar _             = True

unMu x = case x of (Mu a) -> a

findV :: [(V, Expr V)] -> [Expr V] -> [(V, Expr V)]
findV vs list = rev_find swap_vs (1 + length vs) $ list
        where
            swap_vs = map T.swap vs

rev_find :: [(Expr V, V)] -> Int -> [Expr V] -> [(V, Expr V)]
rev_find _ _ [] = []
rev_find vs vNew (e:es) = case lookup e vs of
    Nothing -> (V vNew, e) : (rev_find vs (vNew + 1) es)
    Just v -> (v,e) : (rev_find vs vNew es)

muHeight :: Mu (Expr) -> Int
muHeight (Mu a) = if 0 == length as then 1 else 1 + (maximum $ map muHeight as)
    where
        as = foldr (:) [] a

muChild :: Mu (Expr) -> Mu (Expr) -> Bool
muChild p c = foldr ((||) . (==c)) False $ foldr (:) [] $ unMu p

checkNode :: V -> V -> [(V, Expr V)] -> [(V, Expr V)]
checkNode v_0 v1 xs = map (\(m,expr) -> (m, fmap (\n -> if n == v_0 then v1 else n) expr)) xs

replaceInstances :: [(V, Expr V)] -> [(V, Expr V)] -> [(V, Expr V)]
replaceInstances [] xs = xs
replaceInstances ((v_0, ExpId v1):ys) xs = replaceInstances ys $ checkNode v_0 v1 xs
replaceInstances (y:ys) xs = replaceInstances ys xs

removePointerNodes :: [(V, Expr V)] -> [(V, Expr V)]
removePointerNodes ns = filter (notExpId . snd) $ replaceInstances ns ns

removeParameterNodes :: [V] -> [(V, Expr V)] -> [(V, Expr V)]
removeParameterNodes [] ns      = ns
removeParameterNodes ((V x):ps) ns  = removeParameterNodes ps $ L.delete ((V x), ExpVar x) ns

-- Clean up leaves and inner workings
cleanExpr :: ExprFunction -> ExprFunction
cleanExpr (ExprFunction as vs r) = ExprFunction as' vs' (V r')
    where
        snd_vs = map snd vs
        r'  = foldr (\(V x)-> min x) (length vs) (as ++ (map fst vs))
        isUsed x = foldr (||) False $ map (foldr ((||) . (==x)) False) snd_vs
        as' = filter isUsed as
        vs' = filter (\(v,_) -> isUsed v || (V r') == v) vs

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
                _ ->do
                    let l = foldr (:) [] v
                    let new_vs = vs ++ (map (\(V x) -> (V x, ExpVar x)) as)
                    let args = map (Mu . fmap (\(V x) -> Mu $ ExpVar x) . findNode new_vs) l
                    let m = if length l == 1
                        then
                            Mu $ fmap (\_ -> args !! 0) v
                        else
                            Mu $ fmap (\x-> case L.elemIndex x l of Just i -> args !! i) v
                    let eval_m = case evalMu m of
                                    (Double d) -> Just [d]
                                    (Tuple ds) -> sequence $ map g ds
                                                where
                                                    g (Double a) = Just a
                                                    g _ = Nothing
                                    _          -> Nothing
                    case eval_m of
                        Just d -> if length d == 1
                            then
                                muConversion (V $ n-1) $ ExprFunction as (findAndUpdate (V n) (ExpScalar $ d !! 0) vs) r
                            else do
                                let l = findV new_vs $ map ExpScalar d
                                let d' = filter (\((V x),_) -> x > length new_vs) l
                                muConversion (V $ n-1) $ ExprFunction as (findAndUpdate (V n) (ExpTuple (map fst d')) l) r
                        Nothing -> do
                            let o = opt m
                            if m == o then
                                muConversion (V $ n-1) $ ExprFunction as vs r
                            else do
                                if (1 == (muHeight m) - (muHeight o)) && muChild m o
                                    then do
                                        let e = fmap (\(Mu (ExpVar x))-> V x) $ unMu o
                                        let dNode = case lookup e (map T.swap new_vs) of Just x -> x
                                        muConversion (V $ n-1) $ ExprFunction as (removePointerNodes $ findAndUpdate (V n) (ExpId dNode) vs) r
                                    else do
                                        let os = foldr ((:) . unMu) [] $ unMu o
                                        let o' = if length os == 0
                                            then
                                                 (unMu o):[]
                                            else
                                                os
                                        {-let o'' = if length os == 0
                                            then
                                                unMu o
                                            else
                                                foldr ((:) . unMu) [] $ unMu o
                                        let swap_vs = map T.swap new_vs
                                        case lookup o'' swap_vs of
                                            Just x ->  muConversion (V $ n-1) $ ExprFunction as (removePointerNodes $ findAndUpdate (V n) (ExpId x) vs) r
                                            _ ->-}
                                        case o' of
                                            ((ExpVar x):[]) -> muConversion (V $ n-1) $ ExprFunction as (removePointerNodes $ findAndUpdate (V n) (ExpId (V x)) vs) r
                                            _ -> do
                                                let l' = map (fmap (\(Mu (ExpVar x))-> V x)) o'
                                                let l'' = findV new_vs l'
                                                let l''' = map fst l''
                                                let vs' = foldr (:) vs $ filter (\((V x),e) -> x > length new_vs) l''
                                                let v' = if (muHeight m) /= (muHeight o) then --exprIs o == exprIs v  then
                                                    --used for single argument reduction
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
                                                --let vs'' = removeParameterNodes as $ removePointerNodes $ findAndUpdate (V n) v' vs'
                                                let vs'' = removePointerNodes $ findAndUpdate (V n) v' vs'        
                                                muConversion (V $ n-1) $ ExprFunction as vs'' r

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
