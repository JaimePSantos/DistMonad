{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Vector where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

data Vec x a = Vec{unVec::[(a,x)]} deriving Show

vecZero :: Vec x a
vecZero = Vec []

vecAdd :: (Eq a, Num x) => (a,x) -> Vec x a ->  Vec x a

--Tratar do caso de x+y = 0 para retirar la de dentro
vecAdd(a,x) (Vec xs) = Vec(add' (a,x) xs) where
    add'(a,x) [] = [(a,x)]
    add'(a,x) ((b,y):ys)   | a == b = (b,x+y) : ys
                           | otherwise =(b,y): add' (a,x) ys

vecMult :: (Eq x, Num x) => x -> Vec x a -> Vec x a
vecMult scalar (Vec xs) | scalar == 0 = Vec[]
                        | otherwise = Vec([(a,scalar*i)|(a,i)<-xs]) 

vecConcat :: (Eq a, Num x) => Vec x a -> Vec x a -> Vec x a 
vecConcat (Vec xs)(v) = foldr vecAdd v xs

instance Num n => Functor(Vec n) where
    -- fmap f (Vec xs) = Vec[(a,i*j) | (a,i)<-xs, j<- (f a)] 
    fmap = liftM

instance Num n => Applicative(Vec n) where
    pure = return
    (<*>) = ap

instance Num n => Monad(Vec n) where
    return x = Vec[(x,1)]
    (Vec xs) >>= f = Vec[(b,i*j) | (a,i)<-xs, (b,j)<- unVec(f a)]

-- Fazer uma funçao de comparaçao que compara os elementos de 1 vetor com todos os do outro.
vecFunc :: (Fractional x, Num a) => a -> Vec x a
vecFunc x= Vec[(x+1,(0.2))]
vec1 :: Vec Double Int
vec1 = return 0 :: Vec Double Int
vec2 = return 1 :: Vec Double Int
-- b = vecConcat a 

-- M2 vec sobre os positivos reais para simular a probabilistica
-- M2 vec sobre os complexos para simular a quantica
-- Fazer funçao que retire as probabilidades das amplitudes.
-- Funcao que receber n e faz n aplicacoes do bind ao vetor dentro de m2 ?
