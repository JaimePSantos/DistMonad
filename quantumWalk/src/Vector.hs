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

vecAdd(a,x) (Vec xs) = Vec(add' xs) where
    add' [] = [(a,x)]
    add' ((b,y):ys)   | a == b = (b,x+y) : ys
                      | otherwise = add' ys

vecMult :: (Eq x, Num x) => x -> Vec x a -> Vec x a
vecMult scalar (Vec xs) | scalar == 0 = Vec[]
                        | otherwise = Vec([(a,scalar*i)|(a,i)<-xs]) where

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


func :: (Fractional x, Num a) => a -> Vec x a
func x= Vec[(x+1,(0.2))]
a :: Vec Double Int
a = return 0 :: Vec Double Int