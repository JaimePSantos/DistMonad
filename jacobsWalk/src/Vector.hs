{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Vector where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import System.IO
import Data.Tuple
import Control.Applicative
import Data.Functor.Classes
import Data.List
import Data.Function
import Data.Complex

data Vec x a = Vec{unVec::[(a,x)]}-- deriving Show
data Vec1 x a = Vec1{unVec1::(a,x)}

vecZero :: Vec x a
vecZero = Vec []

vecFromList l = Vec l 

vecAdd :: (Eq a, Num x) => (a,x) -> Vec x a ->  Vec x a

--TODO: Tratar do caso de x+y = 0 para retirar la de dentro
vecAdd(a,x) (Vec xs) = Vec(add' (a,x) xs) where
    add'(a,x) [] = [(a,x)]
    add'(a,x) ((b,y):ys)   | a == b = (b,x+y) : ys
                           | otherwise =(b,y): add' (a,x) ys

vecMult :: (Eq x, Num x) => x -> Vec x a -> Vec x a
vecMult scalar (Vec xs) | scalar == 0 = Vec[]
                        | otherwise = Vec([(a,scalar*i)|(a,i)<-xs]) 

vecConcat :: (Eq a, Num x) => Vec x a -> Vec x a -> Vec x a 
vecConcat (Vec xs)(v) = foldr vecAdd v xs

--TODO: Alterar esta funcao para truncar os vetores direitos. 
sumAmplitudes::(Num b) => [(a,b)] -> (a,b) 
sumAmplitudes stateList =
    let states = map fst stateList
        amplitudes = map snd stateList
    in (head states, sum amplitudes)

stateListTrunc ::(Ord a, Num b) => [(a,b)] -> [(a,b)]
stateListTrunc = map(sumAmplitudes) . groupBy ((==) `on` fst) . sortBy(compare `on` fst) 

vecTrunc :: (Ord a,Num b)=> Vec b a  -> Vec b a
vecTrunc (Vec(l)) = vecFromList(stateListTrunc l)

vecProb ::(Num a,RealFloat a) => Vec (Complex a) b -> Vec a b
vecProb (Vec[]) = Vec[] 
vecProb (Vec l) = Vec( vecProb' l) where
    vecProb' [] = []
    vecProb'((j,k):ks) = (j,realPart(abs(k)^2)) : vecProb' ks 

vecDistUniform :: (Fractional a,Eq b,Num b) => a -> b -> Vec a a 
vecDistUniform a b = Vec((buildVec a b)) where
    buildVec  x 0 = [(x,1)]
    buildVec x y = (x,1) : buildVec (x+1) (y-1)

vecDistUniform1 l = Vec [(x,1 `div` length(l))  | x<-l]

instance Num n => Functor(Vec n) where
    fmap = liftM

instance Num n => Applicative(Vec n) where
    pure = return
    (<*>) = ap

instance Num n => Monad(Vec n) where
    return x = Vec[(x,1)]
    (Vec xs) >>= f = Vec[(b,i*j) | (a,i)<-xs, (b,j)<- unVec(f a)]

instance (Show a, Show b) => Show(Vec a b) where
    show(Vec x) = "Vec"++show(x)

vecFunc :: (Fractional x, Num a) => a -> Vec x a
vecFunc x= Vec[(x+1,(0.2))]
vec1 :: Vec Double Int
vec1 = return 0 :: Vec Double Int
vec2 = return 1 :: Vec Double Int
vec3 = return 2 :: Vec Double Int
vec4 = Vec[(1,0.5),(1,0.5),(2,0.3),(2,0.1),(1,0.1),(2,0.7),(3,0.5)]
listVec4=[(1,0.5),(1,0.5),(2,0.3),(2,0.1),(1,0.1),(2,0.7),(3,0.5)]
vec5 = Vec[(1,1/sqrt(2)),(1,1/sqrt(2))]
vec6 = return 0 :: Vec Rational Int
vec7 = Vec[(1,0.1),(1,0.1),(2,0.1),(1,0.1),(2,0.1),(3,0.1)]
listVec7 = [(1,0.1),(1,0.1),(2,0.1),(1,0.1),(2,0.1),(3,0.1)]
vecConc1 = vecConcat vec1 vec2

