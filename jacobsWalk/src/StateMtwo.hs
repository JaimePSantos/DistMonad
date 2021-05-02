{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}

module StateMtwo where

import Control.Monad(join,ap)
import Control.Applicative
import System.IO
import Data.Tuple
import Control.Applicative
import Data.Functor.Classes
import Control.Monad.State
import System.Random
import Control.Monad (replicateM)
import Control.Applicative (liftA3)

data Two = H | T deriving (Show, Eq,Ord)
data D3 = One | Two | Three deriving (Show, Eq,Ord)
type CubeVertex = (Bool, Bool, Bool) 
type StateM2 t a  = StateT Two t a 
type StateM3 t a  = StateT D3 t a 

func4 :: (a,a) -> (Two -> a)
func4 p H = fst p 
func4 p T = snd p 

--
--
func4Cube :: (a,a,a) -> (D3 -> a)
func4Cube (x,_,_) One = x 
func4Cube (_,y,_) Two = y 
func4Cube (_,_,z) Three = z 







--func4 :: [a] -> Int -> a) 
--examplefunc1 :: Num a => a -> StateT Two [] a
examplefunc1 :: Num a => a -> StateM2 [] a
examplefunc1 a =  StateT $ func4 ([(a-1,H) , (a+1,T)],[(a-1,H) ,(a+1,T)])
--exampleWalk = return 0 :: M2 [] Int
--TODO: Fazer uma qwalk para o State Two
--TODO: Definir State s
--TODO: Fazer as walks para o State.



