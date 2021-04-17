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
type StateM2 t a  = StateT Two t a 

--func :: Twice a -> (a,Two)
--func (In1 a) = (a,O)
--func (In2 a) = (a,T)
--func2 :: (a,Two) -> Twice a
--func2 (a,O) = In1 a
--func2 (a,T) = In2 a
--
--func3:: (Two -> a) -> (a,a)
--func3 f = ( f O, f T)

func4 :: (a,a) -> (Two -> a)
func4 p H = fst p 
func4 p T = snd p 

--func4 :: [a] -> Int -> a) 
--examplefunc1 :: Num a => a -> StateT Two [] a
examplefunc1 :: Num a => a -> StateM2 [] a
examplefunc1 a =  StateT $ func4 ([(a-1,H) , (a+1,T)],[(a-1,H) ,(a+1,T)])
--exampleWalk = return 0 :: M2 [] Int
--TODO: Fazer uma qwalk para o State Two
--TODO: Definir State s
--TODO: Fazer as walks para o State.



