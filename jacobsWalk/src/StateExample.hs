{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}

module  StateExample where

import Control.Monad.State
import System.Random
import Control.Monad (replicateM)
import Control.Applicative (liftA3)
import Data.Functor.Identity

stateExample :: Int -> State Bool Int
stateExample x = do s<-get
                    if s then return (x+1) else return (x-1)
printStateExample = runState (stateExample 2) True

stateExample2 :: Int -> State Bool Int
stateExample2 x = do s<-get
                     put(not s)
                     return x 
printStateExample2 = runState (stateExample2 0) True
printStateExampleBind = runState (return 0 >>= stateExample >>= stateExample2) False 

--Dice
data Die = DieOne
           | DieTwo
           | DieThree
           | DieFour
           | DieFive
           | DieSix
           deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
            1 -> DieOne
            2 -> DieTwo
            3 -> DieThree
            4 -> DieFour
            5 -> DieFive
            6 -> DieSix
            x -> error $"intToDie got non 1-6 integer:" ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
   (n, s) <- randomR (1, 6)
   return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state(randomR(1,6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie
printDice n seed = evalState (nDie n) (mkStdGen seed)

--Chapter Exercises
--1
myGet :: State s s
myGet = state $ \s -> (s,s)

--2
myPut :: s -> State s ()
myPut s = state $ \_->((),s)

--3
exec :: State s a -> s -> s
exec (StateT sa)  = snd . runIdentity . sa
