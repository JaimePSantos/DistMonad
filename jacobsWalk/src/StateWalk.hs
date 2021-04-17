{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module StateWalk where

import System.IO
import Map 
import Data.Complex
import Control.Monad       (liftM, ap,mapM,join)
import Data.Ratio
import qualified AsMonad as AM 
import StateMtwo
import Control.Monad.State

type QDist  = AsMonDist(Complex Float)
type CDist = AsMonDist(Float) 

stateInitCondQMap :: (Num a,Ord a) =>StateM2 (QDist) a
stateInitCondQMap = StateT $ func4( fromListAM[((0,H),1 :: Complex Float)],  fromListAM[((0,T),1 :: Complex Float)])

sqrRoot = 1/sqrt(2) :: Complex Float 
stateHadamardCoinMap :: (Ord a, Num a) => a -> StateM2 (QDist) a 
stateHadamardCoinMap a = StateT $ func4 (fromListAM[((a-1,H),sqrRoot),((a+1,T),sqrRoot)],fromListAM[((a-1,H),sqrRoot),((a+1,T),-sqrRoot)])

stateQuantumWalkNMap :: (Ord a,Num a) => Int -> StateM2 (QDist) a -> StateM2 (QDist) a
stateQuantumWalkNMap (0) state = state
stateQuantumWalkNMap n state =  stateQuantumWalkNMap (n-1) (state >>= stateHadamardCoinMap)

probsState :: (Ord a, Num a) => Int -> StateM2 (CDist) a 
probsState n = mapStateT getProbsAM $ stateQuantumWalkNMap n stateInitCondQMap

printProbs n = evalStateT(probsState n) T 

printStateQuantumWalkMap n = runStateT (stateQuantumWalkNMap n stateInitCondQMap) H 

