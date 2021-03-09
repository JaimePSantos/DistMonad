{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module MapWalk where

import System.IO
import Map 
import Mtwo
import Data.Complex
import Control.Monad       (liftM, ap,mapM)
import Data.Ratio
import qualified AsMonad as AM 

--TODO: Perceber como mudar de complex float para complex rational ou assim.
type QDist  = Dist (Complex Float)
 
sqrRoot = 1/sqrt(2) :: Complex Float 
hadamardCoinMap :: (Ord a, Num a) => a -> M2 (QDist) a 
hadamardCoinMap a = M2 $ Pair ( fromList[(In1( a-1), sqrRoot),(In2( a+1),sqrRoot)], fromList[(In1 (a-1), sqrRoot),    (In2 (a+1),-sqrRoot)])

initCondQuantumMap :: M2 (QDist) Int  
initCondQuantumMap = M2 $ Pair ( fromList [(In1 0, 1 :: Complex Float)] , fromList[(In2 0, 1 :: Complex Float)])

quantumWalkN :: (Ord a,Num a) => Int -> M2 (QDist) a -> M2 (QDist) a
quantumWalkN (0) state = state
quantumWalkN n state =  quantumWalkN (n-1) (state `AM.ordBind`  hadamardCoinMap) 
--quantumWalkN n state = quantumWalkN (n-1) (state' `AM.ordBind`  hadamardCoinMap') where
--  state' = unM2 $ state
--  hadamardCoinMap' a = unM2 $ hadamardCoinMap a

--quantumWalkN n state =  quantumWalkN (n-1) (M2 $ state `AM.ordBind`  hadamardCoinMap) 

--initCondQuantum = return distExample :: M2 (Dist Rational) (Dist Rational Int)
--initCondClassical = return 0 :: M2 (Vec (Rational)) (Int)

