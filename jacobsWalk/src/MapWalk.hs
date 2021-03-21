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
type QDist  = AsMonDist(Complex Float)
 
sqrRoot = 1/sqrt(2) :: Complex Float 
hadamardCoinMap :: (Ord a, Num a) => a -> M2 (QDist) a 
hadamardCoinMap a = M2 $ Pair ( fromListAM[(In1( a-1), sqrRoot),(In2( a+1),sqrRoot)], fromListAM[(In1 (a-1), sqrRoot),    (In2 (a+1),-sqrRoot)])

initCondQuantumMap :: M2 (QDist) Int  
initCondQuantumMap = M2 $ Pair ( fromListAM [(In1 0, 1 :: Complex Float)] , fromListAM [(In2 0, 1 :: Complex Float)])
--
quantumWalkNMap :: (Ord a,Num a) => Int -> M2 (QDist) a -> M2 (QDist) a
--quantumWalkN = undefined
quantumWalkNMap (0) state = state
quantumWalkNMap n state =  quantumWalkNMap (n-1) (state >>= hadamardCoinMap) 
--quantumWalkN n state = quantumWalkN (n-1) (state' `AM.ordBind`  hadamardCoinMap') where
--  state' = unM2 $ state
--  hadamardCoinMap' a = unM2 $ hadamardCoinMap a

--quantumWalkN n state =  quantumWalkN (n-1) (M2 $ state `AM.ordBind`  hadamardCoinMap) 

--initCondQuantum = return distExample :: M2 (Dist Rational) (Dist Rational Int)
--initCondClassical = return 0 :: M2 (Vec (Rational)) (Int)

