{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module QWalk where

import System.IO
import Vector
import Mtwo
import Data.Complex
import Control.Monad       (liftM, ap)

sqrRoot = 1/sqrt(2) :: Complex Float
initCond = return 0 :: M2 (Vec (Complex Float)) (Int)

hadamardCoin a = M2 $ Pair ( Vec[(In1( a-1), sqrRoot),(In2( a+1),sqrRoot)], Vec[(In1 (a-1), sqrRoot),(In2 (a+1),-sqrRoot)])

--Isto resulta em duas quantum walks com 2 condicoes iniciais diferentes.
quantumWalkN :: (Num b) => Int -> M2( Vec( Complex Float)) b -> M2( Vec( Complex Float)) b
quantumWalkN (0) state = state
quantumWalkN n state = quantumWalkN (n-1) (state >>= hadamardCoin)

--TODO: Descobrir como aplicar funcs de vec dentro de M2.
--TODO: Implementar quantum walk probabilistica.
--walkTest = quantumWalkN 1 initCond
--walkProb = undefined
