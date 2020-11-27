{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module QWalk where

import System.IO
import Vector
import Mtwo
import Data.Complex
sqrRoot = 1/sqrt(2) :: Complex Float

mTwoVec = return 0 :: M2 (Vec (Complex Float)) (Int)
hadamardCoin a = M2 $ Pair ( Vec[(In1( a+1), sqrRoot),(In2( a-1),sqrRoot)], Vec[(In1 (a+1), sqrRoot),(In2 (a-1),sqrRoot)])
