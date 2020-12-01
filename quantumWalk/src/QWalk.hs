{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module QWalk where

import System.IO
import Vector
import Mtwo
import Data.Complex

--Duvidas:O M2 funciona para a condicao inicial? M2 tera sempre que ter um par por causa da moeda, mas o estado inicial nao sera um par. Matematicamente, o estado resultante de uma iteracao da quantum walk e Psi(t) = U * Psi(0) e nao estou a ver como isto traduz para M2. Talvez nao esteja simplesmente a perceber a notacao.
sqrRoot = 1/sqrt(2) :: Complex Float
--Duvidas: O tipo de mTwoVec esta correto? Posso usar isto como condicao inicial? Penso que nao por causa da duvida anterior.
mTwoVec = return 0 :: M2 (Vec (Complex Float)) (Int)
--Duvidas: Penso que a moeda esta correta, rever no entanto.
hadamardCoin a = M2 $ Pair ( Vec[(In1( a+1), sqrRoot),(In2( a-1),sqrRoot)], Vec[(In1 (a+1), -sqrRoot),(In2 (a-1),sqrRoot)])

initCond = M2(  
