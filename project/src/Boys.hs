module Boys where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??), (?=<<), )

import Control.Monad

type Probability = Rational
type Dist a = Dist.T Probability a

data Child = Boy | Girl
             deriving (Eq,Ord,Show)

type Family = (Child, Child)

birth :: Dist Child
birth = Dist.uniform [Boy, Girl]

family :: Dist Family
family = liftM2 (,) birth birth

allBoys :: Dist.Event Family
allBoys(x0,x1) = (x0==Boy && x1 == Boy)


oneBoy :: Dist.Event Family
oneBoy(x0,x1) = (x0==Boy || x1 == Boy)

famWithBoy :: Dist Family
famWithBoy = oneBoy ?=<< family

twoBoys :: Probability
twoBoys = allBoys ?? famWithBoy


countBoy :: Child -> Int
countBoy Boy = 1
countBoy Girl = 0

countBoys :: Family -> Int
countBoys (c0,c1) = countBoy c0 + countBoy c1

numBoys :: Dist Int
numBoys = Dist.map countBoys famWithBoy