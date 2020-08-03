module NBoys where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??), (?=<<), )

import Control.Monad

type Probability = Rational
type Dist a = Dist.T Probability a

data Child = Boy | Girl
             deriving (Eq,Ord,Show)

type Family = [Child]

birth :: Dist Child
birth = Dist.uniform [Boy,Girl]

family :: Int -> Dist Family
family n = replicateM n birth
family2 = family 2

countBoys :: Family -> Int
countBoys = length.filter(==Boy)

boys :: Int -> Dist.Event Family
boys k f = countBoys f >= k

nBoys :: Int -> Int -> Int -> Probability
nBoys n k m =  boys m ?? boys k ?=<< family n

numBoys :: Int -> Int -> Dist Int
numBoys n k = Dist.map countBoys (boys k ?=<< family n)