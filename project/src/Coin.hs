module Coin where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??), )
import Control.Monad

data Coin = Heads | Tails deriving (Eq,Ord,Show)
type Probability = Rational
type Dist = Dist.T Probability

coin :: Dist Coin
coin = Dist.uniform[Heads,Tails]

flipOnce :: Dist (Coin, Coin)
flipOnce = liftM2 (,) coin coin

flipTwice :: Dist (Coin,Coin,Coin)
flipTwice = liftM3 (,,) coin coin coin