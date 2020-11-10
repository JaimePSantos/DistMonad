module MountyHall where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Transition as Trans
import Numeric.Probability.Simulation 
import Numeric.Probability.Percentage 
import Control.Monad
import Numeric.Probability.Distribution ((??), )

import Control.Monad

import Data.List

-- import qualified Numeric.Probability.Monad as MonadExt

-- certainly :: Num prob => a -> T prob a
-- certainly x = Cons [(x,1)]
-- type Trans a = a -> Dist a
-- compose :: Monad m => [a -> m a] -> a -> m a
-- compose = foldl (flip (<=<)) return
-- type Probability = Rational
-- type Dist = Dist.T Probability

data Outcome = Win | Lose deriving (Eq,Ord,Show)

firstChoice :: Dist Outcome
firstChoice = Dist.uniform[Win, Lose, Lose]

switch :: Trans Outcome
switch Win = Dist.certainly Lose
switch Lose = Dist.certainly Win

-- choices :: Dist [Outcome]
choices = flip replicateM firstChoice
twoChoice = choices 2

twoChoices = liftM2 (,) firstChoice firstChoice
-- lLoser = (length.filter(==Lose)) ?? choices

data Door = A | B | C deriving(Eq, Ord, Show)

doors:: [Door]
doors = [A,B,C]

-- data State = Doors{prize :: Door, chosen :: Door, }