module Dice where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??), )
import Control.Monad

type Die = Int
type Probability = Rational
type Dist = Dist.T Probability
-- (??) :: Num prob => Event a -> T prob a -> prob 
-- type Event a = a -> Bool

die:: Dist Die
die = Dist.uniform[1..6]
-- = Dist.uniform[1..6]

twoDice :: Dist (Die,Die)
twoDice = liftM2 (,) die die -- lift takes a function with two arguments, combines the values into the type and then flattens it.
threeDice = liftM3 (,,) die die die

dice:: Int -> Dist [Die]
dice = flip replicateM die --we use flip for partial function definition.

twoSixes :: Probability
twoSixes = (==(6,6)) ?? twoDice
threeSixes = (==(6,6,6)) ?? threeDice

sixes :: Int  -> Int -> Probability
sixes p n = ((==p) . length . filter (==6)) ?? dice n 

getFreq :: Int  -> Int -> Int -> Probability
getFreq p n k = ((==p) . length . filter (==k)) ?? dice n -- Gives us the probability of getting p number of k faces in n dice.

rollDieOne :: Dist Die
rollDieOne = liftM2 (+) (Dist.uniform[0,1]) die

plus1 x = Dist.choose 0.5 x (x+1)
rollDieOne' = do
    d <- die
    plus1 d

addTwo :: Dist Die
addTwo = do
    d1 <- die
    d2 <- die
    return (d1+d2)

addTwo' :: Dist Die 
addTwo' = liftM2 (+) die die

-- rollDie' :: Dist [Die]
-- dice''  = dice 1
-- rollDie' = liftM2 (+) dice'' dice''

-- rollDie'' :: Int -> Dist [Die]
-- rollDie'' = flip replicateM (rollDie')  

joinWith:: (a->b->c) -> Dist.T Probability a-> Dist.T Probability b -> Dist.T Probability c
joinWith f (Dist.Cons d)(Dist.Cons d') = Dist.Cons [(f x y, p*q)|(x,p)<-d, (y,q)<-d']

dice' :: Int->Dist [Int]
dice' 0 = Dist.certainly []
dice' n = joinWith (:) die (dice'(n-1))

type Trans prob a = a -> Dist.T prob a