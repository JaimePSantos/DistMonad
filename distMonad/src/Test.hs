module Test where

-- import Numeric.Probability.Show (showR)
import qualified Numeric.Probability.Shape as Shape
import qualified Numeric.Probability.Distribution as Dist

import Control.Applicative (Applicative(..))
import Control.Monad 

-- import qualified Data.Foldable as Fold
-- import qualified Data.List.HT as ListHT
-- import qualified Data.Map  as Map
-- import qualified Data.List as List
-- import qualified Data.Maybe as Maybe
-- import Data.Tuple.HT (mapFst, )
-- import Data.Ord.HT (comparing, )
-- import Data.Eq.HT (equating, )
newtype T prob a = Cons {decons :: [(a,prob)]}

certainly :: Num prob => a -> T prob a
certainly x = Cons [(x,1)]

instance Num prob => Monad (T prob) where
  return   = certainly
  d >>= f  = Cons [(y,q*p) | (x,p) <- decons d, (y,q) <- decons (f x)]

instance Num prob => Applicative (T prob) where
  pure     = certainly
  fm <*> m = Cons [(f x,q*p) | (f,p) <- decons fm, (x,q) <- decons m]

instance Functor (T prob) where
  fmap f (Cons d) = Cons [(f x,p) | (x,p) <- d]

-- uniform' :: Fractional prob => T prob
-- uniform' = const 1
uniform'' = Dist.shape

(>@>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >@> g = (>>= g) . f
