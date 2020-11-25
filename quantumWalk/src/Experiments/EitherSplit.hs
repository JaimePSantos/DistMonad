module Experiments.EitherSplit where

{--
    To do: define the product and coproduct universal properties for these
           types, namely the functions "either" and "split" but instanciated
           to coproducts/products where both components are the same set.
      
           I.e. the usual either and split have types:
              either :: (a -> c) -> (b -> c) -> (Either a b -> c)
              split  :: (c -> a) -> (c -> b) -> (c -> Pair a b)
           We just need to make similar functions but with a = b, namely:
              eitherT :: (a -> c) -> (a -> c) -> (Twice a -> c)
              splitS  :: (c -> a) -> (c -> a) -> (c -> Square a)
           such that they satisfy
              either f g == twice2either . eitherT f g
              split  f g == square2pair  . eitherT f g
           for the obvious isomorphisms:
              twice2either :: Twice  a -> Either a a  -- implement these, too!
              square2pair  :: Square a -> Pair   a a

------ N.B. I'm using here the type synonym 
               type Pair a b = (a,b)
            to make the notation more uniform between Pair and Either.
--}
import Control.Monad 
import Control.Applicative
import System.IO
import Data.Tuple
import Control.Applicative
import Data.Functor.Classes

either' :: (a -> c) -> (b -> c) -> (Either a b -> c)
either' f _ (Left a) = f a
either' _ g (Right a) = g a 

split' :: (c->a) -> (c->b) -> (c->(a,b))
split' = undefined



