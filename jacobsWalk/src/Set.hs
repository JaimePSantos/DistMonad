{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}

module Set where

--Truque do AsMonad https://hsenag.livejournal.com/11803.html
--Paper do QIO https://www.cs.nott.ac.uk/~psztxa/g5xnsc/chapter.pdf
--Package do QIO https://hackage.haskell.org/package/QIO

import qualified Data.Set as S
import Control.Monad       (liftM, ap,mapM)

class OrdMonad m where
   ordReturn :: Ord a => a -> m a
   ordBind :: (Ord a, Ord b) => m a -> (a -> m b) -> m b

instance OrdMonad S.Set where
   ordReturn  = S.singleton
   s `ordBind`  f = S.fold (\v ret -> f v `S.union` ret) S.empty s

data AsMonad m a where
  Embed :: (OrdMonad m, Ord a) => m a -> AsMonad m a
  Return :: OrdMonad m => a -> AsMonad m a
  Bind :: OrdMonad m => AsMonad m a -> (a -> AsMonad m b) -> AsMonad m b

instance OrdMonad m => Functor (AsMonad m) where
  fmap = liftM

instance OrdMonad m => Applicative (AsMonad m) where
  pure = Return
  (<*>) = ap

instance OrdMonad m => Monad (AsMonad m) where
  return = Return
  (>>=) = Bind


unEmbed :: Ord a => AsMonad m a -> m a
unEmbed (Embed m) = m
unEmbed (Return v) = ordReturn v
unEmbed (Bind (Embed m) f) = m `ordBind` (unEmbed . f)
unEmbed (Bind (Return v) f) = unEmbed (f v)
unEmbed (Bind (Bind m f) g) = unEmbed (Bind m (\x -> Bind (f x) g))

setExample = unEmbed $ do
  x <- Embed $ S.fromList [1,2,4]
  y <- Embed $ S.fromList [1,2,4]
  return (x*y)
