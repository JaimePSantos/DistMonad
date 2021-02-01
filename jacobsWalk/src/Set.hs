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
import qualified Data.Foldable as Foldable

-- First approach is to wrap most of the Set structure into a new one and instance that as a monad in order to use it elsewhere. Perhaps this is pointless.
-- What would happen if we instance Data.Set as a monad in this file, and try to use it's monadic properties in QWalk, for example?
newtype SetMon a = SetMon{unSetMon :: S.Set a}

instance (Show a) => Show(SetMon a) where
  show(SetMon x) = show(x)

-- Migrating Set functions to SetMon.
-- Construction
empty  :: SetMon a
empty = SetMon $ S.empty
singleton :: a -> SetMon a
singleton a = SetMon $ S.singleton (a)
fromList :: Ord a => [a] -> SetMon a
fromList a = SetMon $ S.fromList a

--Insertion
insert :: Ord a => a -> SetMon a -> SetMon a
insert a s = SetMon $ S.insert a (unSetMon s)

--Deletion
delete :: Ord a => a -> SetMon a -> SetMon a
delete a s = SetMon $ S.delete a (unSetMon s)

--Combine
union :: Ord a => SetMon a -> SetMon a -> SetMon a
union s s' = SetMon $ S.union (unSetMon s) (unSetMon s')
unions :: (Foldable f, Ord a) => f (SetMon a) -> SetMon a
unions = Foldable.foldl' union empty
difference :: Ord a => SetMon a -> SetMon a -> SetMon a
difference s s' = SetMon $ S.difference (unSetMon s) (unSetMon s')
intersection :: Ord a => SetMon a -> SetMon a -> SetMon a
intersection s s' = SetMon $ S.intersection (unSetMon s) (unSetMon s')

class OrdMonad m where
   ordReturn :: Ord a => a -> m a
   ordBind :: (Ord a, Ord b) => m a -> (a -> m b) -> m b

instance OrdMonad SetMon where
   ordReturn  = SetMon. S.singleton
   s `ordBind`  f =  SetMon$ (S.fold (\v ret -> unSetMon(f v) `S.union` ret) S.empty (unSetMon(s)))

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

--setExample = unEmbed $ do
--  x <- Embed $ S.fromList [1,2,4]
--  y <- Embed $ S.fromList [1,2,4]
--  return (x*y)
