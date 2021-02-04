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
import qualified AsMonad as AM 

-- First approach is to wrap most of the Set structure into a new one and instance that as a monad in order to use it elsewhere. Perhaps this is pointless.
-- What would happen if we instance Data.Set as a monad in this file, and try to use it's monadic properties in QWalk, for example?
newtype SetMon a = SetMon{unSetMon :: S.Set a}

instance (Show a) => Show(SetMon a) where
  show(SetMon x) = show(x)

instance AM.OrdMonad SetMon where
   ordReturn  = SetMon. S.singleton
   s `ordBind`  f =  SetMon$ (S.fold (\v ret -> unSetMon(f v) `S.union` ret) S.empty (unSetMon(s)))

-- Migrating Set functions to SetMon.
-- Construction
empty  :: SetMon a
empty = SetMon $ S.empty
singleton :: a -> SetMon a
singleton a = SetMon $ S.singleton (a)
fromList :: Ord a => [a] -> AM.AsMonad SetMon a
fromList a = AM.Embed $ SetMon $ S.fromList a

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




setExample = AM.unEmbed $ do
  x <-  fromList [1,2,4]
  y <-  fromList [1,2,4]
  return (x*y)

