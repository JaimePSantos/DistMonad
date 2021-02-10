{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}

module AsMonad where

import Control.Monad (liftM, ap,mapM)

class OrdMonad m where
   ordReturn :: Ord a => a -> m a
   ordBind :: (Ord a, Ord b) => m a -> (a -> m b) -> m b

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
