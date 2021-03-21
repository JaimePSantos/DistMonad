{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}

module MapMon where

import Control.Monad (liftM, ap,mapM)

data OrdRest m a where
  OrdReturn :: a -> OrdRest m a
  OrdBind :: Ord a => m a -> (a -> OrdRest m b) -> OrdRest m b

instance Functor (OrdRest m) where
  fmap = liftM

instance Applicative (OrdRest m) where
  pure = return
  (<*>) = ap

instance Monad (OrdRest m) where
  return = OrdReturn
  OrdReturn x >>= f = f x
  OrdBind mx g >>= f = OrdBind mx $ \x -> g x >>= f

embed mx = OrdBind mx OrdReturn
unembed (OrdReturn x) = OrdReturn x
unembed (OrdBind mx g) = OrdBind mx $ unembed . g
