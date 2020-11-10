{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Vector where

import Control.Monad(join,ap)
import Control.Applicative ()
import System.IO ()
import Data.Tuple ()
import Control.Applicative ()
import Data.Functor.Classes ()

data Vec x a = Vec{unVec::[(a,x)]}
