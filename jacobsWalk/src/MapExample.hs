{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module MapExample where

import System.IO
import Vector
import Mtwo
import Data.Complex
import Control.Monad       (liftM, ap,mapM)
