{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Mtwo where

import Control.Monad 
import Control.Applicative
import System.IO
import Data.Tuple
import Control.Applicative
import Data.Functor.Classes

-- sim, estao certos agora o either e o Pair
-- nao vale a pena e acrescentar construtures so como wrappers, em especial o do Either.
-- podes fazer antes:
-- type Square a = Pair {pi1 :: a, pi2 :: a} 
-- data Twice a = In1 a | In2 a deriving Show

-- e as definições depois ficam mais limpas.

-- O M2 é que ainda ainda nao bem. A ideia á usares o Twice (i.e. EitherAux) e o Square (i.e. PairAux) para definir o M2.

--Helper structures
data Twice a  = In1 a | In2 a       -- this is the functor T(A) = A + A with injections In1 and In2
data Square a = Pair(a,a)     
data Square' a =Pair' {pi1 :: a, pi2 :: a}  -- this is the functor S(A) = A x A with projections pi1 and pi2


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
-- (f <<= ) = f#
-- x >>= f = f# x


data M2 t a = M2 (Square (t ( Twice a)))
data M2' t a = M2' (Square' (t ( Twice a)))



-- aqui esta o que e necessario para fazer o Show funcionar
-- a relevante e' que a ideia e' definir os tipos functor como instancias de Show1.
-- depois tendo (Show1 t) e (Show a) ficamos automaticamente com (Show (t a)).
-- os detalhes de como fazer o Show1 funciona nao interessam grande coisa.
-- na verdade, nem os percebi bem, fiz isto adaptando alguns exemplos.
-- o que interessa e que funciona e podemo-nos concentrar no que realmente interessa

instance (Show1 Twice) where
    liftShowsPrec sp _ d (In1 x) = showsUnaryWith sp "In1" d x
    liftShowsPrec sp _ d (In2 x) = showsUnaryWith sp "In2" d x

instance (Show1 Square') where
    liftShowsPrec sp _ d (Pair' x y) = showsBinaryWith sp sp "Pair" d x y

instance (Show1 Square) where
    liftShowsPrec sp _ d (Pair (x,y)) = showsBinaryWith sp sp "Pair" d x y

instance (Show1 t) => Show1 (M2 t) where
    liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> M2 t a -> ShowS
    liftShowsPrec sp l d (M2 (Pair (x,y))) = showsBinaryWith (lft sp l) (lft sp l) "Pair" d x y 
      where lft :: (Show1 t) => (Int -> a -> ShowS) ->  ([a] -> ShowS) -> (Int -> t (Twice a)  -> ShowS)
            lft sp l d =  liftShowsPrec (liftShowsPrec sp l) (liftShowList sp l) d 

instance (Show1 t) => Show1 (M2' t) where
    liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> M2' t a -> ShowS
    liftShowsPrec sp l d (M2' (Pair' x y)) = showsBinaryWith (lft sp l) (lft sp l) "Pair" d x y 
      where lft :: (Show1 t) => (Int -> a -> ShowS) ->  ([a] -> ShowS) -> (Int -> t (Twice a)  -> ShowS)
            lft sp l d =  liftShowsPrec (liftShowsPrec sp l) (liftShowList sp l) d 

--instance (Show1 f, Show a) => Show (f a) where showsPrec = showsPrec1



--Twice
instance Functor Twice where
     fmap f (In1 a) = In1( f a)
     fmap f (In2 a) = In2( f a) 

instance Applicative Twice where
     pure a = In1 a
     In1 f <*> In1 a = In1(f a)
     In2 f <*> In2 a = In2(f a) 
     In2 f <*> In1 a = undefined
     In1 f <*> In2 a = undefined
     --duvida em como emparelhar elementos diferentes do tipo
     -- In2 f <*> In1 a = In1(f a)
     -- In1 f <*> In2 a = In2(f a)

eitherT :: (a->c) -> (a->c) ->(Twice a -> c)
eitherT f _ (In1 a) = f a
eitherT _ g (In2 a) = g a

twice2Either :: Twice a -> Either a a
twice2Either( In1 a )= Left a
twice2Either( In2 a) = Right a


--Square
instance Functor Square where
     fmap f (Pair(a,b) )= Pair(f a, f b)

instance Applicative Square where
     pure  = return
     Pair(f,g) <*> Pair(a,b) = Pair(f a, g b)

instance Monad Square where
     return = pure
     (>>=) = undefined

splitS :: (c->a) -> (c->a) -> (c->Square a)
splitS f g a = Pair(f a, g a)

square2Pair ::Square a-> (a,a)
square2Pair( Pair(a,b)) = (a,b)

x = (Left 1) 
y = (In1 1)
f = (+1)
g = (+2)

--test = either f g x 
--test1 = eitherT f g y
--test1 = twice2Either.eitherT f g y

--M2

--instance (Show a) => Show (M2 [] a) where
--     show(M2 x) = "(M2 " ++ show (x) ++ ")"

--instance (Show a) => Show (M2 Maybe a) where
--     show(M2 x) = "(M2 " ++ show (x) ++ ")"

instance (Functor t) => Functor(M2 t) where
     fmap :: (a -> b) -> (M2 t a -> M2 t b)
     -- fmap = undefined
     fmap f (M2 t) = M2 $ (fmap.fmap.fmap) f t

instance (Monad t) => Applicative (M2 t) where
     pure :: a -> M2 t a
     pure = return
     -- M2 f <*> M2 t = M2 $ (<*>) <$> f <*> t
     (<*>) = ap
     
instance (Monad t) => Monad (M2 t) where
     return :: a -> M2 t a
     return = pure
     (>>=) :: M2 t a -> (a -> M2 t b) -> M2 t b
     (>>=)= undefined
     -- M2 x >>= f =(f =<< M2 x) 
     -- M2 x >>= M2 f a = M2 $ (fmap.fmap) f x 

-- \\ -- \\ -- \\ -- \\ --

--Examples

a = M2 $ Pair(([In1 1], [In2 2]))
aux = M2 $ Pair(([In1 (+2)], [In2 (+2)]))
plusTwoAp = aux <*> a

