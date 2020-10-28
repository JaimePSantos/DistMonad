{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Mtwo where
import Control.Monad(join,ap)
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
data Twice a  = In1 a | In2 a      -- this is the functor T(A) = A + A with injections In1 and In2
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
           such that they satisfy:
              either  f g == eitherT f g . either2twice  -- isto era o que eu queria ter posto, desculpa
              split  f g  == square2pair . splitT  f g

              eitherT f g == either  f g . twice2either  -- mas tambem podia ter escrito assim
                                                         -- orque twice2either e' iso, tem uma
                                                         -- inversa either2twice. 
              splitT f g  == pair2square . split f g     -- o mesmo para square2pair / pair2square
           for the obvious isomorphisms:
              twice2either :: Twice  a -> Either a a  -- implement these, too!
              square2pair  :: Square a -> Pair   a a

------ N.B. I'm using here the type synonym 
               type Pair a b = (a,b)
            to make the notation more uniform between Pair and Either.
--}
-- (f <<= ) = f#
-- x >>= f = f# x


data M2 t a  = M2 {unM2 :: Square  (t (Twice a)) }

data M2' t a = M2'{unM2':: Square' (t (Twice a)) }



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

-- instance (Show1 f, Show a) => Show (f a) where showsPrec = showsPrec1



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

either2Twice :: Either a a -> Twice a
either2Twice (Left a) = In1 a
either2Twice (Right b) = In2 b

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

join1 ::(Monad m) => m(m a) -> m a
join1 x = x >>= id

first1 :: Square a -> a
first1 (Pair(a,b)) = a

second1 :: Square a -> a
second1 (Pair(a,b)) = b

-- unWrap :: (Monad t) => M2 t a -> Square t a
-- unWrap  = 
--M2
instance (Functor t) => Functor(M2 t) where
     fmap :: (a -> b) -> (M2 t a -> M2 t b)
     fmap f (M2 t) = M2 $ (fmap.fmap.fmap) f t

instance (Monad t) => Applicative (M2 t) where
     pure :: a -> M2 t a
     pure = return
     -- M2 f <*> M2 t = M2 $ (<*>) <$> f <*> t
     (<*>) = ap

-- sharp ::(Monad t)=> (a->M2 t b)->(M2 t a -> M2 t b)
-- sharp f = M2. fmap(join1.fmap(eitherT(first1.f) (second1.f)))
{--
     Still some issues defining the unwrap.
     Should it be from M2 t a -> Square a, or M2 t a -> Square t a and
     redefine the first1 and second1 ?
--}

instance (Monad t) => Monad (M2 t) where
     return :: a -> M2 t a
     return = M2. splitS (fmap(In1).return) (fmap(In2).return) 
     -- return  =(splitS (fmap()
     -- return = pure
     -- (>>=) :: M2 t a -> (a -> M2 t b) -> M2 t b
     -- (>>=)= undefined
     -- f =<< = Square(join.t(either f g),join.t(either f g))

{--
-- Questoes Return--
definiçao do eta = <T(Ki) . etaX> -> isto significa que tenho de fazer um split f g,
onde f será o In1 "wrapped" na monad T composto com a identidade, e g igual para In2?
No caso de T ser uma lista seria por exemplo: split ([In1].id?) ([In2].id?)
Na tentativa de cima deverei subsituir (t(In1)) por pure? Tambem nao tenho a certeza da funçao id.

-- Questoes Extension--
definiçao: f# = (mu_{T(n.Y)} . t ([f_i]_i<=n))^n
Significa que esta definiçao é constituida por um par para n =2, composto pelo join 
T(n.Y) (nao tenho a certeza como definir isto) composto com o either de f1 e f2 
(como decomponho o f da definiçao em 2 funçoes?).
--}

-- \\ -- \\ -- \\ -- \\ --

--Examples

-- a = M2 $ Pair(([In1 1], [In2 2]))

--aux = M2 $ Pair(([In1 (+2)], [In2 (+2)]))
--plusTwoAp = aux <*> a

-- fExample :: Int->Int
-- fExample= (+1)
-- gExample:: Int-> Int
-- gExample = (+2)
-- aExample :: Twice Int
-- aExample = In1 $ 1
-- bExample ::Either Int Int
-- bExample = Right 1

-- eitherTTest :: (Int->Int) -> (Int->Int) ->(Twice Int -> Int)
-- eitherTTest f g a = either f g (twice2Either(a))

-- eitherTest :: (Int->Int) -> (Int->Int) ->(Either Int Int -> Int)
-- eitherTest f g = eitherT f g . either2Twice

-- eitherEq :: (Int->Int) -> (Int->Int) -> (Either Int Int-> Bool)
-- eitherEq f g x = (either f g x) ==( eitherTest f g x)

-- eitherTEq :: (Int->Int) -> (Int->Int) -> (Twice Int-> Bool)
-- eitherTEq f g x = eitherT f g x == eitherTTest f g x

