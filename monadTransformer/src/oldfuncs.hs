-- data SquareTwice a = SquareTwice(Square(Twice a)) deriving Show
-- data SquareTwice a = SquareTwice(Square(Twice a, Twice a)) deriving Show

-- instance Functor Square where
--      fmap f (Square(a,b)) = Square $ (f a, f b)

-- instance Functor SquareTwice where
--      fmap f (SquareTwice(Square(First a,First b))) = SquareTwice $ Square (First $ f a,  First $ f b)
--      fmap f (SquareTwice(Square(First a,Second b))) =SquareTwice $ Square (First $ f a,  Second $ f b)
--      fmap f (SquareTwice(Square(Second a,First b))) = SquareTwice $ Square (Second $ f a,  First $ f b)
--      fmap f (SquareTwice(Square(Second a,Second b))) =SquareTwice $ Square (Second $ f a,  Second $ f b)

-- instance (Functor t) => Functor(M2 t) where
     -- fmap :: (a -> b) -> (M2 t a -> M2 t b)
     -- fmap = undefined
     -- fmap f (M2 t) = M2 $ (fmap.fmap) f t

--M2
-- instance Functor M2 where
--      fmap = undefined

-- instance Applicative M2 where
--      pure = return
--      (<*>) = ap

--SquareTwice   
-- instance Functor SquareTwice where
--      fmap f (SquareTwice(First a,First b)) = SquareTwice (First $ f a,  First $ f b)
--      fmap f (SquareTwice(First a,Second b)) =SquareTwice  (First $ f a,  Second $ f b)
--      fmap f (SquareTwice(Second a,First b)) = SquareTwice (Second $ f a,  First $ f b)
--      fmap f (SquareTwice(Second a,Second b)) =SquareTwice (Second $ f a,  Second $ f b)

--hello
-- instance Applicative SquareTwice where
--      pure = return
--      (SquareTwice(First f,First g)) <*> (SquareTwice(First a,First b)) = SquareTwice (First $ f a,  First $ g b)
--      (SquareTwice(First f,Second g)) <*> (SquareTwice(First a,Second b)) = SquareTwice (First $ f a,  Second $ g b)
--      (SquareTwice(Second f,First g)) <*> (SquareTwice(Second a,First b)) = SquareTwice (Second $ f a,  First $ g b)
--      (SquareTwice(Second f,Second g)) <*> (SquareTwice(Second a,Second b)) = SquareTwice (Second $ f a,  Second $ g b)

--      (SquareTwice(First f,First g)) <*> _ = undefined
--      (SquareTwice(First f,Second g)) <*> _ = undefined
--      (SquareTwice(Second f,First g)) <*> _ = undefined
--      (SquareTwice(Second f,Second g)) <*> _ = undefined

-- instance Monad SquareTwice where
--      return = pure
--      (>>=) = undefined


-- \\ -- \\ -- \\ -- \\ --

-- instance (Monad t) => Applicative (M2 t) where
--      pure :: a -> M2 t a
--      pure = M2 . pure .pure
--      M2 f <*> M2 t = M2 $ (<*>) <$> f <*> t
--      (<*>) = ap

-- instance (Monad t) => Monad (M2 t) where
--      return :: a -> M2 t a
--      return = pure
--      (>>=) :: M2 t a -> (a -> M2 t b) -> M2 t b
--      (>>=)= undefined

-- b = M2 $ SquareTwice (First (Just 1),Second Nothing)
-- c = M2 $ SquareTwice (First [1],Second [2])

-- plusTwo = (+2) 
-- plusTwo' = (+2) 

-- aux = M2 $(SquareTwice(First[(+2),(+2)],First[(+2)]))

-- plusTwo'' = aux <*> a 

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

-- sim, estao certos agora o either e o Pair
-- nao vale a pena e acrescentar construtures so como wrappers, em especial o do Either.
-- podes fazer antes:
-- type Square a = Pair {pi1 :: a, pi2 :: a} 
-- data Twice a = In1 a | In2 a deriving Show

-- e as definições depois ficam mais limpas.

-- O M2 é que ainda ainda nao bem. A ideia á usares o Twice (i.e. EitherAux) e o Square (i.e. PairAux) para definir o M2.

--Helper structures
-- data Twice a  = In1 a | In2 a      -- this is the functor T(A) = A + A with injections In1 and In2
-- data Square a = Pair(a,a)     
-- data Square' a =Pair' {pi1 :: a, pi2 :: a}  --this is the functor S(A) = A x A with projections pi1 and pi2
-- data Vec c a = Vec {unVec::[(c,a)]} --deriving Show
