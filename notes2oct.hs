import Control.Monad 
import Control.Applicative



data M2 t a = M2 (t (Either a a), t (Either a a))

instance (Functor t) => Functor (M2 t) where
     fmap :: (a -> b) -> (M2 t a -> M2 t b)
     fmap = undefined

instance (Monad t) => Applicative (M2 t) where
     pure :: a -> M2 t a
     pure  = undefined
     (<*>) = ap    


instance (Monad t) => Monad (M2 t) where
     (>>=) :: M2 t a -> (a -> M2 t b) -> M2 t b
     (>>=)  = undefined    


{-- verify properties

return x >>= f   ==   f x

x >>= return     ==   x

(f# . g)#   ==  f# . g#

x >>= (\y -> g y >>= f)   ==  (x >>= g) >>= f


--}



{--

data Maybe a = Nothing | Just a

Just [1,2,3] :: Maybe [Int]



M2 Maybe a = (Maybe (Either a a) , Maybe (Either a a))

m :: Maybe (Either a a)

m = Nothing 
ou
m = Just y where y :: Either a a


m = Nothing 
ou
m = Just (Left x)  where x :: a
ou
m = Just (Right x) where x :: a

--}



{--
 (a -> M2 t b) -> (M2 t a -> M2 t b)



functor:
(a -> b) -> (F a -> F b)

extension:
(a -> M b) -> (M a -> M b)

(=<<) :: (a -> M b) -> M a -> M b

(=<<) = flip (>>=)

f:: a -> M b

fmap f :: M a -> M (M b)
[M f]

join :: M (M b) -> M b
[mu]

join . fmap f :: M a -> Mb



--}



data Coprod3 a b c = Left3 a | Middle3 b | Right3 c



Prod x = [x]


split :: (a -> b1) -> (a -> b2) -> (a -> (b1,b2))
split f g x = (f x, g x)

split :: (a -> b1, a -> b2) -> (a -> (b1,b2))
split (f,g) x = (f x, g x)

split :: (a -> b, a -> b) -> (a -> (b,b))
split (f,g) x = (f x, g x)



split :: Prod (a -> b) -> (a -> Prod b)
split :: [a -> b] -> (a -> [b])


type Twice x = Either x x



Coprod x = (Int, x)

(2,'a') :: Coprod Char

Left 'a'      (0,'a') 
Right 'a'     (1,'a')

Left3   'a'     (0,'a') 
Middle3 'a'     (1,'a')
Right3  'a'     (2,'a')


either :: (a1 -> b) -> (a2 -> b) -> (Either a1 a2 -> b)
either f g (Left x)  = f x
either f g (Right x) = g x


either' :: (a1 -> b, a2 -> b) -> (Either a1 a2 -> b)
either' (f, g) (Left x)  = f x
either' (f, g) (Right x) = g x


coprod :: Prod (a -> b) -> (Coprod a -> b)


