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
