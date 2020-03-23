module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap f (FourOf a1 a2 a3 a4) = FourOf (f a1) (f a2) (f a3) (f a4)

instance Applicative FourOf where
    pure a = FourOf a a a a
    (<*>) (FourOf f1 f2 f3 f4) (FourOf a1 a2 a3 a4) = FourOf (f1 a1) (f2 a2) (f3 a3) (f4 a4)


instance Monad FourOf where
  return = pure
  (>>=) (FourOf a1 a2 a3 a4) f = FourOf (eFirst (f a1)) (eSecond (f a2)) (eThird (f a3)) (eFourth (f a4)) where
    eFirst (FourOf a _ _ _) = a
    eSecond (FourOf _ a _ _) = a
    eThird (FourOf _ _ a _) = a
    eFourth (FourOf _ _ _ a) = a

x = do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } == FourOf 5 8 10 12


main :: IO ()
main = do print x
