module Main where
{-
  Задание 3.3
  Множество на основе предикатов
-}

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

newtype PSet1 a = PSet1{ contains1 :: (a -> Bool) }
newtype PSet2 a = PSet2{ contains2 :: (a -> Bool) }
newtype PSet3 a = PSet3{ contains3 :: (a -> Bool) }

instance Semigroup (PSet1 a) where
    (<>) = mappend

instance Semigroup (PSet2 a) where
    (<>) = mappend

instance Semigroup (PSet3 a) where
    (<>) = mappend

-- (A && B) (Пересечение множеств)
instance Monoid (PSet1 a) where
    mempty = PSet1 (\a -> True)
    mappend (PSet1 a) (PSet1 b) = PSet1 (\x -> (a x) && (b x))

-- (A || B) (Объединение множеств)
instance Monoid (PSet2 a) where
    mempty = PSet2 (\a -> False)
    mappend (PSet2 a) (PSet2 b) = PSet2 (\x -> (a x) || (b x))

-- (A && not B) (разность множеств)
-- не реализовать, так как не выполним закон моноида:
-- mappend x mzero === mappend mzero x === x

--  (A && not B) || (not A && B) (Симметрическая разность множеств)
instance Monoid (PSet3 a) where
    mempty = PSet3 (\a -> False)
    mappend (PSet3 a) (PSet3 b) = PSet3 (\x -> ((a x) && (not $ b x)) || ((not $ a x) && (b x)))

-- fmap :: (a -> b) -> f a -> f b
-- Если у нас есть только отображение из множества A в B, то мы не можем
-- получить никакой информации о множестве В и нормально реализовать работу
-- функтора, поэтому он всегда возвращает False.
instance Functor PSet1 where
    fmap _ _ = PSet1 (\_ -> False)

instance Functor PSet2 where
    fmap _ _ = PSet2 (\_ -> False)

instance Functor PSet3 where
    fmap _ _ = PSet3 (\_ -> False)
