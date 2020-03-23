{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

module Task3_2 where

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil         = []
rlistToList (RCons xs x) = (rlistToList xs) ++ [x]

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList x  = RCons (listToRList $ init x) (last x)

instance (Show a) => Show (ReverseList a) where
    show RNil               = "[]"
    show (RCons RNil x) = show x
    show (RCons x y) = show y ++ ", " ++ show x

instance (Eq a) => Eq (ReverseList a) where
  (==) RNil RNil = True
  (==) _ RNil    = False
  (==) RNil _    = False
  (==) (RCons x1 y1) (RCons x2 y2) = x1 == x2 && y1 == y2

instance (Ord a) => Ord (ReverseList a) where
  (<=) RNil _ = True
  (<=) _ RNil = False
  (<=) (RCons x1 y1) (RCons x2 y2) = x1 <= x2 || y1 <= y2


instance Semigroup (ReverseList a) where
    (<>) = mappend

instance Monoid (ReverseList a) where
  mempty                = RNil
  mappend RNil x        = x
  mappend x RNil        = x
  mappend x (RCons y z) = RCons (mappend x y) z
  mconcat = foldr mappend mempty

instance Functor ReverseList where
  fmap _ RNil         = RNil
  fmap f (RCons xs x) = RCons ( fmap f xs ) (f x)

main :: IO ()
main = do
       let a = [1,2,3,4,5,6,7,8,9]
           b = listToRList a
       print b