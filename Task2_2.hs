module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}


import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x [] = x
foldl f x (h : t) = foldl f (f x h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x [] = x
foldr f x (h : t) = f h (foldr f x t)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f = maybe [] (\ (a, b) -> a : unfoldr f b) . f


-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldl (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\ item lst -> case item of 
                                      Just item -> item:lst
                                      Nothing -> lst) []

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal a = unfoldr (\ i -> if i + 1 > length a 
                     then Nothing
                     else Just(a !! i !! i, i + 1)) 0

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = foldr (\ x xs -> if f x then x : xs else xs) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem x = foldl (\ s x' -> x' == x || s) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step 
              | from > to = error "error: from > to" 
              | otherwise = unfoldr (\ from -> if from + 1 > to 
                then Nothing 
                else Just(from, from + step)) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append fst scd = foldr (:) scd fst

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = takeWhile (not.null) $ unfoldr (Just . splitAt (fromIntegral n)) lst


main :: IO ()
main = do
       let a = [5,6,7,8,9,10]
           b = sum a
           c = reverse a
           d = product a
           e = diagonal [[a]]
           f = elem 3 a
           g = rangeTo 0 10 2
           h = append a a
           i = groups a 2 
       print a
       print b
       print c
       print d
       print e
       print f
       print g
       print h
       print i
