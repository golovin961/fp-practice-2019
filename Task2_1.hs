module Main where

import Prelude hiding (lookup)

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}


-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree 
                 | Node Integer v (TreeMap v) (TreeMap v)
                 deriving (Eq,Show)


-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree _ = False
contains (Node key _ left right) k
    | k < key = contains left k
    | k > key = contains right k
    | otherwise = True


-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ EmptyTree = error "key not found"
lookup k (Node key value left right)
    | k < key = lookup k left
    | k > key = lookup k right
    | otherwise = value

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTree = Node k v EmptyTree EmptyTree
insert (k, v) (Node key value left right)
    | k < key = Node key value (insert (k, v) left) right
    | k > key = Node key value left (insert (k, v) right)
    | otherwise = Node key value left right

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ EmptyTree = EmptyTree
remove i (Node key value left right)
    | i < key = Node key value (remove i left) right
    | i > key = Node key value left (remove i right)
    | otherwise = case (left, right) of
        (EmptyTree, EmptyTree) -> EmptyTree
        (left , EmptyTree) -> left
        (EmptyTree, right) -> right
        (left , right) -> remove' left right

remove' l EmptyTree = l
remove' l (Node key value EmptyTree right) = Node key value l right
remove' l (Node key value left right) = Node key value (remove' l left) right


-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ EmptyTree = error "Not found this key."
nearestLE i (Node key value left right)
    | key > i = nearestLE i left
    | key < i = case right of
        (Node key value _ _) | (i == key) -> (key, value)
        (Node key value _ _) | (i /= key) -> nearestLE i right
        otherwise -> (key, value)
    | otherwise = (key, value)

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmptyTree lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = case t of
    EmptyTree -> []
    Node key value left right -> listFromTree(left) ++ [(key, value)] ++ listFromTree(right)

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ EmptyTree = error "Empty tree"
kMean i (Node key value left right)
    | sizeLeft == i = (key, value)
    | sizeLeft > i = kMean i left
    | otherwise = kMean (i - sizeLeft - 1) right
    where sizeLeft = size left

size :: TreeMap v -> Integer
size EmptyTree = 0
size (Node _ _ left right) = (size left) + (size right) + 1


main :: IO ()
main = do
       let a = treeFromList [(1,2),(3,4),(5,6)]
           b = kMean 2 a
           c = listFromTree a
           d = nearestLE 3 a
           e = remove 1 a
           f = insert (7,8) a
           g = lookup 1 a
           h = contains a 3
       print a
       print b
       print c
       print d
       print e
       print f
       print g
       print h