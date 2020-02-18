module Main where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

data BinOperator = Plus 
                   | Minus 
                   | Mult
                   deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ op::BinOperator, lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
infixl 7 |+|
(|+|) l r = BinaryTerm Plus l r


(|-|) :: Term -> Term -> Term

infixl 7 |-|
(|-|) l r = BinaryTerm Minus l r


(|*|) :: Term -> Term -> Term

infixl 8 |*|
(|*|) l r = BinaryTerm Mult l r


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
  Variable variable | variable == varName -> replacement
  BinaryTerm op lhv rhv -> BinaryTerm op (replace lhv) (replace rhv)
    where
      replace hv = replaceVar varName replacement hv
  _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
  BinaryTerm op lic ric ->
    case (op, left, right) of 
      (Plus, IntConstant left, IntConstant right) -> IntConstant (left + right)
      (Plus, left, IntConstant 0) -> left
      (Plus, IntConstant 0, right) -> right
      (Minus, IntConstant left, IntConstant right) -> IntConstant (left - right)
      (Minus, left, IntConstant 0) -> left
      (Mult, IntConstant left, IntConstant right) -> IntConstant (left * right)
      (Mult, IntConstant 1, right) -> right
      (Mult, IntConstant 0, right) -> IntConstant 0
      (Mult, left, IntConstant 0) -> IntConstant 0
      (Mult, left, IntConstant 1) -> left
      _ -> BinaryTerm op left right 
      where
        left  = evaluate lic
        right = evaluate ric
  _ -> expression
