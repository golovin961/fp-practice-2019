module Main where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Prev WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

isSimple :: WeirdPeanoNumber -> Bool
isSimple Zero = True
isSimple (Succ (Prev _)) = False
isSimple (Prev (Succ _)) = True
isSimple (Succ num) = isSimple num
isSimple (Prev num) = isSimple num

firstSimplify :: WeirdPeanoNumber -> WeirdPeanoNumber
firstSimplify (Zero) = Zero
firstSimplify (Succ (Prev num)) = firstSimplify num
firstSimplify (Prev (Succ num)) = firstSimplify num
firstSimplify (Succ num) = Succ $ firstSimplify num
firstSimplify (Prev num) = Prev $ firstSimplify num

simplify :: WeirdPeanoNumber -> WeirdPeanoNumber
simplify num = let simplified = firstSimplify num in
               if (isSimple simplified)
               then simplified
               else simplify simplified

eqSimple :: WeirdPeanoNumber -> WeirdPeanoNumber -> Bool
eqSimple Zero Zero = True
eqSimple Zero _ = False
eqSimple _ Zero = False
eqSimple (Succ lhv) (Succ rhv) = eqSimple lhv rhv
eqSimple (Succ lhv) (Prev rhv) = False
eqSimple (Prev lhv) (Succ rhv) = False
eqSimple (Prev lhv) (Prev rhv) = eqSimple lhv rhv

leqSimple :: WeirdPeanoNumber -> WeirdPeanoNumber -> Bool
leqSimple Zero Zero = True
leqSimple Zero (Succ _) = True
leqSimple Zero (Prev _) = False
leqSimple (Succ _) Zero = False
leqSimple (Prev _) Zero = True
leqSimple (Succ lhv) (Succ rhv) = leqSimple lhv rhv
leqSimple (Succ lhv) (Prev rhv) = False
leqSimple (Prev lhv) (Succ rhv) = True
leqSimple (Prev lhv) (Prev rhv) = leqSimple lhv rhv

simpleDiv :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
simpleDiv lhv rhv = let dif = lhv - rhv in
                    if (dif >= Zero) 
                    then (simpleDIV dif rhv) + 1
                    else 0


-- классы типов, которым отвечают целые числа
instance Eq WeirdPeanoNumber where
  (==) lhv rhv = eqSimple (simplify lhv) (simplify rhv)

instance Ord WeirdPeanoNumber where
  (<=) lhv rhv = leqSimple (simplify lhv) (simplify rhv)

instance Num WeirdPeanoNumber where
  (+) Zero rhv = rhv
  (+) lhv Zero = lhv
  (+) (Succ lhv) rhv = Succ (lhv + rhv)
  (+) (Prev lhv) rhv = Prev (lhv + rhv)

  negate Zero = Zero
  negate (Succ num) = Prev (negate num)
  negate (Prev num) = Succ (negate num) 

  fromInteger x | x == 0 = Zero
                | x < 0 = Prev (fromInteger (x + 1))
                | otherwise = Succ (fromInteger (x - 1))

  signum Zero = Zero
  signum (Succ (Prev num)) = signum num
  signum (Prev (Succ num)) = signum num
  signum (Succ num) = Succ Zero
  signum (Prev num) = Prev Zero

  abs num = if (signum num < Zero) 
            then negate num 
            else num

  (*) Zero _ = Zero
  (*) _ Zero = Zero
  (*) (Succ lhv) rhv = rhv + (lhv * rhv)
  (*) (Prev lhv) rhv = if (signum lhv == signum rhv) 
                         then  (rhv + (lhv * rhv))
                       else if (signum lhv < Zero) 
                         then negate(rhv + ((negate lhv) * rhv))
                       else let nrhv = negate rhv in negate (nrhv + (lhv * nrhv))

instance Enum WeirdPeanoNumber where
  toEnum num | num == 0 = Zero
             | num < 0 = Prev (toEnum $ num + 1)
             | otherwise = Succ (toEnum $ num - 1) 
  fromEnum Zero = 0
  fromEnum (Succ lhv) = (fromEnum lhv) + 1
  fromEnum (Prev lhv) = (fromEnum lhv) - 1

instance Real WeirdPeanoNumber where
   toRational num = toRational (toInteger num)

instance Integral WeirdPeanoNumber where
  quotRem lhv rhv = let isNeg = (signum lhv) == (signum rhv) in
                    let div = simpleDIV (abs lhv) (abs rhv) in
                    if (isNeg)
                    then (div, simplify $ lhv - div * rhv) 
                    else (negate div, simplify $ lhv - div * rhv)

  toInteger Zero = 0
  toInteger (Succ lhv) = (toInteger lhv) + 1
  toInteger (Prev lhv) = (toInteger lhv) - 1
