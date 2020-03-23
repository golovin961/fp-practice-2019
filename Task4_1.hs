module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`
instance Functor FunMonad where
    fmap f (FunMonad a) = FunMonad (f . a)

instance Applicative FunMonad where
    pure a = FunMonad (\str -> a)
    (<*>) (FunMonad f) (FunMonad a) = FunMonad (\str -> (f str) (a str))

instance Monad FunMonad where
    return a = FunMonad(\str -> a)
    (>>=) (FunMonad a) f = FunMonad (\str -> fun (f (a str)) str)
