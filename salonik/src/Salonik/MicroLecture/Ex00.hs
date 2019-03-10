module Ex00 where

{-
data Maybe' a = Just' a | Nothing' deriving Show

class Monad' m where
  pure' :: a -> m a
  -- map' :: (a -> b) -> m a -> m b
  bind' :: (a -> m b) -> m a -> m b

instance Monad' Maybe' where
  pure' x = Just' x

  -- map' f (Just' x) = Just' (f x)
  -- map' f Nothing' = Nothing'

  bind' f Nothing' = Nothing'
  bind' f (Just' x) = f x
-}
g :: Int -> Int -> Int
g x y = x * y

-- x :: Integer
f a =
  if a < 0
    then Nothing'
    else Just' 5

-- abstract class Maybe<A>
-- final class Just(A a) extends Maybe<A>
-- final class Nothing extends Maybe<?>
main :: IO ()
main = print "Hello"
