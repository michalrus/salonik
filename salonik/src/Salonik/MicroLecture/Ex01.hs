module Ex01 where

f :: Maybe Integer
f = Just 5 >>= (\x -> Just (x * 3)) >>= (\y -> Just (y - 3))

f' :: Maybe Integer
f' = do
  x <- Just 5
  y <- Just (x * 3)
  Just (y - 3)

yyy :: Maybe Integer -> Integer
yyy (Just a) = a
yyy Nothing = 42

-----------------
whosthere :: IO String
whosthere = readFile "/etc/passwd"

abomination :: IO String -> String
abomination _ = "hai"

xxx :: String -> [String]
xxx = lines

main :: IO ()
main = do
  wt :: String <- whosthere
  print (xxx wt)
  return wt
