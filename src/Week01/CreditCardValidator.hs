module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  ) where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0 = lastDigit : toDigitsRev initialDigits
  | otherwise = []
  where (initialDigits, lastDigit) = divMod x 10

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]      = [x]
doubleEveryOther (x:y:ys) = x' : y' : doubleEveryOther ys
  where
    z = toInteger $ fromEnum $ even $ length ys
    x' = x * (+) 1 z
    y' = y * (-) 2 z

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits [x]    = x
sumDigits (x:xs) = sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate = (==0) . flip mod 10 . sumDigits . doubleEveryOther . toDigits
