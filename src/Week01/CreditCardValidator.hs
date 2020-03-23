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
doubleEveryOther []  = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:ys) = x * fx : y * fy : doubleEveryOther ys
  where (fx, fy) = if (even $ length ys) then (2,1) else (1,2)

sumDigits :: [Integer] -> Integer
sumDigits = error "Week01.CreditCardValidator#sumDigits not implemented"

validate :: Integer -> Bool
validate = error "Week01.CreditCardValidator#validate not implemented"
