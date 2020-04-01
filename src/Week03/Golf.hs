module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  )
where

import           Data.List

skips :: [a] -> [[a]]
skips xs = [ [ xs !! (x - 1) | x <- range, mod x y == 0 ] | y <- range ]
  where range = [1 .. length xs]


localMaxima :: [Integer] -> [Integer]
localMaxima xs = map second $ filter isPeak (zip3 xs xs' xs'')
 where
  xs'  = tail xs
  xs'' = tail xs'
  isPeak (x, y, z) = x < y && y > z
  second (_, x, _) = x


histogram :: [Integer] -> String
histogram xs = unlines (reverse . transpose $ map f [0 .. 9]) ++ chrome
 where
  bins   = group $ sort xs
  lens   = map length bins
  reco   = maximum lens
  chrome = "==========\n0123456789\n"

  f :: Integer -> String
  f i = case i `elemIndex` map head bins of
    Just n -> replicate len '*' ++ replicate (reco - len) ' '
      where len = lens !! n
    Nothing -> replicate reco ' '
