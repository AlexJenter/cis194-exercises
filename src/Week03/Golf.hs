module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  )
where

import           Data.List

skips :: [a] -> [[a]]
skips xs = [ everyNth n xs | n <- [1 .. length xs] ]
 where
  everyNth :: Int -> [a] -> [a]
  everyNth = everyNth' 1

  everyNth' :: Int -> Int -> [a] -> [a]
  everyNth' _ _ [] = []
  everyNth' i n' (z : zs) | i == n'   = z : everyNth' 1 n' zs
                          | otherwise = everyNth' (i + 1) n' zs

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map second $ filter isPeak (zip3 xs xs' xs'')
 where
  xs'  = tail xs
  xs'' = tail xs'
  isPeak (x, y, z) = x < y && y > z
  second (_, x, _) = x


histogram :: [Integer] -> String
histogram xs =
  unlines (reverse . transpose $ map fn [0 .. 9]) ++ "==========\n0123456789\n"
 where
  bins    = group $ sort xs
  lengths = map length bins
  record  = maximum lengths

  fn :: Integer -> String
  fn i = case i `elemIndex` map head bins of
    Nothing -> replicate record ' '
    Just n -> replicate lenStar '*' ++ replicate lenSpace ' '
      where
        lenStar = lengths !! n
        lenSpace = record - lenStar



