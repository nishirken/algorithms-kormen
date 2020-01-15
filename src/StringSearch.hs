module StringSearch where

naiveMatch :: String -> String -> Bool
naiveMatch _ [] = True
naiveMatch [] ys = ys == ""
naiveMatch string substring = f (take substringLength string) string (length string)
  where
    substringLength = length substring
    f next rest n | n < substringLength = False
    f next rest n = if next == substring
      then True
      else f (take substringLength (tail rest)) (tail rest) (n - 1)
