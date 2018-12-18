import Sort (insertionSort)
import Criterion.Main (defaultMainWith, defaultConfig, bgroup, bench, nf, Benchmark, env)
import Criterion.Types (reportFile)
import Test.QuickCheck (generate, vectorOf, arbitrary, Gen)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

benchList = [0..5] ++ (drop 1 $ map (* 3) $ take 10 fibs) ++ (drop 15 $ map (* 8) $ take 10 fibs)

benchCases :: ([Int] -> [Int]) -> String -> Benchmark
benchCases sortFn benchName = bgroup benchName $ map makeOneBench benchList
  where
    makeOneBench :: Int -> Benchmark
    makeOneBench listSize = env
      (arbitraryList listSize)
      (\ ~xs -> bench (show listSize) $ nf sortFn xs)
    arbitraryList :: Int -> IO [Int]
    arbitraryList n = generate $ vectorOf n (arbitrary :: Gen Int)

main :: IO ()
main = defaultMainWith
  (defaultConfig { reportFile = Just "benchmarks/sort-bench-nf.html" })
  [
    benchCases insertionSort "insertion sort"
  ]
