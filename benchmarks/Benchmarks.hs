import Sort (insertionSort)
import Criterion.Main (defaultMainWith, defaultConfig, bgroup, bench, nf, Benchmark, env)
import Criterion.Types (reportFile)
import Test.QuickCheck (generate, vectorOf, arbitrary, Gen)

benchList = [0, 3, 50, 1000, 10000, 1000000]

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
