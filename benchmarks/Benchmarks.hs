import Sort (insertionSort, mergeSort, bubbleSort, heapSort, quickSort)
import Criterion.Main (defaultMainWith, defaultConfig, bgroup, bench, nf, Benchmark, env)
import Criterion.Types (reportFile)
import Test.QuickCheck (generate, vectorOf, arbitrary, Gen)

defaultBenchList = [0, 3, 10, 50, 100, 1000, 10000, 100000, 1000000]
minBenchList = take (length defaultBenchList - 2) defaultBenchList

benchCases :: ([Int] -> [Int]) -> String -> [Int] -> Benchmark
benchCases sortFn benchName benchList = bgroup benchName $ map makeOneBench benchList
  where
    makeOneBench :: Int -> Benchmark
    makeOneBench listSize = env
      (arbitraryList listSize)
      (\xs -> bench (show listSize) $ nf sortFn xs)
    arbitraryList :: Int -> IO [Int]
    arbitraryList n = generate $ vectorOf n (arbitrary :: Gen Int)

main :: IO ()
main = defaultMainWith defaultConfig [
    benchCases insertionSort "insertion sort" defaultBenchList
    -- , benchCases mergeSort "merge sort" defaultBenchList
    -- , benchCases bubbleSort "bubble sort" minBenchList
    -- , benchCases quickSort "quick sort" defaultBenchList
    -- benchCases quickSort "quick sort" [10000]
    -- , benchCases heapSort "heap sort" [10000]
  ]
