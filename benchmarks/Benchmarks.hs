import Sort (insertionSort, mergeSort, bubbleSort, quickSort, heapSort', heapSort)
import Criterion.Main (defaultMainWith, defaultConfig, bgroup, bench, nf, Benchmark, env)
import Criterion.Types (reportFile)
import Test.QuickCheck (generate, vectorOf, arbitrary, Gen)
import qualified Data.Vector as V

defaultBenchList = [0, 3, 10, 50, 100, 1000, 10000, 100000, 1000000]
minBenchList = take (length defaultBenchList - 2) defaultBenchList

arbitraryList :: Int -> IO [Int]
arbitraryList n = generate $ vectorOf n (arbitrary :: Gen Int)

makeOneBench :: ([Int] -> [Int]) -> Int -> Benchmark
makeOneBench sortFn listSize = env
  (arbitraryList listSize)
  (bench (show listSize) . nf sortFn)

makeOneVectorBench :: (V.Vector Int -> V.Vector Int) -> Int -> Benchmark
makeOneVectorBench sortFn listSize = env
  (arbitraryList listSize)
  (bench (show listSize) . nf sortFn . V.fromList)

benchCases :: ([Int] -> [Int]) -> String -> [Int] -> Benchmark
benchCases sortFn benchName benchList = bgroup benchName $ map (makeOneBench sortFn) benchList

main :: IO ()
main = defaultMainWith defaultConfig [
    -- benchCases insertionSort "insertion sort" defaultBenchList
    -- , benchCases mergeSort "merge sort" defaultBenchList
    -- , benchCases bubbleSort "bubble sort" minBenchList
    -- benchCases quickSort "quick sort" defaultBenchList
    benchCases heapSort "heap sort" defaultBenchList
  ]
