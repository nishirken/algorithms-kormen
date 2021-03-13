import Sort (insertionSort, mergeSort, bubbleSort, quickSort, heapSort, optimalSort)
import Criterion.Main (defaultMainWith, defaultConfig, bgroup, bench, nf, Benchmark, env)
import Criterion.Types (reportFile)
import Test.QuickCheck (generate, vectorOf, arbitrary, Gen)
import qualified Data.Vector as V

defaultBenchList = [0, 1, 3, 4, 5, 10, 50, 100, 1000, 10000, 100000, 1000000]
minBenchList = take (length defaultBenchList - 2) defaultBenchList

genList :: Int -> IO [Int]
genList len = generate $ vectorOf len (arbitrary :: Gen Int)

generateLists :: IO [[Int]]
generateLists = traverse genList minBenchList

cases :: (Show a, Ord a) => [(String, [a] -> [a])]
cases =
  [
    ("merge sort", mergeSort)
    , ("heap sort", heapSort)
    , ("quick sort", quickSort)
    , ("optimal sort", optimalSort)
  ]

main :: IO ()
main = do
  lists <- generateLists
  defaultMainWith
    defaultConfig { reportFile = Just "benchmarks/sort.html" } $
    do
      testList <- lists
      (name, f) <- cases
      pure $ bench (name ++ "/" ++ (show . length) testList) $ nf f testList
