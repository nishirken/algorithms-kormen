import Test.Hspec
import SortSpec
import DataStructuresSpec

main :: IO ()
main = hspec $ do
  sortSpec
  dataStructuresSpec
