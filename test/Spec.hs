import Test.Hspec
import SortSpec
import DataStructuresSpec
import StringSearchSpec

main :: IO ()
main = hspec $ do
  sortSpec
  dataStructuresSpec
  stringSearchSpec
