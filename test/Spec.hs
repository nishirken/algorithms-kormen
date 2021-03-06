import Test.Hspec
import SortSpec
import DataStructuresSpec
import StringSearchSpec
--import PrisonersSpec

main :: IO ()
main = hspec $ do
  sortSpec
  dataStructuresSpec
  stringSearchSpec
--  prisonersSpec
