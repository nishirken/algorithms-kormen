module STVectorShow where

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import qualified Control.Monad.ST as ST
import Control.Monad (forM)

showMVector :: Show a => (MV.MVector s a) -> ST.ST s String
showMVector xs = show <$> forM [0..(MV.length xs - 1)] (\i -> show <$> MV.read xs i)
