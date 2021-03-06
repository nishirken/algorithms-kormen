module Prisoners where

import qualified Control.Monad.State as State
import Numeric.Natural (Natural)
import Control.Monad (mapM)
import qualified System.Random as Random
import qualified Data.Map.Strict as Map

data Light = On | Off

type RoomVisits = Int
type PrisonerVisits = Int
type Prisoner = Int
type RoomState = State.State (Light, RoomVisits) (Light, RoomVisits)

countingPrisoner = 0

--visitRoom :: (Prisoner, PrisonerVisits) -> RoomState -> RoomState
--visitRoom (prisoner, visits) state =
--  if prisoner == countingPrisoner then forCounting else forOthers
--    where
--      forCounting = do
--        (light, visits') <- State.get
--        case light of
--          On -> State.put (Off, visits' + 1)
--          Off -> state
--      forOthers = do
--        (light, visits') <- State.get
--        case light of
--          On -> state
--          Off -> if visits >= 1 then state else State.put (On, visits')
--
--genVisits :: Int -> IO (Map.Map Prisoner PrisonerVisits)
--genVisits prisonersCount = do
--  generated <- mapM (Random.getStdRandom (Random.randomR (1, 10))) [0..prisonersCount - 1]
--  pure $ Map.fromList $ zip [0..prisonersCount - 1] generated
--
--chooseOne :: Map.Map Prisoner PrisonerVisits -> IO (Map.Map Prisoner PrisonerVisits, Prisoner)
--chooseOne visits = do
--  prisoner <- Random.getStdRandom (Random.randomR (0, (length visits) - 1))
--  let v = visits !! prisoner
--  pure (visits, prisoner)
--
--answer :: Int -> IO [PrisonerVisits]
--answer prisonersCount = do
--  randomVisits <- genVisits prisonersCount
--  let
--    prisoners = [0..prisonersCount - 1]
--    visits = zip prisoners $ Map.toList randomVisits
--    f :: [(Prisoner, PrisonerVisits)] -> IO [PrisonerVisits]
--    f [] = pure []
--    f xs = mapM
--      (\(prisoner, prisonerVisits) -> visitRoom (prisoner, prisonerVisits) (pure (Off, 0) :: RoomState))
--      visits
--  pure [] -- TODO
