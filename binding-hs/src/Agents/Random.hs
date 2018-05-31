-------------------------------------------------------------------------------
-- |
-- Module    :  Agents.Random
-- License   :  MIT
-- Stability :  experimental
-- Portability: non-portable
--
-- agent that acts randomly
-------------------------------------------------------------------------------
module Agents.Random (randomAgent) where

import OpenAI.Gym (Action(..), Environment(..), GymEnv(..), InstID(..), Outcome(..), Step(..), envReset, envStep, envActionSpaceInfo, envActionSpaceSample, envActionSpaceContains, envObservationSpaceInfo)
import Servant.Client (ClientM)
import Control.Monad (replicateM_, when)

-- | an agent that acts randomly using the HTTP API's `envActionSpaceSample`
randomAgent :: InstID -> ClientM ()
randomAgent inst = do
  envReset inst -- first close monitor
  go 0 False
  where
    maxSteps = 200
    reward = 0
    go :: Int -> Bool -> ClientM ()
    go x done = do
      Action a <- envActionSpaceSample inst
      Outcome ob reward done _ <- envStep inst (Step a True)
      when (not done && x < maxSteps) $ go (x + 1) done
