module Agents.Random (randomAgent) where

import OpenAI.Gym (Action(..), Environment(..), GymEnv(..), InstID(..), Outcome(..), Step(..), envReset, envStep, envActionSpaceInfo, envActionSpaceSample, envActionSpaceContains, envObservationSpaceInfo)
import Servant.Client (ClientM)
import Control.Monad (replicateM_, when)

randomAgent :: InstID -> ClientM ()
randomAgent inst = do
  envReset inst -- first close monitor
  go 0 False
  where
    maxSteps :: Int
    maxSteps = 200

    reward :: Int
    reward = 0

    go :: Int -> Bool -> ClientM ()
    go x done = do
      Action a <- envActionSpaceSample inst
      Outcome ob reward done _ <- envStep inst (Step a True)
      when (not done && x < maxSteps) $ go (x + 1) done
