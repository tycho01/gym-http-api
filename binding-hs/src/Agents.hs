-------------------------------------------------------------------------------
-- |
-- Module    :  Agents
-- License   :  MIT
-- Stability :  experimental
-- Portability: non-portable
--
-- re-exports RL agents for use with OpenAI's Gym
-------------------------------------------------------------------------------
module Agents
  ( module Agents.Random
  , Agent
  , agents
  ) where

import qualified Data.Map.Strict as Map
import OpenAI.Gym (Agent)
import Agents.Random

agents :: Map.Map String Agent
agents = Map.fromList [
    ("random", randomAgent)
  ]
