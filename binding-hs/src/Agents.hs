{-# LANGUAGE UnicodeSyntax #-}
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
  , AnyAgentType
  , AnyAgent
  , AgentCtor
  , agents
  ) where

import           Agents.Random   (RandomAgent (..))
import qualified Data.Map.Strict as Map
import           OpenAI.Gym      (ActionSpace, Agent, EnvSpec, ObservationSpace)

type AnyAgentType = RandomAgent
type AnyAgent a = AnyAgentType EnvSpec ActionSpace ObservationSpace a
type AgentCtor a = EnvSpec → ActionSpace → ObservationSpace → a → AnyAgent a

-- | a map of string identifiers to agents
agents ∷ Map.Map String (AgentCtor ())
agents = Map.fromList [
    ("random", RandomAgent)
  ]
