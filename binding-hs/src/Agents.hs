{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE ExistentialQuantification #-}

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
  -- , module Agents.Greedy
  , AnyAgentType
  , AnyAgent
  , AgentCtor
  , agents
  ) where

import           Agents.Random   (RandomAgent (..))
-- import           Agents.Greedy   (GreedyAgent (..))
import qualified Data.Map.Strict as Map
import           OpenAI.Gym      (ActionSpace, Agent, EnvSpec, ObservationSpace)
-- import           Data.Dynamic    (Dynamic)

type AnyAgentType = RandomAgent -- | GreedyAgent
-- data AnyAgentType = RandomAgent | GreedyAgent
type AnyAgent a = AnyAgentType EnvSpec ActionSpace ObservationSpace a
type AgentCtor a = EnvSpec → ActionSpace → ObservationSpace → a → AnyAgent a

-- | a map of string identifiers to agents
agents ∷ Map.Map String (AgentCtor ())
-- agents ∷ forall a . Map.Map String (AgentCtor a)
-- agents ∷ exists a. Map.Map String (AgentCtor a)
agents = Map.fromList [
    ("random", RandomAgent)
  -- , ("greedy", GreedyAgent)
  ]
