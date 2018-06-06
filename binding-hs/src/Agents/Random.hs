{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- |
-- Module    :  Agents.Random
-- License   :  MIT
-- Stability :  experimental
-- Portability: non-portable
--
-- agent that acts randomly
-------------------------------------------------------------------------------
module Agents.Random (RandomAgent (..)) where

import           OpenAI.Gym (Action (..), ActionSpace, Agent (..), EnvSpec (..),
                             Info (..), ObservationSpace, envActionSpaceSample)

-- | an agent that acts randomly using the HTTP API's `envActionSpaceSample`: $a_{random} \in A$
data RandomAgent spec actionSpace obsSpace = RandomAgent EnvSpec ActionSpace ObservationSpace
instance Agent (RandomAgent spec actionSpace obsSpace) where
  act (RandomAgent spec actionSpace obsSpace) ob t inst = envActionSpaceSample inst
