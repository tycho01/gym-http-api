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

import           OpenAI.Gym (Action (..), Agent (..), Info (..),
                             envActionSpaceSample)

-- | an agent that acts randomly using the HTTP API's `envActionSpaceSample`: $a_{random} \in A$
-- data RandomAgent = RandomAgent
-- data RandomAgent Info Info = RandomAgent actionSpace obsSpace
data RandomAgent actionSpace obsSpace = RandomAgent Info Info -- where
-- data D ab where
--   D :: (a -> b) -> D '(a, b)
instance Agent (RandomAgent actionSpace obsSpace) where
  act (RandomAgent actionSpace obsSpace) ob t inst = envActionSpaceSample inst
