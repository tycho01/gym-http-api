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

import           Control.Monad.IO.Class (liftIO)
import Debug.Dump (d)
import System.Random (randomRIO)
import           Data.Aeson.Types       (Value (..))
import           Data.Scientific        (scientific)
import           OpenAI.Gym             (Action (..), ActionSpace (..),
                                         Agent (..), EnvSpec (..), Info (..),
                                         ObservationSpace (..), Observation (..), Space (..),
                                         SpaceInfo (..), envActionSpaceSample, spaceShape, spaceDType, spaceLoHi)

-- | an agent that acts randomly using the HTTP API's `envActionSpaceSample`: $a_{random} \in A$
data RandomAgent spec actionSpace obsSpace = RandomAgent EnvSpec ActionSpace ObservationSpace
instance Agent (RandomAgent spec actionSpace obsSpace) where
  act (RandomAgent spec actionSpace obsSpace) obs t inst = do
    let aSpace = getSpaceInfo $ unActionSpace actionSpace
    let shape = spaceShape aSpace
    let (lo, hi) = spaceLoHi aSpace
    ac <- case shape of
            [] -> do
              i <- liftIO $ randomRIO (lo, hi - 1)
              return $ Number $ scientific (toInteger i) 0
    return $ Action ac
