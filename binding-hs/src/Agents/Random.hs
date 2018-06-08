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

import Data.Vector (fromList)
import Data.Map.Strict (keys, elems)
import Data.HashMap.Strict (fromList)
import           Control.Monad.IO.Class (liftIO)
import Debug.Dump (d)
import Data.Text (pack)
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

  act (RandomAgent spec (ActionSpace (SpaceInfo (TupleSpace spaces))) obsSpace) obs t inst = do
    acts <- traverse f spaces
    return $ Action $ Array $ Data.Vector.fromList $ fmap getAction acts
    where f spc = act (RandomAgent spec (ActionSpace (SpaceInfo spc)) obsSpace) obs t inst

  act (RandomAgent spec (ActionSpace (SpaceInfo (DictSpace dict))) obsSpace) obs t inst = do
    acts <- traverse f spaces
    return $ Action $ Object $ Data.HashMap.Strict.fromList $ zip ks $ fmap getAction acts
    where f spc = act (RandomAgent spec (ActionSpace (SpaceInfo spc)) obsSpace) obs t inst
          ks = pack <$> keys dict
          spaces = elems dict

  act (RandomAgent spec actionSpace obsSpace) obs t inst = do
    let aSpace = getSpaceInfo $ unActionSpace actionSpace
    let shape = spaceShape aSpace
    let (lo, hi) = spaceLoHi aSpace
    ac <- case shape of
            [] -> do
              i <- liftIO $ randomRIO (lo, hi - 1)
              return $ Number $ scientific (toInteger i) 0
    return $ Action ac
