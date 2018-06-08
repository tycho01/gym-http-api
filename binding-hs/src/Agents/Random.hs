{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

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
import           Data.Aeson.Types       (Value (..))
import           Data.HashMap.Strict    (fromList)
import           Data.Map.Strict        (elems, keys)
import           Data.Scientific        (fromFloatDigits, scientific)
import           Data.Text              (pack)
import           Data.Vector            (fromList)
import           Debug.Dump             (d)
import           OpenAI.Gym             (Action (..), ActionSpace (..),
                                         Agent (..), EnvSpec (..), Info (..),
                                         InstID, Observation (..),
                                         ObservationSpace (..), Space (..),
                                         SpaceInfo (..), envActionSpaceSample,
                                         spaceDType, spaceLoHi, spaceShape)
import           Servant.Client         (ClientM)
import           System.Random          (getStdGen, randomRIO, randoms)

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
    ac <- case shape of
            -- Discrete
            [] -> do
              i <- liftIO $ randomRIO (lo, hi - 1)
              return $ Number $ toScientific $ toInteger i
            -- MultiBinary
            [n] -> do
              rng <- liftIO $ getStdGen
              let nums :: [Double] = randoms rng
              return $ Array $ Data.Vector.fromList $ Number <$> fromFloatDigits <$> take n nums
    return $ Action ac

    where aSpace = getSpaceInfo $ unActionSpace actionSpace
          ob = getObservation obs
          shape = spaceShape aSpace
          toScientific = flip scientific 0
