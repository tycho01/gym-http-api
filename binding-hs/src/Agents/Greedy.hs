{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

-------------------------------------------------------------------------------
-- |
-- Module    :  Agents.Greedy
-- License   :  MIT
-- Stability :  experimental
-- Portability: non-portable
--
-- agent that acts greedily
-------------------------------------------------------------------------------
module Agents.Greedy (GreedyAgent (..)) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Types       (Value (..))
import Data.HashMap.Strict    (fromList)
import Data.Map.Strict        (Map, elems, keys, member, insert, adjust)
import Data.Scientific        (fromFloatDigits, scientific)
import Data.Text              (pack)
import Data.Vector            (fromList)
import OpenAI.Gym             (Action (..), ActionSpace (..),
                                Agent (..), EnvSpec (..), Info (..),
                                InstID, Observation (..),
                                ObservationSpace (..), Space (..),
                                SpaceInfo (..), isFraction, spaceDType, spaceShape)
import System.Random           (getStdGen, randomR, randomRIO,
                                         randomRs, randoms)
import Control.Monad.State.Lazy (MonadState (..), modify)

-- | an agent that acts greedily
data GreedyAgent spec actionSpace obsSpace = GreedyAgent EnvSpec ActionSpace ObservationSpace
-- instance Agent (GreedyAgent spec actionSpace obsSpace) (Map Action Int) where

--   learn (GreedyAgent spec actionSpace obsSpace) ob ac reward ob_ done t info = do
--     modify state $ \map -> case member ac map of
--       False -> insert ac 1 map
--       True -> adjust (+1) ac map
--     return ()

--   act (GreedyAgent spec actionSpace obsSpace) obs t inst = do

--     rng <- liftIO getStdGen
--     ac <- case aSpace of
--       Discrete n -> do
--         let nums :: [Int] = randomRs (0, n - 1) rng
--         return $ toNum $ head nums
--       MultiBinary n -> do
--         let nums :: [Int] = randomRs (0, 1) rng
--         return $ toArr $ toNum <$> take n nums
--       MultiDiscrete nvec -> do
--         let nums :: [Double] = randomRs (0, 1) rng -- randoms rng
--         let mults :: [Double] = zipWith (*) (fromIntegral <$> nvec) $ take (length nvec) nums
--         return $ toArr (toNum . floor <$> mults)
--       TupleSpace spaces -> do
--         acts <- traverse f spaces
--         return $ toArr $ getAction <$> acts
--         where f spc = act (GreedyAgent spec (ActionSpace (SpaceInfo spc)) obsSpace) obs t inst
--       DictSpace dict -> do
--         acts <- traverse f spaces
--         return $ Object $ Data.HashMap.Strict.fromList $ zip ks $ getAction <$> acts
--         where f spc = act (GreedyAgent spec (ActionSpace (SpaceInfo spc)) obsSpace) obs t inst
--               ks = pack <$> keys dict
--               spaces = elems dict
--     return $ Action ac

--     where aSpace = getSpaceInfo $ unActionSpace actionSpace
--           ob = getObservation obs
--           shape = spaceShape aSpace
--           toNum = Number . flip scientific 0 . toInteger
--           toArr = Array . Data.Vector.fromList

-- -- , reward_threshold    :: Double
-- -- , nondeterministic    :: Bool
