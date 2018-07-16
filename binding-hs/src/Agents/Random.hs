{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson.Types       (Value (..))
import           Data.HashMap.Strict    (fromList)
import           Data.Map.Strict        (elems, keys)
import           Data.Scientific        (fromFloatDigits, scientific)
import           Data.Text              (pack)
import           Data.Vector            (fromList)
import           OpenAI.Gym             (Action (..), ActionSpace (..),
                                         Agent (..), EnvSpec (..), Info (..),
                                         InstID, Observation (..),
                                         ObservationSpace (..), Space (..),
                                         SpaceInfo (..), isFraction, spaceDType, spaceShape)
                                        --  , spaceLoHi
import           System.Random          (getStdGen, randomR, randomRIO,
                                         randomRs, randoms)
-- import           TensorFlow.GenOps.Core (randomUniform, randomUniformInt)

-- | an agent that acts randomly: $a_{random} \in A$
data RandomAgent spec actionSpace obsSpace state = RandomAgent EnvSpec ActionSpace ObservationSpace ()
instance Agent (RandomAgent spec actionSpace obsSpace state) () where

  -- fake a static method
  initState agent _ = ()

  -- -- act ∷ MonadIO m ⇒ agent → Observation → Int → InstID → m Action
  -- act (RandomAgent spec (ActionSpace (SpaceInfo (TupleSpace spaces))) obsSpace) obs t inst = do
  --   acts <- traverse f spaces
  --   return $ Action $ toArr $ getAction <$> acts
  --   where f spc = act (RandomAgent spec (ActionSpace (SpaceInfo spc)) obsSpace) obs t inst

  -- -- act ∷ MonadIO m ⇒ agent → Observation → Int → InstID → m Action
  -- act (RandomAgent spec (ActionSpace (SpaceInfo (DictSpace dict))) obsSpace) obs t inst = do
  --   acts <- traverse f spaces
  --   return $ Action $ Object $ Data.HashMap.Strict.fromList $ zip ks $ getAction <$> acts
  --   where f spc = act (RandomAgent spec (ActionSpace (SpaceInfo spc)) obsSpace) obs t inst
  --         ks = pack <$> keys dict
  --         spaces = elems dict

  -- act ∷ MonadIO m ⇒ agent → Observation → Int → InstID → m Action
  act (RandomAgent spec actionSpace obsSpace state') state obs t inst = do

    -- factor this out?
    rng <- liftIO getStdGen
    ac <- case aSpace of
      -- shape []
      -- Int
      Discrete n -> do
        -- gym.spaces.np_random.randint(self.n)
        let nums :: [Int] = randomRs (0, n - 1) rng
        return $ toNum $ head nums
      -- Bool
      MultiBinary n -> do
        -- gym.spaces.np_random.randint(low=0, high=2, size=self.n).astype(self.dtype)
        let nums :: [Int] = randomRs (0, 1) rng
        return $ toArr $ toNum <$> take n nums
      MultiDiscrete nvec -> do
        -- (gym.spaces.np_random.rand(self.nvec.size) * self.nvec).astype(self.dtype)
        let nums :: [Double] = randomRs (0, 1) rng -- randoms rng
        let mults :: [Double] = zipWith (*) (fromIntegral <$> nvec) $ take (length nvec) nums
        return $ toArr (toNum . floor <$> mults)
      -- Box shape low high dtype -> do
      --   -- gym.spaces.np_random.uniform(low=self.low, high=self.high + (0 if self.dtype.kind == 'f' else 1), size=self.low.shape).astype(self.dtype)
      --   -- let nums :: [Int] = randomRs (low, high) rng
      --   -- let nums :: [Double] = randomRs (low, high) rng
      --   case isFraction dtype of
      --     False -> do
      --       randomUniformInt shape low high
      --     True -> do
      --       randomUniform shape low high
      --   -- return $ toArr $ Number <$> fromFloatDigits <$> take n nums
      TupleSpace spaces -> do
        -- tuple([space.sample() for space in self.spaces])
        acts <- traverse f spaces
        return $ toArr $ getAction <$> acts
        where f spc = act (RandomAgent spec (ActionSpace (SpaceInfo spc)) obsSpace state) state obs t inst
      DictSpace dict -> do
        -- OrderedDict([(k, space.sample()) for k, space in self.spaces.items()])
        acts <- traverse f spaces
        return $ Object $ Data.HashMap.Strict.fromList $ zip ks $ getAction <$> acts
        where f spc = act (RandomAgent spec (ActionSpace (SpaceInfo spc)) obsSpace state) state obs t inst
              ks = pack <$> keys dict
              spaces = elems dict

    -- ac <- case shape of
    --         -- Discrete
    --         [] -> do
    --           i <- liftIO $ randomRIO (lo, hi - 1)
    --           return $ toNum i
    --         -- MultiBinary
    --         [n] -> do
    --           -- factor this out!
    --           rng <- liftIO $ getStdGen
    --            -- annotation: nasty hack
    --           let nums :: [Double] = randoms rng
    --           -- nums <- randoms <$> liftIO $ getStdGen
    --           -- nums <- randomRs (lo, hi - 1) rng
    --           return $ toArr $ Number <$> fromFloatDigits <$> take n nums
    --         -- MultiDiscrete
    --         -- Box
    --         -- list -> do

    -- liftIO $ print [d| shape |]
    -- liftIO $ print [d| aSpace |]
    -- -- liftIO $ print [d| shape aSpace |]
    -- -- liftIO $ print [d|oSpace|]
    -- liftIO $ print [d|ob|]
    -- liftIO $ print [d|ac|]
    return $ Action ac

    where aSpace = getSpaceInfo $ unActionSpace actionSpace
          -- oSpace = getSpaceInfo $ unObservationSpace obsSpace
          ob = getObservation obs
          shape = spaceShape aSpace
          -- (lo, hi) = spaceLoHi aSpace
          -- toScientific = flip scientific 0
          toNum = Number . flip scientific 0 . toInteger
          toArr = Array . Data.Vector.fromList

-- , reward_threshold    :: Double
-- , nondeterministic    :: Bool
