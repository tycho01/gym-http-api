-------------------------------------------------------------------------------
-- |
-- Module    :  Main
-- License   :  MIT
-- Stability :  experimental
-- Portability: non-portable
--
-- Example of how to build an agent using OpenAI.Gym.Client
-------------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude
import Control.Monad (replicateM_, when)
import Control.Monad.Catch()
import Control.Exception.Base()

import OpenAI.Gym (Action(..), Config(..), Environment(..), GymEnv(..), Monitor(..), InstID(..), Outcome(..), Step(..), envCreate, envListAll, envReset, envStep, envActionSpaceInfo, envActionSpaceSample, envActionSpaceContains, envObservationSpaceInfo, envMonitorStart, envMonitorClose, envClose, upload, shutdownServer)
import Servant.Client (BaseUrl(..), ClientEnv(..), ClientM, Scheme(Http), runClientM)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)


main :: IO ()
main = do
  apiKey <- T.pack <$> fromMaybe "" <$> lookupEnv "OPENAI_GYM_API_KEY"
  manager <- newManager defaultManagerSettings
  out <- runClientM (example apiKey) (ClientEnv manager url)
  case out of
    Left err -> print err
    Right ok -> print ok

  where
    url :: BaseUrl
    url = BaseUrl Http "localhost" 5000 ""


example :: T.Text -> ClientM ()
example apiKey = do
  inst <- envCreate CartPoleV0
  Monitor{directory} <- withMonitor inst $
    replicateM_ episodeCount (agent inst)

  -- Upload to the scoreboard.
  -- TODO: Implement environment variable support.
  upload (Config directory "algo" apiKey)

  where
    episodeCount :: Int
    episodeCount = 100


agent :: InstID -> ClientM ()
agent inst = do
  envReset inst
  go 0 False
  where
    maxSteps :: Int
    maxSteps = 200

    reward :: Int
    reward = 0

    go :: Int -> Bool -> ClientM ()
    go x done = do
      Action a <- envActionSpaceSample inst
      Outcome ob reward done _ <- envStep inst (Step a True)
      when (not done && x < maxSteps) $ go (x + 1) done


withMonitor :: InstID -> ClientM () -> ClientM Monitor
withMonitor inst agent = do
  envMonitorStart inst configs
  agent
  envMonitorClose inst
  return configs
  where
    configs :: Monitor
    configs = Monitor "/tmp/random-agent-results" True False False



