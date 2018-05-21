{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude
import Control.Monad (replicateM_)
import Control.Monad.Catch ()
import Control.Exception.Base (finally)
import OpenAI.Gym (Action(..), Config(..), Environment(..), GymEnv(..), Monitor(..), InstID(..), Outcome(..), Step(..), envCreate, envListAll, envReset, envStep, envActionSpaceInfo, envActionSpaceSample, envActionSpaceContains, envObservationSpaceInfo, envMonitorStart, envMonitorClose, envClose, upload, shutdownServer)
import Cli (CliArgs(..), getArgs)
import Agents.Random (randomAgent)
import Servant.Client (BaseUrl(..), ClientEnv(..), ClientM, Scheme(Http), runClientM)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Control.Monad.Trans (liftIO)

-- | main function, run `example` given CLI args + env vars
main :: IO ()
main = do
  CliArgs{game, state, scenario, record, verbose, quiet, ram, doRender, agent} <- getArgs
  apiKey <- T.pack <$> fromMaybe "" <$> lookupEnv "OPENAI_GYM_API_KEY"
  manager <- newManager defaultManagerSettings
  out <- runClientM (example apiKey) $ ClientEnv manager url
  case out of
    Left err -> print err
    Right ok -> print ok

  where
    url :: BaseUrl
    url = BaseUrl Http "localhost" 5000 ""

-- | get envs, reuse existing `CartPolev0` env, run `randomAgent` x100, upload score
example :: T.Text -> ClientM ()
example apiKey = do
  let game = CartPoleV0
  liftIO $ print game
  envs <- all_envs <$> envListAll
  liftIO $ print envs
  let gameIds = Map.filter (== (T.pack $ show game)) envs
  liftIO $ print gameIds
  let maybeId = listToMaybe $ map InstID $ Map.keys $ gameIds
  inst <- case maybeId of
          -- reuse existing env
          Just instId -> return instId
          Nothing -> envCreate game
  liftIO $ print inst
  let agent = replicateM_ episodeCount $ randomAgent inst
  case maybeId of
    Just instId -> agent
    Nothing -> do
      Monitor{directory} <- withMonitor inst agent
      -- Upload to the scoreboard.
      upload (Config directory "algo" apiKey)

  where
    episodeCount :: Int
    episodeCount = 100

-- | run agent within monitor, ctrl-C proof
withMonitor :: InstID -> ClientM () -> ClientM Monitor
withMonitor inst agent = do
  envMonitorStart inst configs
  -- agent `finally` envMonitorClose inst
  agent
  envMonitorClose inst
  return configs
  where
    configs :: Monitor
    configs = Monitor "/tmp/random-agent-results" True False False



