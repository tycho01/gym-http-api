{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (log)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (lookupEnv)
import System.Log.Logger (Priority(..), logM, rootLoggerName, setLevel, updateGlobalLogger)
import Servant.Client (BaseUrl(..), ClientEnv(..), ClientM, Scheme(Http), runClientM)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Control.Monad (replicateM_)
import Control.Monad.Catch ()
import Control.Monad.IO.Class (MonadIO)
import Control.Exception.Lifted (finally)
import Control.Monad.Trans (liftIO)

import OpenAI.Gym (Action(..), Config(..), Environment(..), GymEnv(..), Monitor(..), InstID(..), Outcome(..), Step(..), envCreate, envListAll, envReset, envStep, envActionSpaceInfo, envActionSpaceSample, envActionSpaceContains, envObservationSpaceInfo, envMonitorStart, envMonitorClose, envClose, upload, shutdownServer)
import Cli (CliArgs(..), getArgs)
import Agents.Random (randomAgent)


loggerName = rootLoggerName -- "Gym Agent"
-- | logging function
log :: (MonadIO m, Show a) => Priority -> a -> m ()
log lvl = liftIO . (logM loggerName lvl) . show

-- | main function, run `example` given CLI args + env vars
main :: IO ()
main = do
  CliArgs{game, state, scenario, record, verbose, quiet, ram, agent} <- getArgs
  let logLvl = if verbose then DEBUG else if quiet then WARNING else INFO
  updateGlobalLogger loggerName $ setLevel logLvl
  apiKey <- T.pack <$> fromMaybe "" <$> lookupEnv "OPENAI_GYM_API_KEY"
  manager <- newManager defaultManagerSettings
  out <- runClientM (example apiKey) $ ClientEnv manager url Nothing
  case out of
    Left err -> log ERROR err
    Right _ -> return ()

  where
    url :: BaseUrl
    url = BaseUrl Http "localhost" 5000 ""

-- | get envs, reuse existing `CartPolev0` env, run `randomAgent` x100, upload score
example :: T.Text -> ClientM ()
example apiKey = do
  let game = CartPoleV0
  log INFO game
  envs <- all_envs <$> envListAll
  log INFO envs
  let gameIds = Map.filter (== (T.pack $ show game)) envs
  log INFO gameIds
  let maybeId = listToMaybe $ map InstID $ Map.keys $ gameIds
  inst <- case maybeId of
          -- reuse existing env
          Just instId -> return instId
          Nothing -> envCreate game
  log INFO inst
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
  agent `finally` envMonitorClose inst -- clean even on ctrl-C
  return configs
  where
    configs :: Monitor
    configs = Monitor "/tmp/random-agent-results" True False False



