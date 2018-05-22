{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (log)
import qualified Data.Text as T
import Text.Read (readMaybe)
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

import OpenAI.Gym (Action(..), Config(..), Environment(..), GymEnv(..), Monitor(..), InstID(..), Outcome(..), Step(..), Agent, envCreate, envListAll, envReset, envStep, envActionSpaceInfo, envActionSpaceSample, envActionSpaceContains, envObservationSpaceInfo, envMonitorStart, envMonitorClose, envClose, upload, shutdownServer)
import Cli (CliArgs(..), getArgs)
import Agents (agents)


loggerName = rootLoggerName -- "Gym Agent"
-- | logging function
log :: (MonadIO m, Show a) => Priority -> a -> m ()
log lvl = liftIO . (logM loggerName lvl) . show

defaultGame = CartPoleV0

-- | main function, run `example` given CLI args + env vars
main :: IO ()
main = do
  CliArgs{game, verbose, quiet, agent} <- getArgs
  let logLvl = if verbose then DEBUG else if quiet then WARNING else INFO
  updateGlobalLogger loggerName $ setLevel logLvl

  gymEnv :: GymEnv <- case readMaybe game of
                Just env -> return env
                Nothing -> do
                  log ERROR "unknown game"
                  return defaultGame -- default

  -- apiKey <- T.pack <$> fromMaybe "" <$> lookupEnv "OPENAI_GYM_API_KEY"
  let agentType = agents Map.! agent
  manager <- newManager defaultManagerSettings
  let experiment = example gymEnv agentType :: ClientM ()
  out <- runClientM experiment $ ClientEnv manager url Nothing
  case out of
    Left err -> log ERROR err
    Right _ -> return ()

  where
    url :: BaseUrl
    url = BaseUrl Http "localhost" 5000 ""

-- | get game env, run agent x100, upload score
example :: GymEnv -> Agent -> ClientM ()
example gymEnv agentType = do
  log INFO gymEnv
  envs <- all_envs <$> envListAll
  log INFO envs
  let gameIds = Map.filter (== (T.pack $ show gymEnv)) envs
  log INFO gameIds
  let maybeId = listToMaybe $ map InstID $ Map.keys $ gameIds
  inst <- case maybeId of
          -- reuse existing env
          Just instId -> return instId
          Nothing -> envCreate gymEnv
  log INFO inst
  let agent = replicateM_ episodeCount $ agentType inst
  agent

  where
    episodeCount :: Int
    episodeCount = 100




