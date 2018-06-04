{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
module Main where

import           Control.Exception.Lifted (finally)
import           Control.Monad            (replicateM_, when)
import           Control.Monad.Catch      ()
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe, listToMaybe)
import qualified Data.Text                as T
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Servant.Client           (BaseUrl (..), ClientEnv (..),
                                           ClientM, Scheme (Http), runClientM)
import           System.Environment       (lookupEnv)
import           System.Log.Logger        (Priority (..), setLevel, updateGlobalLogger)
import           Text.Read                (readMaybe)

import           Agents                   (AgentCtor, AnyAgent, AnyAgentType,
                                           agents)
import           Cli                      (CliArgs (..), getArgs)
import Log (say, loggerName)
import           OpenAI.Gym               (Action (..), Agent (..), Config (..),
                                           Environment (..), GymEnv (..),
                                           Info (..), InstID (..), Monitor (..),
                                           Observation (..), Outcome (..),
                                           Step (..), envActionSpaceContains,
                                           envActionSpaceInfo,
                                           envActionSpaceSample, envClose,
                                           envCreate, envListAll,
                                           envMonitorClose, envMonitorStart,
                                           envObservationSpaceInfo, envReset,
                                           envStep, shutdownServer, upload)
import Debug.Dump (d)


defaultGame = CartPoleV0

-- | main function, run `example` given CLI args + env vars
main ∷ IO ()
main = do
  CliArgs{game, verbose, quiet, agent} <- getArgs

  let logLvl
        | verbose = DEBUG
        | quiet = WARNING
        | otherwise = INFO
  updateGlobalLogger loggerName $ setLevel logLvl
  say INFO [d|logLvl|]

  gymEnv :: GymEnv <- case readMaybe game of
                Just env -> return env
                Nothing -> do
                  say ERROR "unknown game"
                  return defaultGame -- default

  let agentType = agents Map.! agent
  manager <- newManager defaultManagerSettings
  let client = runExp gymEnv agentType :: ClientM ()
  out <- runClientM client $ ClientEnv manager url Nothing
  case out of
    Left err -> say ERROR [d|err|]
    Right _  -> return ()

  where
    url = BaseUrl Http "localhost" 5000 ""

-- | get game env, run n episodes
runExp ∷ GymEnv → AgentCtor → ClientM ()
runExp gymEnv agentType = do
  envs <- all_envs <$> envListAll
  let gameIds = Map.filter (== (T.pack $ show gymEnv)) envs
  let maybeId = listToMaybe $ map InstID $ Map.keys gameIds
  inst <- case maybeId of
          -- reuse existing env
          Just instId -> return instId
          Nothing     -> envCreate gymEnv
  actionSpace <- envActionSpaceInfo inst
  obsSpace <- envObservationSpaceInfo inst
  say DEBUG [d| gymEnv |]
  say DEBUG [d| envs |]
  say DEBUG [d| gameIds |]
  say DEBUG [d| inst |]
  say INFO [d| actionSpace |]
  say INFO [d| obsSpace |]
  let agent = agentType actionSpace obsSpace
  let exp = replicateM_ episodeCount $ experiment agent inst maxSteps
  exp

  where
    episodeCount = 10
    maxSteps = 200

-- | an experiment for an agent, an environment
-- experiment :: Monad m => AnyAgent -> InstID -> Int -> m ()
experiment ∷ AnyAgent → InstID → Int → ClientM ()
experiment agent inst maxSteps = do
  ob0 <- envReset inst -- first close monitor
  go 0 False ob0
  where
    reward = 0
    -- go :: Monad m => Int -> Bool -> Observation -> m ()
    go ∷ Int → Bool → Observation → ClientM ()
    go t done ob = do
      ac <- act agent ob t inst
      Outcome ob' reward done info <- envStep inst $ Step ac True
      learn agent ob ac reward (Observation ob') done t info
      when (not done && t < maxSteps) $ go (t + 1) done $ Observation ob'
