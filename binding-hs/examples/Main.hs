{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module Main where

import           Control.Exception.Lifted (finally)
import           Control.Monad            (replicateM_, when)
import           Control.Monad.Catch      ()
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.State.Strict (StateT, runStateT)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe, listToMaybe)
import qualified Data.Text                as T
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Servant.Client           (BaseUrl (..), ClientEnv (..),
                                           ClientM, Scheme (Http), runClientM)
import           System.Environment       (lookupEnv)
import           System.Log.Logger        (Priority (..), setLevel, updateGlobalLogger)
import           Text.Read                (readMaybe)
import           Agents                   (AgentCtor, AnyAgent, AnyAgentType, RandomAgent (..),
                                           agents)
import           Cli                      (CliArgs (..), getArgs)
import Log (say, loggerName)
import           OpenAI.Gym               (Action (..), Agent (..), Config (..),
                                           Environment (..), GymEnv (..),
                                           Info (..), InstID (..), Monitor (..),
                                           Observation (..), Outcome (..),
                                           Step (..), EnvSpec (..), ActionSpace,
                                           ObservationSpace, envActionSpaceContains,
                                           envActionSpaceInfo, envActionSpaceSample,
                                           envClose, envCreate, envListAll,
                                           envMonitorClose, envMonitorStart,
                                           envObservationSpaceInfo, envReset,
                                           envStep, shutdownServer, upload, envSpec)


main ∷ IO ()
main = do
  inst <- lift $ envCreate CartPoleV0
  actionSpace <- lift $ envActionSpaceInfo inst
  spec <- lift $ envSpec inst
  obsSpace <- lift $ envObservationSpaceInfo inst
  let agent = RandomAgent spec actionSpace obsSpace ()
  let state = initState agent ()
  ob0 <- lift $ envReset inst
  lift $ act agent state ob0 0 inst
  return ()

