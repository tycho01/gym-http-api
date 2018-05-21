{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Cli (CliArgs(..), getArgs) where

import System.Console.CmdArgs (Typeable, Data, cmdArgs, help, name, (&=))

data CliArgs = CliArgs
  {game :: String
  ,state :: Int
  ,scenario :: String
  ,record :: Bool
  ,verbose :: Bool
  ,quiet :: Bool
  ,ram :: Bool
  ,agent :: String
  }
  deriving (Show, Data, Typeable)

-- | Gym constant for default initial game state
stateDefault :: Int
stateDefault = -1

-- | get CLI args. run with `--help` for info.
getArgs :: IO CliArgs
getArgs = cmdArgs CliArgs
  {game = "Airstriker-Genesis" &= name "g" &= name "game" &= help "the name or path for the game to run"
  ,state = stateDefault &= name "t" &= name "state" &= help "the initial state file to load, minus the extension"
  ,scenario = "scenario" &= name "s" &= name "scenario" &= help "the scenario file to load, minus the extension"
  ,record = False &= name "r" &= name "record" &= help "record bk2 movies"
  ,verbose = False &= name "v" &= name "verbose" &= help "increase verbosity (can be specified multiple times)"
  ,quiet = False &= name "q" &= name "quiet" &= help "decrease verbosity (can be specified multiple times)"
  ,ram = False &= name "m" &= name "ram" &= help "the observation type, either image (default) or ram"
  ,agent = "random" &= name "a" &= name "agent" &= help "choose the agent, default random"
  }
