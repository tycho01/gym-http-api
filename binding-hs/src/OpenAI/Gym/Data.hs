-------------------------------------------------------------------------------
-- |
-- Module    :  OpenAI.Gym.Data
-- License   :  MIT
-- Stability :  experimental
-- Portability: non-portable
--
-- Aeson-based data types to be returned by "OpenAI.Gym.API"
-------------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UnicodeSyntax             #-}
module OpenAI.Gym.Data
  ( GymEnv (..)
  , InstID (..)
  , Environment (..)
  , Observation (..)
  , Step (..)
  , Outcome (..)
  , Info (..)
  , Action (..)
  , Monitor (..)
  , Config (..)
  , Agent (..)
  , Space (..)
  , DType (..)
  -- , AnyAgent
  , ActionSpace
  , ObservationSpace
  , SpaceContains
  ) where

import           Data.Aeson                (FromJSON (..), Object, ToJSON (..),
                                            Value (..), object, (.:), (.=))
import qualified Data.Aeson                as A ()
import           Data.Aeson.Types          (Parser, withEmbeddedJSON,
                                            withObject, withText)
import           Data.HashMap.Strict       (HashMap, fromList)
import           Data.Map.Strict           (Map, map, mapKeys, toList)
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text, pack)
import qualified Data.Text                 as T ()
import           GHC.Exts                  (fromList)
import           GHC.Generics              (Generic)
import           Prelude                   hiding (print, pure, (<*))
import           Servant.API               (ToHttpApiData (..))
import           Servant.Client            (ClientM)
import           Text.Syntax               (Syntax, text, (<|>))
import           Text.Syntax.Classes       (pure)
import           Text.Syntax.Combinators   ((<*))
import           Text.Syntax.Parser.Naive  (Parser (..))
import           Text.Syntax.Printer.Naive (print)
-- {-# ANN module "HLint: ignore Use camelCase" #-} -- silences hlint but won't compile...

-- runParser :: ? a => Parser a -> String -> [(a, String)]
runParser (Parser p) = p

-- | Classic Control Environments
data GymEnv
  = CartPoleV0               -- ^ Balance a pole on a cart (for a short time).
  | CartPoleV1               -- ^ Balance a pole on a cart.
  | AcrobotV1                -- ^ Swing up a two-link robot.
  | MountainCarV0            -- ^ Drive up a big hill.
  | MountainCarContinuousV0  -- ^ Drive up a big hill with continuous control.
  | PendulumV0               -- ^ Swing up a pendulum.

  -- Toy text games
  | FrozenLakeV0             -- ^ Swing up a pendulum.

  -- | Atari Games
  | PongRamV0                -- ^ Maximize score in the game Pong, with RAM as input
  | PongV0                   -- ^ Maximize score in the game Pong

  -- Retro games
  -- Genesis games
  | Airstriker
  deriving (Eq, Enum, Ord)

gymEnv ∷ Syntax f ⇒ f GymEnv
gymEnv =  pure CartPoleV0               <* text "CartPole-v0"
      <|> pure CartPoleV1               <* text "CartPole-v1"
      <|> pure AcrobotV1                <* text "Acrobot-v1"
      <|> pure MountainCarV0            <* text "MountainCar-v0"
      <|> pure MountainCarContinuousV0  <* text "MountainCarContinuous-v0"
      <|> pure PendulumV0               <* text "Pendulum-v0"
      <|> pure FrozenLakeV0             <* text "FrozenLake-v0"
      <|> pure PongRamV0                <* text "Pong-ram-v0"
      <|> pure PongV0                   <* text "Pong-v0"
      <|> pure Airstriker               <* text "Airstriker-Genesis"

instance Show GymEnv where show = fromJust . print gymEnv
instance Read GymEnv where readsPrec _ = runParser gymEnv

instance ToJSON GymEnv where
  toJSON env = object [ "env_id" .= show env ]

-- | a short identifier (such as '3c657dbc') for the created environment instance.
-- The instance_id is used in future API calls to identify the environment to be manipulated.
newtype InstID = InstID { getInstID :: Text }
  deriving (Eq, Show, Generic)

instance ToHttpApiData InstID where
  toUrlPiece (InstID i) = i

instance ToJSON InstID where
  toJSON (InstID i) = toSingleton "instance_id" i

instance FromJSON InstID where
  parseJSON = parseSingleton InstID "instance_id"

-- | whether the value belongs to the action_space
newtype SpaceContains = SpaceContains { getSpaceContains :: Bool }
  deriving (Eq, Show, Generic)

instance ToJSON SpaceContains where
  toJSON (SpaceContains b) = toSingleton "member" b

instance FromJSON SpaceContains where
  parseJSON = parseSingleton SpaceContains "member"

-- | a mapping of instance_id to env_id (e.g. {'3c657dbc': 'CartPole-v0'}) for every env on the server
newtype Environment = Environment { all_envs :: Map Text Text }
  deriving (Eq, Show, Generic)

instance ToJSON Environment
instance FromJSON Environment

newtype ActionSpace = ActionSpace Space

newtype ObservationSpace = ObservationSpace Space

-- | The agent's observation of the current environment
newtype Observation = Observation { getObservation :: Value }
  deriving (Eq, Show, Generic)

instance ToJSON Observation where
  toJSON (Observation v) = toSingleton "observation" v

instance FromJSON Observation where
  parseJSON = parseSingleton Observation "observation"

-- | An action to take in the environment and whether or not to render that change
data Step = Step
  { action :: !Value
  , render :: !Bool -- not respected by server
  } deriving (Eq, Generic, Show)

instance ToJSON Step

-- | The result of taking a step in an environment
data Outcome = Outcome
  { observation :: !Value  -- ^ agent's observation of the current environment
  , reward      :: !Double -- ^ amount of reward returned after previous action
  , done        :: !Bool   -- ^ whether the episode has ended
  , info        :: !Object -- ^ a dict containing auxiliary diagnostic information
  } deriving (Eq, Show, Generic)

instance ToJSON Outcome
instance FromJSON Outcome

-- | A dict containing auxiliary diagnostic information
newtype Info = Info { getInfo :: Object }
  deriving (Eq, Show, Generic)

instance ToJSON Info where
  toJSON (Info v) = toSingleton "info" v

instance FromJSON Info where
  parseJSON = parseSingleton Info "info"

-- | An action to take in the environment
newtype Action = Action { getAction :: Value }
  deriving (Eq, Show, Generic)

instance ToJSON Action where
  toJSON (Action v) = toSingleton "action" v

instance FromJSON Action where
  parseJSON = parseSingleton Action "action"

-- | Parameters used to start a monitoring session.
data Monitor = Monitor
  { directory      :: !Text -- ^ directory to use for monitoring
  , force          :: !Bool -- ^ Clear out existing training data from this directory (by deleting
                            --   every file prefixed with "openaigym.") (default=False)
  , resume         :: !Bool -- ^ Retain the training data already in this directory, which will be
                            --   merged with our new data. (default=False)
  , video_callable :: !Bool -- ^ video_callable parameter from the native env.monitor.start function
  } deriving (Generic, Eq, Show)

instance ToJSON Monitor

-- | Parameters used to upload a monitored session to OpenAI's servers
data Config = Config
  { training_dir :: !Text -- ^ A directory containing the results of a training run.
  , algorithm_id :: !Text -- ^ An arbitrary string indicating the paricular version of the
                          --   algorithm (including choices of parameters) you are running.
                          --   (default=None)
  , api_key      :: !Text -- ^ Your OpenAI API key
  } deriving (Generic, Eq, Show)

instance ToJSON Config


-- shape, dtype, n/low/high
data Space =
    Discrete
      { n     :: Int
      -- , shape :: [Int]
      }
  | MultiBinary
      { n     :: Int
      -- , shape :: [Int]
      }
  | MultiDiscrete
      { nvec :: [Int]
      }
  | Box
      { shape :: [Int]
      , low   :: [Double]
      , high  :: [Double]
      , dtype :: DType
      }
  | HighLow
      { num_rows :: Int
      , matrix   :: [Double] -- nested?
      }
  | TupleSpace [Space]
  | DictSpace (Map String Space)
  deriving (Eq, Show)

instance ToJSON Space where
  toJSON (Discrete n) = object -- shape
    [ "name" .= ("Discrete" :: String)
    , "n" .= n
    -- , "shape" .= shape
    ]
  toJSON (MultiBinary n) = object -- shape
    [ "name" .= ("MultiBinary" :: String)
    , "n" .= n
    -- , "shape" .= shape
    ]
  toJSON (MultiDiscrete nvec) = object
    [ "name" .= ("MultiDiscrete" :: String)
    , "nvec" .= nvec
    ]
  toJSON (Box shape low high dtype) = object
    [ "name" .= ("Box" :: String)
    , "shape" .= shape
    , "low" .= low
    , "high" .= high
    , "dtype" .= dtype
    ]
  toJSON (HighLow num_rows matrix) = object
    [ "name" .= ("HighLow" :: String)
    , "num_rows" .= num_rows
    , "matrix" .= matrix
    ]
  toJSON (TupleSpace spaces) = object
    [ "name" .= ("Tuple" :: String)
    , "spaces" .= (Array $ GHC.Exts.fromList $ toJSON <$> spaces)
    ]
  toJSON (DictSpace spaces) = object
    [ "name" .= ("Dict" :: String)
    , "spaces" .= (Object $ Data.HashMap.Strict.fromList $ toList $
                    mapKeys pack $ Data.Map.Strict.map toJSON spaces)
    ]

instance FromJSON Space where
  parseJSON = withObject "Space" $ \v -> do
    name <- v .: "name"
    let f = case name of
              "Discrete" -> \o -> Discrete
                <$> o .: "n"
                -- <*> o .: "shape"
              "MultiBinary" -> \o -> MultiBinary
                <$> o .: "n"
                -- <*> o .: "shape"
              "MultiDiscrete" -> \o -> MultiDiscrete
                <$> o .: "nvec"
              "Box" -> \o -> Box
                <$> o .: "shape"
                <*> o .: "low"
                <*> o .: "high"
                <*> o .: "dtype"
              "HighLow" -> \o -> HighLow
                <$> o .: "num_rows"
                <*> o .: "matrix"
              "TupleSpace" -> \o -> TupleSpace
                <$> o .: "spaces"
              "DictSpace" -> \o -> DictSpace
                <$> o .: "spaces"
    withObject name f $ Object v

-- | a Numpy `dtype`, see:
-- - https://github.com/numpy/numpy/blob/master/numpy/doc/basics.py
-- - https://github.com/numpy/numpy/blob/master/numpy/core/numerictypes.py
data DType =
    Int8
  | Int16
  | Int32
  | Int64
  | UInt8
  | UInt16
  | UInt32
  | UInt64
  deriving (Eq, Show)

instance ToJSON DType where
  toJSON Int8   = "int8"
  toJSON Int16  = "int16"
  toJSON Int32  = "int32"
  toJSON Int64  = "int64"
  toJSON UInt8  = "uint8"
  toJSON UInt16 = "uint16"
  toJSON UInt32 = "uint32"
  toJSON UInt64 = "uint64"

instance FromJSON DType where
  parseJSON = withText "DType" $ \s -> return $ case s of
    "int8"   -> Int8
    "int16"  -> Int16
    "int32"  -> Int32
    "int64"  -> Int64
    "uint8"  -> UInt8
    "uint16" -> UInt16
    "uint32" -> UInt32
    "uint64" -> UInt64

-- | an agent as described in the reinforcement learning literature
class Agent agent where
  -- act :: Monad m => agent -> Observation -> Int -> m Action
  -- learn :: Monad m => agent -> Observation -> Action -> Double -> Observation -> Bool -> Int -> Info -> m ()
  -- actionSpace ∷ agent → Info
  -- obsSpace ∷ agent → Info
  act ∷ agent → Observation → Int → InstID → ClientM Action
  learn ∷ agent → Observation → Action → Double → Observation → Bool → Int → Info → ClientM ()
  learn agent ob ac reward ob_ done t info = return ()

-- -- | a type yielding the union of any Agent implementation
-- data AnyAgent = forall a . Agent a ⇒ AnyAgent a
-- instance Agent AnyAgent where
--   act (AnyAgent a) ob t inst = act a ob t inst
--   learn (AnyAgent a) ob ac reward ob_ done t info = learn a ob ac reward ob_ done t info

-- | helper to parse a singleton object from aeson
parseSingleton ∷ FromJSON a ⇒ (a → b) → Text → Value → Data.Aeson.Types.Parser b
parseSingleton fn f (Object v) = fn <$> v .: f
parseSingleton fn f _          = mempty

-- | convert a value into a singleton object
toSingleton ∷ ToJSON a ⇒ Text → a → Value
toSingleton f a = object [ f .= toJSON a ]
