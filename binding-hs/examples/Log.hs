module Log (log, loggerName) where

import           Prelude                  hiding (log)
import System.Console.ANSI (SGR(..), ConsoleLayer(..), ColorIntensity(..), setSGR, Color(..), ConsoleIntensity(..))
import System.Console.ANSI.Codes (colorToCode)
import           System.Log.Logger        (Priority (..), logM, rootLoggerName)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Trans      (liftIO)

colorize :: String -> String -> String
colorize color msg = "\x1b[" ++ color ++ "m" ++ msg ++ "\x1b[0m"

intensityToCode :: ColorIntensity -> Int
intensityToCode intensity = case intensity of
  Dull -> 30
  Vivid -> 90

loggerName = rootLoggerName -- "Gym Agent"

-- | log Showable
logThing :: (MonadIO m, Show a) => Priority -> a -> m ()
logThing lvl = liftIO . logM loggerName lvl . show

-- | log quasiquoted variables, e.g. [d|foo, bar|]
log :: MonadIO m => Priority -> String -> m ()
log lvl msg = liftIO $ logM loggerName lvl $ colorize colorCode msg
  where color = case lvl of
          DEBUG -> Magenta
          WARNING -> Yellow
          INFO -> Blue
          ERROR -> Red
          _ -> White
        colorCode = show $ intensityToCode Dull + colorToCode color
