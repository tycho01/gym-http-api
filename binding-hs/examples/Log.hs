{-# LANGUAGE UnicodeSyntax #-}
module Log (say, loggerName) where

import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans       (liftIO)
import           System.Console.ANSI       (Color (..), ColorIntensity (..),
                                            ConsoleIntensity (..),
                                            ConsoleLayer (..), SGR (..), setSGR)
import           System.Console.ANSI.Codes (colorToCode)
import           System.Log.Logger         (Priority (..), logM, rootLoggerName)

colorize ∷ String → String → String
colorize color msg = "\x1b[" ++ color ++ "m" ++ msg ++ "\x1b[0m"

intensityToCode ∷ ColorIntensity → Int
intensityToCode intensity = case intensity of
  Dull  -> 30
  Vivid -> 90

loggerName = rootLoggerName -- "Gym Agent"

-- | log stuff, incl. variables like [d|foo, bar|]
say ∷ MonadIO m ⇒ Priority → String → m ()
say lvl msg = liftIO $ logM loggerName lvl $ colorize colorCode msg
  where color = case lvl of
          DEBUG   -> Magenta
          WARNING -> Yellow
          INFO    -> Blue
          ERROR   -> Red
          _       -> White
        colorCode = show $ intensityToCode Dull + colorToCode color
