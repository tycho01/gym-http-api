{-# LANGUAGE UnicodeSyntax #-}
module Log (say, loggerName) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans    (liftIO)
import           System.Console.ANSI    (Color (..), ColorIntensity (..),
                                         ConsoleIntensity (..),
                                         ConsoleLayer (..), SGR (..), hSetSGR,
                                         setSGR)
import           System.IO              (stderr)
import           System.Log.Logger      (Priority (..), logM, rootLoggerName)

loggerName = rootLoggerName

-- | log stuff, incl. variables like [d|foo, bar|]
say ∷ MonadIO m ⇒ Priority → String → m ()
say lvl msg = liftIO $ do
    hSetSGR stderr [SetColor Foreground intensity color]
    logM loggerName lvl msg
    hSetSGR stderr []
  where intensity = Vivid
        color = case lvl of
          DEBUG   -> Magenta
          WARNING -> Yellow
          INFO    -> Blue
          ERROR   -> Red
          _       -> White
