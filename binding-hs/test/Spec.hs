module Main where

import Test.HUnit (Test(..), runTestTT, assertEqual)
import Test.HUnit.Lang (Assertion)
import Text.Read (readMaybe)
import OpenAI.Gym (Action(..), Config(..), Environment(..), GymEnv(..), Monitor(..), InstID(..), Outcome(..), Step(..), Agent)

test :: (Eq a) => String -> (String -> a -> a -> Assertion) -> a -> a -> Test
test name f a b = TestLabel name $ TestCase $ f name a b

main :: IO ()
main = do
    runTestTT $ TestList
        [ test "CartPole - Show" assertEqual "CartPole-v0" $ show CartPoleV0
        , test "CartPole - Read" assertEqual (Just CartPoleV0) $ readMaybe "CartPole-v0"
        ]
    return ()
