module Main where

import Data.Aeson (toJSON, fromJSON)
import Test.HUnit (Test(..), runTestTT, assertEqual)
import Test.HUnit.Lang (Assertion)
import OpenAI.Gym (Action(..), Config(..), Environment(..), GymEnv(..), Monitor(..), InstID(..), Outcome(..), Step(..), Agent)

test :: (Eq a) => String -> (String -> a -> a -> Assertion) -> a -> a -> Test
test name f a b = TestLabel name $ TestCase $ f name a b

main :: IO ()
main = do
    runTestTT $ TestList
        [ test "CartPole" assertEqual "CartPole-v0" $ show CartPoleV0
        ]
    return ()
