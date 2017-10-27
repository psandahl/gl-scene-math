module Main
    ( main
    ) where

import           AngleTests                     (convertDegreesToDegrees,
                                                 convertDegreesToRadians,
                                                 convertRadiansToDegrees,
                                                 convertRadiansToRadians)
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "Angle tests"
        [ testCase "Convert Degrees to Degrees" convertDegreesToDegrees
        , testCase "Convert Radians to Degrees" convertRadiansToDegrees
        , testCase "Convert Radians to Radians" convertRadiansToRadians
        , testCase "Convert Degrees to Radians" convertDegreesToRadians
        ]
    ]
