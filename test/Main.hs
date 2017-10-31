module Main
    ( main
    ) where

import           AngleTests                     (addDegreesToDegrees,
                                                 addDegreesToRadians,
                                                 addRadiansToDegrees,
                                                 addRadiansToRadians,
                                                 convertDegreesToDegrees,
                                                 convertDegreesToRadians,
                                                 convertRadiansToDegrees,
                                                 convertRadiansToRadians,
                                                 eulerElevations, eulerHeadings,
                                                 mulDegrees, mulRadians)
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
        , testCase "Add Degrees to Degrees" addDegreesToDegrees
        , testCase "Add Radians to Radians" addRadiansToRadians
        , testCase "Add Radians to Degrees" addRadiansToDegrees
        , testCase "Add Degrees to Radians" addDegreesToRadians
        , testCase "Multiply Degrees" mulDegrees
        , testCase "Multiply Radians" mulRadians
        , testCase "Calculation of Euler headings" eulerHeadings
        , testCase "Calculation of Euler elevations" eulerElevations
        ]
    ]
