module Main
    ( main
    ) where

import           AngleTests                           (addDegreesToDegrees,
                                                       addDegreesToRadians,
                                                       addRadiansToDegrees,
                                                       addRadiansToRadians,
                                                       convertDegreesToDegrees,
                                                       convertDegreesToRadians,
                                                       convertRadiansToDegrees,
                                                       convertRadiansToRadians,
                                                       mulDegrees, mulRadians,
                                                       subDegreesFromDegrees,
                                                       subDegreesFromRadians,
                                                       subRadiansFromDegrees,
                                                       subRadiansFromRadians)
import           EulerTests                           (eulerElevations,
                                                       eulerHeadings,
                                                       fromVectorAndBackAgain)
import           Test.Framework                       (Test, defaultMain,
                                                       testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           UtilTests                            (clampInt)


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
        , testCase "Sub Degrees from Degrees" subDegreesFromDegrees
        , testCase "Sub Radians from Radians" subRadiansFromRadians
        , testCase "Sub Radians from Degrees" subRadiansFromDegrees
        , testCase "Sub Degrees from Radians" subDegreesFromRadians
        , testCase "Multiply Degrees" mulDegrees
        , testCase "Multiply Radians" mulRadians
        ]
    , testGroup "Euler tests"
        [ testCase "Calculation of Euler headings" eulerHeadings
        , testCase "Calculation of Euler elevations" eulerElevations
        , testProperty "From vector and back again" fromVectorAndBackAgain
        ]
    , testGroup "Util tests"
        [ testProperty "Clamping an int between -10 and 10" clampInt
        ]
    ]
