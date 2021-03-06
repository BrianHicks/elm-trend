module Trend.LinearSpec exposing (goodnessOfFitTests, predictXTests, predictYTests, quickTest, robustTest, standard)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz
import Helpers exposing (reasonablyCloseTo)
import Test exposing (..)
import Trend.Linear exposing (..)
import Trend.Math exposing (Error(..))


predictYTests : Test
predictYTests =
    describe "predictY"
        [ fuzz2 Fuzz.float Fuzz.float "at x=1" <|
            \slope intercept ->
                Expect.within (Absolute 0.0000001)
                    (slope + intercept)
                    (predictY { slope = slope, intercept = intercept } 1)
        ]


predictXTests : Test
predictXTests =
    describe "predictX"
        [ fuzz2 Fuzz.float Fuzz.float "at y=1" <|
            \slope intercept ->
                Expect.within (Absolute 0.0000001)
                    ((1 - intercept) / slope)
                    (predictX { slope = slope, intercept = intercept } 1)
        ]


standard : (List Point -> Result Error (Trend a)) -> List Test
standard predictor =
    [ describe "strong positive correlation" <|
        [ fuzz Fuzz.float "slope" <|
            \i ->
                [ ( i, i ), ( i + 1, i + 1 ), ( i + 2, i + 2 ) ]
                    |> predictor
                    |> Result.map line
                    |> Result.map .slope
                    |> reasonablyCloseTo 1
        , fuzz Fuzz.float "intercept" <|
            \i ->
                [ ( i, i ), ( i + 1, i + 1 ), ( i + 2, i + 2 ) ]
                    |> predictor
                    |> Result.map line
                    |> Result.map .intercept
                    |> reasonablyCloseTo 0
        ]
    , describe "strong negative correlation" <|
        [ fuzz (Fuzz.floatRange -1.0e6 1.0e6) "slope" <|
            \i ->
                [ ( i - 1, i + 1 ), ( i, i ), ( i + 1, i - 1 ) ]
                    |> predictor
                    |> Result.map line
                    |> Result.map .slope
                    |> reasonablyCloseTo -1
        , fuzz (Fuzz.floatRange -1.0e6 1.0e6) "intercept" <|
            \i ->
                [ ( i - 1, i + 1 ), ( i, i ), ( i + 1, i - 1 ) ]
                    |> predictor
                    |> Result.map line
                    |> Result.map .intercept
                    |> reasonablyCloseTo (i * 2)
        ]
    , fuzz Fuzz.float "no correlation" <|
        \i ->
            [ ( 0, i ), ( i, 0 ), ( 0, -i ), ( -i, 0 ) ]
                |> predictor
                |> Result.map line
                |> Expect.equal
                    (if i == 0 then
                        Err AllZeros

                     else
                        Ok { slope = 0, intercept = 0 }
                    )
    ]


quickTest : Test
quickTest =
    describe "quick" (standard quick)


robustTest : Test
robustTest =
    describe "robust" (standard robust)


goodnessOfFitTests : Test
goodnessOfFitTests =
    describe "goodnessOfFit"
        [ fuzz Fuzz.float "good fit" <|
            \i ->
                quick [ ( i, i ), ( i + 1, i + 1 ), ( i + 2, i + 2 ) ]
                    |> Result.map goodnessOfFit
                    |> Expect.equal (Ok 1)
        , fuzz Fuzz.float "bad fit" <|
            \i ->
                quick [ ( 0, i ), ( 0, -i ), ( i, 0 ), ( -i, 0 ) ]
                    |> Result.map goodnessOfFit
                    |> Expect.equal
                        (if i == 0 then
                            Err AllZeros

                         else
                            Ok 0
                        )
        ]
