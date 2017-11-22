module Trend.LinearSpec exposing (..)

import Expect
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
                Expect.equal
                    (slope + intercept)
                    (predictY { slope = slope, intercept = intercept } 1)
        ]


quickTest : Test
quickTest =
    describe "quick"
        [ describe "strong positive correlation" <|
            [ fuzz Fuzz.float "slope" <|
                \i ->
                    [ ( i, i ), ( i + 1, i + 1 ), ( i + 2, i + 2 ) ]
                        |> quick
                        |> Result.map line
                        |> Result.map .slope
                        |> reasonablyCloseTo 1
            , fuzz Fuzz.float "intercept" <|
                \i ->
                    [ ( i, i ), ( i + 1, i + 1 ), ( i + 2, i + 2 ) ]
                        |> quick
                        |> Result.map line
                        |> Result.map .intercept
                        |> reasonablyCloseTo 0
            ]
        , describe "strong negative correlation" <|
            [ fuzz (Fuzz.floatRange -1.0e6 1.0e6) "slope" <|
                \i ->
                    [ ( i - 1, i + 1 ), ( i, i ), ( i + 1, i - 1 ) ]
                        |> quick
                        |> Result.map line
                        |> Result.map .slope
                        |> reasonablyCloseTo -1
            , fuzz (Fuzz.floatRange -1.0e6 1.0e6) "intercept" <|
                \i ->
                    [ ( i - 1, i + 1 ), ( i, i ), ( i + 1, i - 1 ) ]
                        |> quick
                        |> Result.map line
                        |> Result.map .intercept
                        |> reasonablyCloseTo (i * 2)
            ]
        , fuzz Fuzz.float "no correlation" <|
            \i ->
                quick [ ( 0, i ), ( i, 0 ), ( 0, -i ), ( -i, 0 ) ]
                    |> Result.map line
                    |> Expect.equal
                        (if i == 0 then
                            Err AllZeros
                         else
                            Ok { slope = 0, intercept = 0 }
                        )
        ]


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
