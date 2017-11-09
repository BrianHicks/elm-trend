module TrendSpec exposing (..)

import Expect
import Fuzz
import Helpers exposing (reasonablyCloseTo)
import Test exposing (..)
import Trend exposing (..)
import Trend.Math exposing (Error(..))


linearTest : Test
linearTest =
    describe "linear"
        [ describe "strong positive correlation" <|
            [ fuzz Fuzz.float "slope" <|
                \i ->
                    [ ( i, i ), ( i + 1, i + 1 ), ( i + 2, i + 2 ) ]
                        |> linear
                        |> Result.map .slope
                        |> reasonablyCloseTo 1
            , fuzz Fuzz.float "intercept" <|
                \i ->
                    [ ( i, i ), ( i + 1, i + 1 ), ( i + 2, i + 2 ) ]
                        |> linear
                        |> Result.map .intercept
                        |> reasonablyCloseTo 0
            ]
        , describe "strong negative correlation" <|
            [ fuzz (Fuzz.floatRange -1.0e6 1.0e6) "slope" <|
                \i ->
                    [ ( i - 1, i + 1 ), ( i, i ), ( i + 1, i - 1 ) ]
                        |> linear
                        |> Result.map .slope
                        |> reasonablyCloseTo -1
            , fuzz (Fuzz.floatRange -1.0e6 1.0e6) "intercept" <|
                \i ->
                    [ ( i - 1, i + 1 ), ( i, i ), ( i + 1, i - 1 ) ]
                        |> linear
                        |> Result.map .intercept
                        |> reasonablyCloseTo (i * 2)
            ]
        , fuzz Fuzz.float "no correlation" <|
            \i ->
                linear [ ( 0, i ), ( i, 0 ), ( 0, -i ), ( -i, 0 ) ]
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
                let
                    values =
                        [ ( i, i ), ( i + 1, i + 1 ), ( i + 2, i + 2 ) ]
                in
                linear values
                    |> Result.andThen (\fit -> goodnessOfFit fit values)
                    |> Expect.equal (Ok 1)
        , fuzz Fuzz.float "bad fit" <|
            \i ->
                let
                    values =
                        [ ( 0, i ), ( 0, -i ), ( i, 0 ), ( -i, 0 ) ]
                in
                linear values
                    |> Result.andThen (\fit -> goodnessOfFit fit values)
                    |> Expect.equal
                        (if i == 0 then
                            Err AllZeros
                         else
                            Ok 0
                        )
        ]
