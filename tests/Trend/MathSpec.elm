module Trend.MathSpec exposing (..)

import Expect
import Fuzz
import Helpers exposing (reasonablyCloseTo)
import Test exposing (..)
import Trend.Math exposing (..)


meanTest : Test
meanTest =
    describe "mean"
        [ test "no values" <|
            \_ -> Expect.equal (Err NotEnoughData) (mean [])
        , fuzz Fuzz.float "one value" <|
            \i -> Expect.equal (Ok i) (mean [ i ])
        , fuzz Fuzz.float "two identical values" <|
            \i -> Expect.equal (Ok i) (mean [ i, i ])
        , test "a small series" <|
            \_ ->
                List.range 0 10
                    |> List.map toFloat
                    |> mean
                    |> Expect.equal (Ok 5)
        ]


stddevTest : Test
stddevTest =
    describe "stddev"
        [ test "no values" <|
            \_ -> Expect.equal (Err NotEnoughData) (stddev [])
        , fuzz Fuzz.float "one value" <|
            \i -> Expect.equal (Ok 0) (stddev [ i ])
        , fuzz Fuzz.float "two identical values" <|
            \i -> Expect.equal (Ok 0) (stddev [ i, i ])
        , fuzz Fuzz.float "a small series" <|
            \i -> Expect.equal (Ok 0.5) (stddev [ i, i + 1 ])
        ]


correlationTest : Test
correlationTest =
    describe "correlation"
        [ test "no values" <|
            \_ -> Expect.equal (Err NotEnoughData) (correlation [])
        , test "single value" <|
            \_ -> Expect.equal (Err NotEnoughData) (correlation [ ( 1, 1 ) ])
        , fuzz Fuzz.float "strong positive correlation" <|
            \i ->
                [ ( i, i ), ( i + 1, i + 1 ), ( i + 2, i + 2 ) ]
                    |> correlation
                    |> reasonablyCloseTo 1
        , fuzz Fuzz.float "strong negative correlation" <|
            \i ->
                [ ( i - 1, i + 1 ), ( i, i ), ( i + 1, i - 1 ) ]
                    |> correlation
                    |> reasonablyCloseTo -1
        , fuzz Fuzz.float "no correlation" <|
            \i ->
                [ ( 0, i ), ( i, 0 ), ( 0, -i ), ( -i, 0 ) ]
                    |> correlation
                    |> (if i == 0 then
                            Expect.equal (Err ResultWasNaN)
                        else
                            reasonablyCloseTo 0
                       )
        ]
