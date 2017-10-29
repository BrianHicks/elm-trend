module TrendSpec exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Test exposing (..)
import Trend exposing (..)


reasonablyCloseTo : Float -> Result x Float -> Expectation
reasonablyCloseTo target maybeActual =
    case maybeActual of
        Err err ->
            Expect.fail <| "got an error result (" ++ toString err ++ ") instead of " ++ toString target

        Ok actual ->
            if target - 0.001 <= actual && actual <= target + 0.001 then
                Expect.pass
            else
                Expect.fail <| toString actual ++ " was not within 0.0001 of " ++ toString target



-- meanTest : Test
-- meanTest =
--     describe "mean"
--         [ test "no values" <|
--             \_ -> Expect.equal Nothing (mean [])
--         , fuzz Fuzz.float "one value" <|
--             \i -> Expect.equal (Just i) (mean [ i ])
--         , fuzz Fuzz.float "two identical values" <|
--             \i -> Expect.equal (Just i) (mean [ i, i ])
--         , test "a small series" <|
--             \_ ->
--                 List.range 0 10
--                     |> List.map toFloat
--                     |> mean
--                     |> Expect.equal (Just 5)
--         ]
-- stddevTest : Test
-- stddevTest =
--     describe "stddev"
--         [ test "no values" <|
--             \_ -> Expect.equal Nothing (stddev [])
--         , fuzz Fuzz.float "one value" <|
--             \i -> Expect.equal (Just 0) (stddev [ i ])
--         , fuzz Fuzz.float "two identical values" <|
--             \i -> Expect.equal (Just 0) (stddev [ i, i ])
--         , fuzz Fuzz.float "a small series" <|
--             \i -> Expect.equal (Just 0.5) (stddev [ i, i + 1 ])
--         ]
-- correlationTest : Test
-- correlationTest =
--     describe "correlation"
--         [ test "no values" <|
--             \_ -> Expect.equal Nothing (correlation [])
--         , test "single value" <|
--             \_ -> Expect.equal Nothing (correlation [ ( 1, 1 ) ])
--         , fuzz Fuzz.float "strong positive correlation" <|
--             \i ->
--                 [ ( i, i ), ( i + 1, i + 1 ), ( i + 2, i + 2 ) ]
--                     |> correlation
--                     |> reasonablyCloseTo 1
--         , fuzz Fuzz.float "strong negative correlation" <|
--             \i ->
--                 [ ( i - 1, i + 1 ), ( i, i ), ( i + 1, i - 1 ) ]
--                     |> correlation
--                     |> reasonablyCloseTo -1
--         , fuzz Fuzz.float "no correlation" <|
--             \i ->
--                 [ ( 0, i ), ( i, 0 ), ( 0, -i ), ( -i, 0 ) ]
--                     |> correlation
--                     |> (if i == 0 then
--                             Expect.equal Nothing
--                         else
--                             reasonablyCloseTo 0
--                        )
--         ]


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
                            Err ResultWasNaN
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
                            Err ResultWasNaN
                         else
                            Ok 0
                        )
        ]
