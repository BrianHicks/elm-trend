module Trend.Math exposing
    ( Error(..)
    , mean, stddev, correlation
    )

{-| Math helpers for calculating trends.

@docs Error

@docs mean, stddev, correlation

-}


{-| Indicate that something has gone wrong in the caculation of a
trend line. Specifically:

  - `NeedMoreValues`: there was not enough data to calculate a
    value. The attached `Int` is the minimum required.
  - `AllZeros`: this likely means you've tried to plot a point
    through a bunch of zeroes, or a values that are very very close to
    zero. If that's not the case, please open an issue.

-}
type Error
    = NeedMoreValues Int
    | AllZeros


{-| Calculate the mean (average) for some values.

    mean [ 1, 2, 3, 4, 5 ]
        --> Ok 3

-}
mean : List Float -> Result Error Float
mean numbers =
    case numbers of
        [] ->
            Err (NeedMoreValues 1)

        _ ->
            Ok <| List.sum numbers / toFloat (List.length numbers)


{-| Calculate the standard deviation for some values.

    stddev [ 1, 2, 3, 4, 5 ]
        --> Ok 1.4142135623730951

-}
stddev : List Float -> Result Error Float
stddev numbers =
    let
        helper : Float -> Result Error Float
        helper seriesMean =
            numbers
                |> List.map (\n -> (n - seriesMean) ^ 2)
                |> mean
                |> Result.map sqrt
    in
    mean numbers |> Result.andThen helper


{-| Get the correlation coefficient for some values. The returned
value will be between 0 (no correlation) and 1 (perfect correlation.)

    correlation [ (1, 1), (2, 2), (3, 3), (4, 4) ]
        --> Ok 1

Minimum required values: 2

Under the covers, this is a Pearson correlation coefficient.

-}
correlation : List ( Float, Float ) -> Result Error Float
correlation values =
    case values of
        -- you can't get a correlation out of no data points
        [] ->
            Err (NeedMoreValues 2)

        -- you can't get a correlation out of a single data point
        _ :: [] ->
            Err (NeedMoreValues 2)

        -- two or more? That's more like it.
        _ ->
            let
                ( xs, ys ) =
                    List.unzip values

                standardize : Result Error Float -> Result Error Float -> List Float -> Result Error (List Float)
                standardize meanResult stddevResult series =
                    Result.map2
                        (\meanValue stddevValue -> List.map (\point -> (point - meanValue) / stddevValue) series)
                        meanResult
                        stddevResult

                summedProduct =
                    Result.map2
                        (\stdX stdY -> List.map2 (*) stdX stdY)
                        (standardize (mean xs) (stddev xs) xs)
                        (standardize (mean ys) (stddev ys) ys)
                        |> Result.map List.sum
            in
            summedProduct
                |> Result.map (\sum -> sum / toFloat (List.length values))
                |> Result.andThen
                    (\val ->
                        if isNaN val then
                            Err AllZeros

                        else
                            Ok val
                    )
