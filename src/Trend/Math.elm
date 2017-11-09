module Trend.Math
    exposing
        ( Error(..)
        , correlation
        , mean
        , stddev
        )

{-| Math helpers for calculating trends.

@docs Error

@docs mean, stddev, correlation

-}


{-| Indicate that something has gone wrong in the caculation of a
trend line. Specifically:

  - `NotEnoughData`: there was not enough data to calculate a
    value. Each function returning this error specifies how many
    points it needs.
  - `ResultWasNaN`: this likely means you've tried to plot a point
    through a bunch of zeroes, or a values that are very very close to
    zero. If that's not the case, please open an issue.

-}
type Error
    = NotEnoughData
    | ResultWasNaN


{-| Calculate the mean (average) for some values.

Minimum required values: 1

-}
mean : List Float -> Result Error Float
mean numbers =
    case numbers of
        [] ->
            Err NotEnoughData

        _ ->
            Ok <| List.sum numbers / toFloat (List.length numbers)


{-| Calculate the standard deviation for some values.

Minimum required values: 1

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

Minimum required values: 2

-}
correlation : List ( Float, Float ) -> Result Error Float
correlation values =
    case values of
        -- you can't get a correlation out of no data points
        [] ->
            Err NotEnoughData

        -- you can't get a correlation out of a single data point
        _ :: [] ->
            Err NotEnoughData

        -- two or more? That's more like it.
        _ ->
            let
                ( xs, ys ) =
                    List.unzip values

                standardize : Result Error Float -> Result Error Float -> List Float -> Result Error (List Float)
                standardize meanResult stddevResult series =
                    Result.map2
                        (\mean stddev -> List.map (\point -> (point - mean) / stddev) series)
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
                            Err ResultWasNaN
                        else
                            Ok val
                    )
