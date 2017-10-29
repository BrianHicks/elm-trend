module Trend exposing (Error(..), LinearFit, linear)

{-| Calculate trend lines given 2-dimensonal data.

@docs linear, LinearFit


## Errors

@docs Error

-}


{-| Indicate that something has gone wrong in the caculation of a
trend line. Specifically:

  - `NotEnoughData`: there was not enough data to calculate a trend. The
    number attached is the minimum number of points needed to
    calculate.

-}
type Error
    = NotEnoughData


mean : List Float -> Result Error Float
mean numbers =
    case numbers of
        [] ->
            Err NotEnoughData

        _ ->
            Ok <| List.sum numbers / toFloat (List.length numbers)


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
                            Err NotEnoughData
                        else
                            Ok val
                    )


{-| A line plotted through points. Get one by passing your data to
[`linear`](#linear).

TODO: prediction functions, goodness of fit

-}
type alias LinearFit =
    { slope : Float, intercept : Float }


{-| Plot a line through a series of points `(x, y)`.

     linear [ (1, 1), (2, 2), (3, 3), (4, 4) ]
         --> Ok { slope = 1, intercept = 0 }

Use this in situations where the relationship between `x` and `y` is
linear and has as few outliers as possible. A relationship is linear
if it can be described accurately as `y = x * slope + intercept`. The
easiest way to determine this is to look at a plot of your values. If
they look roughly like a line, we're in business. But if your plot
shows a curve or a random point cloud then don't trust the results of
this function applied to them.

Examples of good linear relationships:

  - relationship of some value to time. For example, are bananas
    getting cheaper or more expensive as time goes on?
  - relationships where doing something more or less leads to doing
    something else more or less. For example, does smoking more
    decrease your life expectancy?

-}
linear : List ( Float, Float ) -> Result Error LinearFit
linear values =
    case values of
        -- can't draw a line through no values
        [] ->
            Err NotEnoughData

        -- also can't draw a line through a single value
        _ :: [] ->
            Err NotEnoughData

        -- we've got two or more, let's go!
        _ ->
            let
                ( xs, ys ) =
                    List.unzip values

                slope =
                    Result.map3 (\correl stddevY stddevX -> correl * stddevY / stddevX)
                        (correlation values)
                        (stddev ys)
                        (stddev xs)

                intercept =
                    Result.map3 (\meanY slope meanX -> meanY - slope * meanX)
                        (mean ys)
                        slope
                        (mean xs)
            in
            Result.map2 LinearFit slope intercept


predictY : LinearFit -> Float -> Float
predictY fit x =
    fit.slope * x + fit.intercept


goodnessOfFit : LinearFit -> List ( Float, Float ) -> Result Error Float
goodnessOfFit fit values =
    case values of
        [] ->
            Err NotEnoughData

        _ ->
            let
                ( xs, ys ) =
                    List.unzip values

                predictions =
                    List.map (predictY fit) xs

                meanY =
                    mean ys

                sumSquareTotal =
                    meanY
                        |> Result.map (\localMean -> List.map (\y -> (y - localMean) ^ 2) ys)
                        |> Result.map List.sum

                sumSquareResiduals =
                    List.map2 (\actual prediction -> (actual - prediction) ^ 2) ys predictions
                        |> List.sum
            in
            sumSquareTotal
                |> Result.map (\ssT -> 1 - sumSquareResiduals / ssT)
