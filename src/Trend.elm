module Trend exposing (LinearFit, linear)

{-| Calculate trend lines given 2-dimensonal data.

@docs linear, LinearFit

-}


mean : List Float -> Maybe Float
mean numbers =
    case numbers of
        [] ->
            Nothing

        _ ->
            Just <| List.sum numbers / toFloat (List.length numbers)


stddev : List Float -> Maybe Float
stddev numbers =
    let
        helper : Float -> Maybe Float
        helper seriesMean =
            numbers
                |> List.map (\n -> (n - seriesMean) ^ 2)
                |> mean
                |> Maybe.map sqrt
    in
    mean numbers |> Maybe.andThen helper


correlation : List ( Float, Float ) -> Maybe Float
correlation values =
    case values of
        -- you can't get a correlation out of no data points
        [] ->
            Nothing

        -- you can't get a correlation out of a single data point
        _ :: [] ->
            Nothing

        -- two or more? That's more like it.
        _ ->
            let
                ( xs, ys ) =
                    List.unzip values

                standardize : Maybe Float -> Maybe Float -> List Float -> Maybe (List Float)
                standardize maybeMean maybeStddev series =
                    Maybe.map2
                        (\mean stddev -> List.map (\point -> (point - mean) / stddev) series)
                        maybeMean
                        maybeStddev

                summedProduct =
                    Maybe.map2
                        (\stdX stdY -> List.map2 (*) stdX stdY)
                        (standardize (mean xs) (stddev xs) xs)
                        (standardize (mean ys) (stddev ys) ys)
                        |> Maybe.map List.sum
            in
            summedProduct
                |> Maybe.map (\sum -> sum / toFloat (List.length values))
                |> Maybe.andThen
                    (\val ->
                        if isNaN val then
                            Nothing
                        else
                            Just val
                    )


{-| A line plotted through points. Get one by passing your data to
[`linear`](#linear).

TODO: prediction functions, goodness of fit

-}
type alias LinearFit =
    { slope : Float, intercept : Float }


{-| Plot a line through a series of points `(x, y)`.

    -- TODO: example

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
linear : List ( Float, Float ) -> Maybe LinearFit
linear values =
    case values of
        -- can't draw a line through no values
        [] ->
            Nothing

        -- also can't draw a line through a single value
        _ :: [] ->
            Nothing

        -- we've got two or more, let's go!
        _ ->
            let
                ( xs, ys ) =
                    List.unzip values

                slope =
                    Maybe.map3 (\correl stddevY stddevX -> correl * stddevY / stddevX)
                        (correlation values)
                        (stddev ys)
                        (stddev xs)

                intercept =
                    Maybe.map3 (\meanY slope meanX -> meanY - slope * meanX)
                        (mean ys)
                        slope
                        (mean xs)
            in
            Maybe.map2 LinearFit slope intercept


predictY : LinearFit -> Float -> Float
predictY fit x =
    fit.slope * x + fit.intercept


goodnessOfFit : LinearFit -> List ( Float, Float ) -> Maybe Float
goodnessOfFit fit values =
    case values of
        [] ->
            Nothing

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
                        |> Maybe.map (\localMean -> List.map (\y -> (y - localMean) ^ 2) ys)
                        |> Maybe.map List.sum

                sumSquareResiduals =
                    List.map2 (\actual prediction -> (actual - prediction) ^ 2) ys predictions
                        |> List.sum
            in
            sumSquareTotal
                |> Maybe.map (\ssT -> 1 - sumSquareResiduals / ssT)
