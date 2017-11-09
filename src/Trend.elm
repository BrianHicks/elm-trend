module Trend
    exposing
        ( LinearFit
        , goodnessOfFit
        , linear
        , predictY
        )

{-| Calculate trend lines given 2-dimensonal data.

@docs linear, LinearFit


## Using Fits

@docs goodnessOfFit, predictY

-}

import Trend.Math as Math exposing (Error(..))


{-| A line plotted through points. Get one by passing your data to
[`linear`](#linear).

Make predictions using this and [`predictY`](#predictY) and measure
accuracy with [`goodnessOfFit`](#goodnessOfFit).

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

Examples of data which fit these criteria:

  - relationship of some value to time. For example, are bananas
    getting cheaper or more expensive as time goes on?
  - relationships where doing something more or less leads to doing
    something else more or less. For example, does smoking more
    decrease your life expectancy?

Statistically speaking, this is a least-squares regression.

-}
linear : List ( Float, Float ) -> Result Error LinearFit
linear values =
    case values of
        -- can't draw a line through no values
        [] ->
            Err (NotEnoughData 2)

        -- also can't draw a line through a single value
        _ :: [] ->
            Err (NotEnoughData 2)

        -- we've got two or more, let's go!
        _ ->
            let
                ( xs, ys ) =
                    List.unzip values

                slope =
                    Result.map3 (\correl stddevY stddevX -> correl * stddevY / stddevX)
                        (Math.correlation values)
                        (Math.stddev ys)
                        (Math.stddev xs)

                intercept =
                    Result.map3 (\meanY slope meanX -> meanY - slope * meanX)
                        (Math.mean ys)
                        slope
                        (Math.mean xs)
            in
            Result.map2 LinearFit slope intercept


{-| Predict a value of `y` from a [`LinearFit`](#LinearFit) calculated
with [`linear`](#linear) and a value of `x`.

    linear [ (1, 1), (2, 2), (3, 3), (4, 4) ]
        |> Result.map (\fit -> predictY fit 5)
        --> Ok 5

-}
predictY : LinearFit -> Float -> Float
predictY fit x =
    fit.slope * x + fit.intercept


{-| How good is the fit that [`linear`](#linear) generated? We can
give you a confidence between 0 and 1 representing a percent
confidence. Practically, we'll usually return a value between almost 0
and almost 1.

    values : List (Float, Float)
    values =
        [ (1, 1), (2, 2), (3, 3), (4, 4) ]

    linear values
        |> Result.andThen (\fit -> goodnessOfFit fit values)
        --> Ok 1

Statistically speaking, this is the R-squared value of the linear
regression.

-}
goodnessOfFit : LinearFit -> List ( Float, Float ) -> Result Error Float
goodnessOfFit fit values =
    case values of
        [] ->
            Err (NotEnoughData 1)

        _ ->
            let
                ( xs, ys ) =
                    List.unzip values

                predictions =
                    List.map (predictY fit) xs

                meanY =
                    Math.mean ys

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
