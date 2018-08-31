module Trend.Linear exposing
    ( Trend
    , Line, line, predictY, predictX
    , Point
    , Quick, quick, goodnessOfFit
    , Robust, robust, confidenceInterval
    )

{-| Calculate trends for linear data (that is, data with one dependent
and one independent variable whose relationship can be described as `y
= mx + b`)

The easiest way to determine if a relationship is linear is to plot of
your values. If your data form a rough line, we're in business. But if
your plot shows a curve or a random point cloud then don't trust the
results you get from these functions. (n.b. check out
`terezka/elm-plot`, which makes this very easy!)

Some kinds of data which fit these criteria:

  - the relationship between some value and time. For example, are
    bananas getting cheaper or more expensive as time goes on? Does
    running something twice take exactly twice as long as running it
    once?
  - Big sociological questions: does an increase in healthcare
    spending lead to longer life expectancy? How much does
    education level depend on socioeconomic status?

@docs Trend


## Using Trend Lines

@docs Line, line, predictY, predictX


# Creating Trends

@docs Point


## Quick Fit

@docs Quick, quick, goodnessOfFit


## Robust Fit

@docs Robust, robust, confidenceInterval

-}

import Trend.Math as Math exposing (Error(..))


{-| A trend generated from your data. This contains various things you
may want, like [`line`](#line)s. Generate these with [`quick`](#quick)
and [`robust`](#robust). You will have different options for
interpretation depending on which method you choose to calculate.
-}
type Trend kind
    = Trend Line kind


{-| A single 2-dimensional point `(x, y)`.
-}
type alias Point =
    ( Float, Float )


{-| The result of a trend prediction. Use this to make predictions
using [`predictY`](#predictY).
-}
type alias Line =
    { slope : Float, intercept : Float }


{-| Retrieve the calculated trend line.
-}
line : Trend a -> Line
line (Trend precalculated _) =
    precalculated


{-| Given an `x`, predict `y`.

    predictY { slope = 1, intercept = 0 } 1
        --> 1

    predictY { slope = -1, intercept = 0 } 5.5 |> Ok
        --> Ok -5.5

-}
predictY : Line -> Float -> Float
predictY { slope, intercept } x =
    slope * x + intercept


{-| Given a `y`, predict `x`.

    predictX { slope = 1, intercept = 0 } 1
        --> 1

    predictX { slope = -1, intercept = 0 } 5.5 |> Ok
        --> Ok -5.5

-}
predictX : Line -> Float -> Float
predictX { slope, intercept } y =
    (y - intercept) / slope


{-| A trend calculated from [`quick`](#quick).
-}
type Quick
    = Quick (List Point)


{-| Plot a line through a series of points `(x, y)`:

     quick [ (1, 1), (2, 2), (3, 3), (4, 4) ]
         |> Result.map line
         --> Ok { slope = 1, intercept = 0 }

This is the fastest of the functions in this module, but it's also the
most susceptible to being thrown off by outliers. Let's look at that
line again, but with an outlier:

     quick [ (1, 1), (2, 2), (3, 3), (4, 4), (5, -5) ]
         |> Result.map line
         --> Ok { slope = -0.9999999999999999, intercept = 3.9999999999999996 }

We went from a _perfect_ fit to a _horrible_ one! And, the more
outliers you have, the worse fit you'll get. You can get one measure
of goodness by sending your the result of this function to
[`goodnessOfFit`](#goodnessOfFit).

Under the covers, this is an [ordinary least squares
regression](https://en.wikipedia.org/wiki/Ordinary_least_squares).

-}
quick : List Point -> Result Error (Trend Quick)
quick values =
    case values of
        -- can't draw a line through no values
        [] ->
            Err (NeedMoreValues 2)

        -- also can't draw a line through a single value
        _ :: [] ->
            Err (NeedMoreValues 2)

        -- we've got two or more, let's go!
        _ ->
            let
                ( xs, ys ) =
                    List.unzip values

                slopeResult =
                    Result.map3 (\correl stddevY stddevX -> correl * stddevY / stddevX)
                        (Math.correlation values)
                        (Math.stddev ys)
                        (Math.stddev xs)

                intercept =
                    Result.map3 (\meanY slope meanX -> meanY - slope * meanX)
                        (Math.mean ys)
                        slopeResult
                        (Math.mean xs)
            in
            Result.map2 Line slopeResult intercept
                |> Result.map (\trendLine -> Trend trendLine (Quick values))


{-| Get the goodness of fit for a quick trend. This is a percent,
represented as a floating point number between 0 and 1. A higher
number generally indicates a better fit, but it doesn't know anything
about what your data _means_. This means that you have to use some
judgement in interpreting it!

    quick [ (1, 1), (2, 2), (3, 3), (4, 4) ]
        |> Result.map goodnessOfFit
        --> Ok 1

And again with that outlier from [`quick`](#quick):

     quick [ (1, 1), (2, 2), (3, 3), (4, 4), (5, -5) ]
         |> Result.map goodnessOfFit
         --> Ok 0.19999999999999996

This calculation is only valid for [`quick`](#quick) trends, since it
measures how well a fit has minimized the square sum of error. That
means it's only really useful for ordinary least squares, not the
Theil-Sen estimator we use for [`robust`](#robust)

**Maintainer's note:** this will evaluate the fit for the original
data. If you need to evaluate goodness of fit for _new_ data given an
existing `Trend`, we'll need to expose a new function. I don't have a
concrete use case for this, so the function does not exist yet. I want
to make this library work for you, so please [open an
issue](https://github.com/BrianHicks/elm-trend/issues/new) if you find
yourself in this situation!

-}
goodnessOfFit : Trend Quick -> Float
goodnessOfFit (Trend fit (Quick values)) =
    let
        ( xs, ys ) =
            List.unzip values

        predictions : List Float
        predictions =
            List.map (predictY fit) xs

        meanY : Float
        meanY =
            Math.mean ys |> Result.withDefault 0

        sumSquareTotal : Float
        sumSquareTotal =
            ys
                |> List.map (\y -> (y - meanY) ^ 2)
                |> List.sum

        sumSquareResiduals : Float
        sumSquareResiduals =
            List.map2 (\actual prediction -> (actual - prediction) ^ 2) ys predictions
                |> List.sum
    in
    1 - sumSquareResiduals / sumSquareTotal


{-| A trend calculated from [`robust`](#robust).
-}
type Robust
    = Robust Line Line


{-| When your data has outliers, you'll want to use a robust estimator
instead of the quick estimator. This is much slower (it runs roughly
in `O(n^2)` time), but will still give good results in the face of
corrupted data. Specifically, it will still work if up to ~29.3% of
your data consists of outliers. Again, the easiest way to check this
is to visualize it. We can provide automated checks, but humans are
still the best at saying "hmm, something's funny here..."

For good data, we have the same results as [`quick`](#quick):

     robust [ (1, 1), (2, 2), (3, 3), (4, 4) ]
         |> Result.map line
         --> Ok { slope = 1, intercept = 0 }

But when we have outliers, we still get a good result:

     robust [ (1, 1), (2, 2), (3, 3), (4, 4), (5, -5) ]
         |> Result.map line
         --> Ok { slope = 1, intercept = 0 }

Under the covers, this is a [Theil-Sen
estimator](https://en.wikipedia.org/wiki/Theil%E2%80%93Sen_estimator)
(which is pretty cool and easy to get an intuitive grasp of what's
going on; check it out!)

-}
robust : List Point -> Result Error (Trend Robust)
robust values =
    case values of
        [] ->
            Err (NeedMoreValues 2)

        _ :: [] ->
            Err (NeedMoreValues 2)

        _ ->
            let
                slopes =
                    values
                        |> List.foldl
                            (\( x, y ) acc1 ->
                                List.foldl
                                    (\( x1, y1 ) acc2 ->
                                        let
                                            res =
                                                (y - y1) / (x - x1)
                                        in
                                        if isNaN res then
                                            acc2

                                        else
                                            res :: acc2
                                    )
                                    acc1
                                    values
                            )
                            []
                        |> List.sort

                finiteSlopes =
                    List.filter (not << isInfinite) slopes
            in
            Maybe.map3
                (\trendLine lower upper -> Trend trendLine (Robust lower upper))
                (theilSenLine 0.5 finiteSlopes values)
                (theilSenLine 0.975 slopes values)
                (theilSenLine 0.025 slopes values)
                -- I *think* AllZeros is the correct error here, but I'm not 100% on it.
                |> Result.fromMaybe AllZeros


theilSenLine : Float -> List Float -> List Point -> Maybe Line
theilSenLine pct slopes points =
    let
        slope =
            percentile pct slopes

        intercept =
            slope
                |> Maybe.map (\m -> List.map (\( x, y ) -> y - m * x) points)
                |> Maybe.map List.sort
                |> Maybe.andThen (percentile pct)
    in
    Maybe.map2 Line slope intercept


{-| get the kth percentile in the list of values. This assumes that
the list is already sorted.
-}
percentile : Float -> List Float -> Maybe Float
percentile k xs =
    let
        index =
            toFloat (List.length xs) * k
    in
    if index - toFloat (floor index) == 0 then
        xs
            |> List.drop (ceiling index - 1)
            |> List.head

    else
        xs
            |> List.drop (floor index - 1)
            |> List.take 2
            |> Math.mean
            |> Result.toMaybe


{-| Calculate a confidence interval from a robust set of
data. [Consult
Wikipedia](https://en.wikipedia.org/wiki/Confidence_interval) for a
thorough understanding of what this may mean for your data set. This
function gives a 95% confidence interval.

**Maintainer's note:** We ought to be able to generate a confidence
interval for quick trends too, but I'm not confident enough in my math
skills to do it correctly. Help wanted here! If you know how to do
that calculation, let's work together and add it.

-}
confidenceInterval : Trend Robust -> ( Line, Line )
confidenceInterval (Trend _ (Robust lower upper)) =
    ( lower, upper )
