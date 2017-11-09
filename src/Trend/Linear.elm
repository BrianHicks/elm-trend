module Trend.Linear
    exposing
        ( Line
        , Point
        , Quick
        , Robust
        , Trend
        , confidenceInterval
        , goodnessOfFit
        , line
        , predictY
        , quick
        , robust
        )

{-| Calculate trends for linear data (that is, data with one dependent
and one independent variable whose relationship can be described a `y
= mx + b`)

The easiest way to determine if a relationship is linear is to plot of
your values . If your data form a rough line, we're in business. But
if your plot shows a curve or a random point cloud then don't trust
the results you get from these functions. (n.b. check out
`terezka/elm-plot`, which makes this very easy!)

Some kinds of data which fit these criteria:

  - the relationship between some value and time. For example, are
    bananas getting cheaper or more expensive as time goes on? Does
    running something twice take exactly twice as long as running it
    once?
  - Big sociological questions: does an increase in healthcare
    spending lead to longer life expectancy? How much does
    socioeconomic status depend on education levels?

@docs Trend, Quick, Robust


## Using Trend Lines

@docs Line, line, predictY


# Creating Trends

@docs Point


## Quick Fit

@docs quick, goodnessOfFit


## Robust Fit

@docs robust, confidenceInterval

-}

import Trend.Math as Math exposing (Error(..))


{-| -}
type Trend kind
    = QuickTrend (List Point) Line
      -- TODO: these should be arrays
    | RobustTrend { values : List Point, slopes : List Float, line : Line }


{-| a single 2-dimensional point
-}
type alias Point =
    ( Float, Float )


{-| The result of a trend prediction. Use this to make predictions
using [`predictY`](#predictY).
-}
type alias Line =
    { slope : Float, intercept : Float }


{-| Extract a line from a trend.
-}
line : Trend a -> Line
line trend =
    case trend of
        QuickTrend _ precalculated ->
            precalculated

        RobustTrend { line } ->
            line


{-| Given an `x`, predict `y`.

    predictY { slope = 1, intercept = 0 } 1
        --> 1

    predictY { slope = -1, intercept = 0 } 5.5
        --> -5.5

-}
predictY : Line -> Float -> Float
predictY { slope, intercept } x =
    slope * x + intercept


{-| a trend calculated from [`quick`](#quick)
-}
type Quick
    = Quick


{-| Plot a line through a series of points `(x, y)`. So given a
perfect linear relationship:

     quick [ (1, 1), (2, 2), (3, 3), (4, 4) ]
         |> Result.map line
         --> Ok { slope = 1, intercept = 0 }

This is the fastest of the functions in this module, but it's also the
most susceptible to being thrown off by outliers. Let's look at that
line again, but with an outlier:

     quick [ (1, 1), (2, 2), (3, 3), (4, 0) ]
         |> Result.map line
         --> Ok { slope = -0.2, intercept = 2 }

The more outliers you have, the worse fit you'll get. You can figure
out if this is happening by sending your `Trend` to
[`goodnessOfFit`](#goodnessOfFit).

Under the covers, this is an [ordinary least squares
regression](https://TODO.some-nice-explanation.com).

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
            Result.map2 Line slope intercept
                |> Result.map (QuickTrend values)


{-| Get the goodness of fit for a quick trend. This is a number
between 0 to 1. A higher number generally indicates a better fit, but
it doesn't know anything about what your data _means_. This means that
you have to use some judgement in interpreting it!

    quick [ (1, 1), (2, 2), (3, 3), (4, 4) ]
        |> Result.map goodnessOfFit
        --> Ok 1

And again with that outlier from [`quick`](#quick):

     quick [ (1, 1), (2, 2), (3, 3), (4, 0) ]
         |> Result.map goodnessOfFit
         --> Ok 0.039999999999999813

**Maintainer's note:** this will evaluate the fit for the original
data. If you need to evaluate goodness of fit for _new_ data given an
existing `Trend`, we'll need to expose a new function. I don't have a
concrete use case for this, so the function does not corrently
exists. I want to make this library work for you, so please [open an
issue](https://github.com/BrianHicks/elm-trend/issues/new) if you find
yourself in this situation!

-}
goodnessOfFit : Trend Quick -> Float
goodnessOfFit trend =
    case trend of
        RobustTrend _ ->
            Debug.crash "got a RobustTrend in goodnessOfFit. The phantom type should have prevented this."

        QuickTrend values fit ->
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


{-| a trend calculated from [`robust`](#robust)
-}
type Robust
    = Robust


{-| When your data has outliers, you'll want to use a robust estimator
instead of the quick estimator. This is much slower (it runs roughly
in `O(n^2)` time), but will still give good results in the face of
corrupted data. Specifically, it will still work if up to ~29.3% of
your data consists of outliers. And again, the easiest way to check
this is to visualize it with `terezka/elm-plot` or something similar.

For good data, we have the same results as [`quick`](#quick):

     robust [ (1, 1), (2, 2), (3, 3), (4, 4) ]
         |> Result.map line
         --> Ok { slope = 1, intercept = 0 }

But when we have outliers, we still get a good result:

     robust [ (1, 1), (2, 2), (3, 3), (4, 0) ]
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
                            (\( x, y ) acc ->
                                List.foldl
                                    (\( x1, y1 ) acc ->
                                        if x == x1 then
                                            acc
                                        else
                                            (y - y1) / (x - x1) :: acc
                                    )
                                    acc
                                    values
                            )
                            []
                        |> List.sort

                slope =
                    percentile 0.5 slopes

                intercepts =
                    slope
                        |> Maybe.map
                            (\m -> List.map (\( x, y ) -> y - m * x) values)
                        |> Maybe.map List.sort

                intercept =
                    intercepts
                        |> Maybe.andThen (percentile 0.5)
            in
            Maybe.map2 Line slope intercept
                |> Maybe.map
                    (\line ->
                        RobustTrend
                            { values = values
                            , slopes = slopes
                            , line = line
                            }
                    )
                -- I *think* AllZeros is the correct error here, but I'm not 100% on it.
                |> Result.fromMaybe AllZeros


{-| get the kth percentile in the list of values. This assumes that
the list is already sorted.

TODO: generalize this and move it to Trend.Math? But maybe this is not
a good idea, as it would have to be made less efficient to return
reliable results without the preconditions mentioned above.

-}
percentile : Float -> List a -> Maybe a
percentile k xs =
    -- TODO: this feels basically wrong. It doesn't work well for 50th
    -- percentiles on lists with an odd number of items. Probably
    -- should just grab one of those linear math libraries off of
    -- elm-package or look stuff up on WikiPedia.
    --
    -- TODO #2: use arrays
    xs
        |> List.drop (ceiling <| toFloat (List.length xs) * k)
        |> List.head


{-| TODO: good docs, including how to interpret this data.
-}
confidenceInterval : Float -> Trend Robust -> Result Error ( Line, Line )
confidenceInterval values fit =
    Debug.crash "TODO: robust confidence interval"
