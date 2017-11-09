module Trend.Linear
    exposing
        ( Line
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


## Quick Fit

@docs quick, goodnessOfFit


## Robust Fit

@docs robust, confidenceInterval

-}

import Trend.Math exposing (Error(..))


{-| -}
type Trend kind
    = QuickTrend Line
      -- TODO: these should be arrays
    | RobustTrend { slopes : List Float, intercepts : List Float }


{-| The result of a trend prediction. Use this to make predictions
using [`predictY`](#predictY).
-}
type alias Line =
    { slope : Float, intercept : Float }


{-| Extract a line from a trend.
-}
line : Trend a -> Line
line trend =
    Debug.crash "TODO"


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

     quick [ (1, 1), (2, 2), (3, 3), (4, 8) ]
         |> Result.map line
         --> Ok { slope = 1, intercept = 0 }

The more outliers you have, the worse fit you'll get. You can figure
out if this is happening by sending your `Trend` to
[`goodnessOfFit`](#goodnessOfFit).

Under the covers, this is an [ordinary least squares
regression](https://TODO.some-nice-explanation.com).

-}
quick : List ( Float, Float ) -> Result Error (Trend Quick)
quick values =
    Debug.crash "TODO"


{-| Get the goodness of fit for a quick trend. This is a number
between 0 to 1. A higher number generally indicates a better fit, but
it doesn't know anything about what your data _means_. This means that
you have to use some judgement in interpreting it!

    quick [ (1, 1), (2, 2), (3, 3), (4, 4) ]
        |> Result.map goodnessOfFit
        --> Ok 1

And again with that outlier from [`quick`](#quick):

     quick [ (1, 1), (2, 2), (3, 3), (4, 8) ]
         |> Result.map goodnessOfFit
         --> Ok 1

**Maintainer's note:** this will evaluate the fit for the original
data. If you need to evaluate goodness of fit for _new_ data given an
existing `Trend`, we'll need to expose a new function. I don't have a
concrete use case for this, so the function does not corrently
exists. I want to make this library work for you, so please [open an
issue](https://github.com/BrianHicks/elm-trend/issues/new) if you find
yourself in this situation!

-}
goodnessOfFit : Trend Quick -> Float
goodnessOfFit fit =
    Debug.crash "TODO"


{-| a trend calculated from [`robust`](#robust)
-}
type Robust
    = Robust


{-| When your data has outliers, you'll want to use a robust estimator
instead of the quick estimator. This is much slower (it runs in
`O(n^2)` time), but will still give good results in the face of
corrupted data. Specifically, it will still work if up to ~29.3% of
your data consists of outliers. And again, the easiest way to check
this is to visualize it with `terezka/elm-plot` or something similar.

For good data, we have the same results as [`quick`](#quick):

     robust [ (1, 1), (2, 2), (3, 3), (4, 4) ]
         |> Result.map line
         --> Ok { slope = 1, intercept = 0 }

But when we have outliers, we still get a good result:

     robust [ (1, 1), (2, 2), (3, 3), (4, 8) ]
         |> Result.map line
         --> Ok { slope = 1, intercept = 0 }

Under the covers, this is a [Thiel-Sen
estimator](https://en.wikipedia.org/wiki/Theil%E2%80%93Sen_estimator)
(which is pretty cool and easy to get an intuitive grasp of what's
going on; check it out!)

-}
robust : List ( Float, Float ) -> Result Error (Trend Robust)
robust values =
    Debug.crash "TODO"


{-| TODO: good docs, including how to interpret this data.
-}
confidenceInterval : Float -> Trend Robust -> Result Error ( Line, Line )
confidenceInterval values fit =
    Debug.crash "TODO"
