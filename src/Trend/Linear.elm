module Trend.Linear exposing (..)

{-| TODO
-}


{-| This may be a horrible idea, but it should mean we can do nice
things with the interval vs goodness of fit calculation.
-}
type Trend kind
    = Quick Line
    | Robust { slopes : List Float, intercepts : List Float }


type alias Line =
    { slope : Float, intercept : Float }


line : Trend a -> Line
line trend =
    Debug.crash "TODO"


type Quick
    = Quick


{-| ordinary least squares
-}
quick : List ( Float, Float ) -> Trend Quick
quick values =
    Debug.crash "TODO"


goodnessOfFit : List ( Float, Float ) -> Trend Quick -> Result Error Float
goodnessOfFit values fit =
    Debug.crash "TODO"


type Robust
    = Robust


{-| Thiel-Sen estimator
-}
robust : List ( Float, Float ) -> Trend Robust
robust values =
    Debug.crash "TODO"


confidenceInterval : Float -> Trend Robust -> Result Error ( Line, Line )
confidenceInterval values fit =
    Debug.crash "TODO"
