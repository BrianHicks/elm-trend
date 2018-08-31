module Helpers exposing (reasonablyCloseTo)

import Expect exposing (Expectation, FloatingPointTolerance(..))


reasonablyCloseTo : Float -> Result x Float -> Expectation
reasonablyCloseTo target maybeActual =
    case maybeActual of
        Err err ->
            Expect.ok maybeActual

        Ok actual ->
            Expect.within (Absolute 0.001) target actual
