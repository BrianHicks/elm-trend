module Helpers exposing (reasonablyCloseTo)

import Expect exposing (Expectation)


reasonablyCloseTo : Float -> Result x Float -> Expectation
reasonablyCloseTo target maybeActual =
    case maybeActual of
        Err err ->
            Expect.fail <| "got an error result (" ++ toString err ++ ") instead of " ++ toString target

        Ok actual ->
            if target - 0.001 <= actual && actual <= target + 0.001 then
                Expect.pass
            else
                Expect.fail <| toString actual ++ " was not within 0.001 of " ++ toString target
