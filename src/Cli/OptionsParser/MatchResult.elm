module Cli.OptionsParser.MatchResult exposing
    ( MatchResult(..)
    , matchResultToMaybe
    )

{-| TODO

@docs MatchResult

@docs matchResultToMaybe

-}

import Cli.Decode


{-| TODO
-}
type MatchResult cliOptions
    = Match (Result (List Cli.Decode.ValidationError) cliOptions)
    | NoMatch (List String)


{-| TODO
-}
matchResultToMaybe : MatchResult cliOptions -> Maybe (Result (List Cli.Decode.ValidationError) cliOptions)
matchResultToMaybe matchResult =
    case matchResult of
        Match thing ->
            Just thing

        NoMatch unknownFlags ->
            Nothing
