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
type MatchResult globalOptions cliOptions
    = Match (Result (List Cli.Decode.ValidationError) ( globalOptions, cliOptions ))
    | NoMatch (List String)


{-| TODO
-}
matchResultToMaybe : MatchResult globalOptions cliOptions -> Maybe (Result (List Cli.Decode.ValidationError) ( globalOptions, cliOptions ))
matchResultToMaybe matchResult =
    case matchResult of
        Match thing ->
            Just thing

        NoMatch unknownFlags ->
            Nothing
