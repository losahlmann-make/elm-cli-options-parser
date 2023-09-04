module Cli.LowLevel exposing (MatchResult(..), commandHelpText, helpText, try)

import Cli.Decode
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (OptionsParser)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.OptionsParser.MatchResult as MatchResult exposing (MatchResult)
import List.Extra
import Set exposing (Set)


type MatchResult msg
    = ValidationErrors (List Cli.Decode.ValidationError)
    | NoMatch (List String)
    | Match msg
    | ShowHelp
    | ShowCommandHelp (List String)
    | ShowVersion


intersection : List (Set comparable) -> Set comparable
intersection sets =
    case sets of
        [] ->
            Set.empty

        [ set ] ->
            set

        first :: rest ->
            intersection rest
                |> Set.intersect first


type CombinedParser userOptions
    = SystemParser (MatchResult userOptions)
    | UserParser userOptions


try : List (OptionsParser.OptionsParser msg builderState) -> List String -> MatchResult msg
try optionsParsers argv =
    let
        matchResults =
            (optionsParsers
                |> List.map (OptionsParser.map UserParser)
                |> List.map OptionsParser.end
            )
                ++ [ helpParser
                        |> OptionsParser.end
                        |> OptionsParser.map SystemParser
                   , commandHelpParser
                        |> OptionsParser.map SystemParser
                   , showVersionParser
                        |> OptionsParser.end
                        |> OptionsParser.map SystemParser
                   ]
                |> List.map
                    (argv
                        |> List.drop 2
                        |> OptionsParser.tryMatch
                    )

        commonUnmatchedFlags =
            matchResults
                |> List.map
                    (\matchResult ->
                        case matchResult of
                            MatchResult.NoMatch unknownFlags ->
                                Set.fromList unknownFlags

                            _ ->
                                Set.empty
                    )
                |> intersection
                |> Set.toList
    in
    matchResults
        |> List.map MatchResult.matchResultToMaybe
        |> oneOf
        |> (\maybeResult ->
                case maybeResult of
                    Just result ->
                        case result of
                            Ok msg ->
                                case msg of
                                    SystemParser systemMsg ->
                                        systemMsg

                                    UserParser userMsg ->
                                        Match userMsg

                            Err validationErrors ->
                                ValidationErrors validationErrors

                    Nothing ->
                        NoMatch commonUnmatchedFlags
           )


helpParser : OptionsParser (MatchResult msg) BuilderState.AnyOptions
helpParser =
    OptionsParser.build ShowHelp
        |> OptionsParser.expectFlag "help"


commandHelpParser : OptionsParser (MatchResult msg) BuilderState.NoMoreOptions
commandHelpParser =
    OptionsParser.build ShowCommandHelp
        |> OptionsParser.expectFlag "help"
        |> OptionsParser.withRestArgs (Option.restArgs "subCommand")


showVersionParser : OptionsParser (MatchResult msg) BuilderState.AnyOptions
showVersionParser =
    OptionsParser.build ShowVersion
        |> OptionsParser.expectFlag "version"


oneOf : List (Maybe a) -> Maybe a
oneOf =
    List.foldl
        (\x acc ->
            if acc /= Nothing then
                acc

            else
                x
        )
        Nothing


helpText : String -> String -> String -> List (OptionsParser msg builderState) -> String
helpText programName version description optionsParsers =
    programName
        ++ " "
        ++ version
        ++ "\n"
        ++ description
        ++ "\n\n"
        ++ usageText programName
        ++ "\n\n"
        ++ flagsText
        ++ "\n"
        ++ commandsText optionsParsers


commandHelpText : String -> List (OptionsParser msg builderState) -> List String -> String
commandHelpText programName optionsParsers subCommand =
    let
        optionsParser =
            optionsParsers
                |> List.Extra.findMap
                    (\optParser ->
                        case OptionsParser.getSubCommand optParser of
                            Just subCmd ->
                                if subCmd == subCommand then
                                    Just optParser

                                else
                                    Nothing

                            Nothing ->
                                Nothing
                    )

        description =
            "Description"
    in
    case optionsParser of
        Just optParser ->
            programName
                ++ " "
                ++ String.join " " subCommand
                ++ "\n"
                ++ (OptionsParser.getDescription optParser
                        |> Maybe.map (\desc -> desc ++ "\n\n")
                        |> Maybe.withDefault "\n"
                   )
                ++ "USAGE\n    "
                ++ OptionsParser.synopsis programName optParser

        Nothing ->
            "No matching optionsParser..."



-- ++ (optionsParsers
--         |> List.map (OptionsParser.synopsis programName)
--         |> String.join "\n"
--    )


usageText : String -> String
usageText programName =
    "USAGE\n    "
        ++ programName
        ++ " <COMMAND> <SUBCOMMAND> [FLAGS]"


flagsText : String
flagsText =
    """FLAGS
    --help      Show help for command
    --version   Show version
    """


commandsText : List (OptionsParser msg builderState) -> String
commandsText optionsParsers =
    optionsParsers
        |> List.map (OptionsParser.getSubCommand >> Maybe.andThen List.head)
        |> List.filterMap identity
        |> List.Extra.unique
        |> List.map (String.append "    ")
        |> String.join "\n"
        |> (++) "COMMANDS\n"
