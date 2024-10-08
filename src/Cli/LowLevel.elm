module Cli.LowLevel exposing (MatchResult(..), commandHelpText, helpText, try)

import Cli.Decode
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (OptionsParser)
import Cli.OptionsParser.BuilderState as BuilderState
import Cli.OptionsParser.MatchResult as MatchResult exposing (MatchResult)
import Dict exposing (Dict)
import List.Extra
import Set exposing (Set)


type MatchResult globalOptions cliOptions
    = ValidationErrors (List Cli.Decode.ValidationError)
    | NoMatch (List String)
    | Match globalOptions cliOptions
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


type CombinedParser globalOptions cliOptions
    = SystemParser (MatchResult globalOptions cliOptions)
    | UserParser cliOptions


try : OptionsParser.OptionsParser globalOptions globalBuilderState -> List (OptionsParser.OptionsParser cliOptions builderState) -> List String -> MatchResult globalOptions cliOptions
try globalOptionsParser optionsParsers argv =
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
                        |> (\a -> OptionsParser.tryMatch a globalOptionsParser)
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
                            Ok ( globalOptions, msg ) ->
                                case msg of
                                    SystemParser systemMsg ->
                                        systemMsg

                                    UserParser userMsg ->
                                        Match globalOptions userMsg

                            Err validationErrors ->
                                ValidationErrors validationErrors

                    Nothing ->
                        NoMatch commonUnmatchedFlags
           )


helpParser : OptionsParser (MatchResult globalOptions msg) BuilderState.AnyOptions
helpParser =
    OptionsParser.build ShowHelp
        |> OptionsParser.expectFlag "help"


commandHelpParser : OptionsParser (MatchResult globalOptions msg) BuilderState.NoMoreOptions
commandHelpParser =
    OptionsParser.build ShowCommandHelp
        |> OptionsParser.expectFlag "help"
        |> OptionsParser.withRestArgs (Option.restArgs "subCommand")


showVersionParser : OptionsParser (MatchResult globalOptions msg) BuilderState.AnyOptions
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


helpText : String -> String -> String -> Dict String String -> List (OptionsParser msg builderState) -> String
helpText programName version description commandDescriptions optionsParsers =
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
        ++ commandsText commandDescriptions optionsParsers


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


commandsText : Dict String String -> List (OptionsParser msg builderState) -> String
commandsText commandDescriptions optionsParsers =
    optionsParsers
        |> List.map (OptionsParser.getSubCommand >> Maybe.andThen List.head)
        |> List.filterMap identity
        |> List.Extra.unique
        |> List.map (\command -> String.padRight 16 ' ' command ++ (Dict.get command commandDescriptions |> Maybe.withDefault ""))
        |> List.map (String.append "    ")
        |> String.join "\n"
        |> (++) "COMMANDS\n"
