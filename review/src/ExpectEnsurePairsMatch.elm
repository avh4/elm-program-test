module ExpectEnsurePairsMatch exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import ElmSyntaxHelper exposing (removeTypeAnnotationRange, typeAnnotationToString)
import List.Nonempty as Nonempty exposing (Nonempty)
import Review.Rule as Rule exposing (Rule)


{-| Makes sure that `expect*`/`ensure*` function pairs are consistent.
-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "ExpectEnsurePairsMatch" initContext
        |> Rule.withDeclarationListVisitor collectExpectTypes
        |> Rule.withDeclarationEnterVisitor validateEnsureTypes
        |> Rule.fromModuleRuleSchema


type alias Context =
    { expectFunctionArguments : Dict String (List (Node TypeAnnotation))
    }


initContext : Context
initContext =
    { expectFunctionArguments = Dict.empty
    }


collectExpectTypes : List (Node Declaration) -> Context -> ( List never, Context )
collectExpectTypes declarations context =
    ( []
    , { context
        | expectFunctionArguments =
            -- TODO: report when expect functions aren't valid
            List.filterMap
                (\decl ->
                    case getNamedFunctionType "expect" decl of
                        Just ( name, _, Ok args ) ->
                            Just ( name, args )

                        _ ->
                            Nothing
                )
                declarations
                |> Dict.fromList
      }
    )


getNamedFunctionType : String -> Node Declaration -> Maybe ( String, Node String, Result FunctionParseError (List (Node TypeAnnotation)) )
getNamedFunctionType prefix declaration =
    case getFunctionType declaration of
        Just ( functionName, annotation ) ->
            if String.startsWith prefix (Node.value functionName) then
                Just
                    ( String.dropLeft (String.length prefix) (Node.value functionName)
                    , functionName
                    , case Maybe.map (List.reverse << Nonempty.toList) annotation of
                        Just (returnType :: programTest :: args) ->
                            -- TODO: validate that return value is Expectation
                            -- TODO: validate that last arg is ProgramTest msg model effect
                            Ok (List.reverse args)

                        Just _ ->
                            Err NotEnoughArgs

                        Nothing ->
                            Err NoTypeAnnotation
                    )

            else
                Nothing

        Nothing ->
            Nothing


type FunctionParseError
    = NotAFunction
    | NotEnoughArgs
    | NoTypeAnnotation


getFunctionType : Node Declaration -> Maybe ( Node String, Maybe (Nonempty (Node TypeAnnotation)) )
getFunctionType declaration =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            let
                functionName =
                    function.declaration
                        |> Node.value
                        |> .name
            in
            Just
                ( functionName
                , Maybe.map (flattenFunctionType << .typeAnnotation << Node.value) function.signature
                )

        _ ->
            Nothing


flattenFunctionType : Node TypeAnnotation -> Nonempty (Node TypeAnnotation)
flattenFunctionType typeAnnotation =
    case Node.value typeAnnotation of
        FunctionTypeAnnotation left right ->
            Nonempty.cons left (flattenFunctionType right)

        _ ->
            Nonempty.fromElement typeAnnotation


validateEnsureTypes : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
validateEnsureTypes node context =
    ( case getNamedFunctionType "ensure" node of
        Just ( name, functionName, Ok ensureArgs ) ->
            case Dict.get name context.expectFunctionArguments of
                Just expectArgs ->
                    let
                        checkArg ensureArg expectArg =
                            if removeTypeAnnotationRange ensureArg == removeTypeAnnotationRange expectArg then
                                Nothing

                            else
                                Just ensureArg

                        mismatchedArgs =
                            List.map2 checkArg ensureArgs expectArgs
                                |> List.filterMap identity
                    in
                    if List.isEmpty mismatchedArgs then
                        []

                    else
                        [ Rule.error
                            { message = "ensure" ++ name ++ " should take the same arguments as expect" ++ name
                            , details =
                                [ "Assuming the type annotation for expect" ++ name ++ " is correct, the type annotation for ensure" ++ name ++ " should be:"
                                , String.join " -> "
                                    (List.map typeAnnotationToString expectArgs
                                        ++ [ "ProgramTest msg model effect -> ProgramTest msg model effect" ]
                                    )
                                ]
                            }
                            (Range.combine <| List.map Node.range mismatchedArgs)
                        ]

                Nothing ->
                    [ Rule.error
                        { message = "ensure" ++ name ++ " must have a corresponding expect" ++ name ++ " function"
                        , details =
                            [ "The type for expect" ++ name ++ " should be:"
                            , String.join " -> "
                                (List.map typeAnnotationToString ensureArgs
                                    ++ [ "ProgramTest msg model effect -> Expectation" ]
                                )
                            ]
                        }
                        (Node.range functionName)
                    ]

        Just ( name, functionName, Err NoTypeAnnotation ) ->
            [ Rule.error
                { message = Node.value functionName ++ " must have a type annotation"
                , details =
                    [ "Assuming the type annotation for expect" ++ name ++ " is correct, the type annotation for ensure" ++ name ++ " should be:"
                    , "String -> ProgramTest msg model effect -> ProgramTest msg model effect"
                    ]
                }
                (Node.range functionName)
            ]

        _ ->
            -- TODO: report that ensure* must be a function
            []
    , context
    )
