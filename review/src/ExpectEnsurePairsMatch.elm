module ExpectEnsurePairsMatch exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import ElmSyntaxHelper exposing (removeTypeAnnotationRange)
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
    { expectFunctionArguments : Dict String (List TypeAnnotation)
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
            List.filterMap (getNamedFunctionType "expect") declarations
                |> Dict.fromList
      }
    )


getNamedFunctionType : String -> Node Declaration -> Maybe ( String, List TypeAnnotation )
getNamedFunctionType prefix declaration =
    case getFunctionType declaration of
        Ok ( functionName, args ) ->
            if String.startsWith prefix functionName then
                Just
                    ( String.dropLeft (String.length prefix) functionName
                    , List.take 1 args
                      -- TODO: validate that return value is Expectation
                      -- TODO: validate that last arg is ProgramTest msg model effect
                      -- TODO: work correctly when there is more than one initial arg
                    )

            else
                Nothing

        _ ->
            -- TODO: report if there was no type annotation
            Nothing


type FunctionParseError
    = NotAFunction
    | NoTypeAnnotation


getFunctionType : Node Declaration -> Result FunctionParseError ( String, List TypeAnnotation )
getFunctionType declaration =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            let
                functionName =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            case Maybe.map (.typeAnnotation << Node.value) function.signature of
                Just annotation ->
                    Ok
                        ( functionName
                        , flattenFunctionType annotation
                        )

                _ ->
                    Err NoTypeAnnotation

        _ ->
            Err NotAFunction


flattenFunctionType : Node TypeAnnotation -> List TypeAnnotation
flattenFunctionType typeAnnotation =
    case Node.value <| removeTypeAnnotationRange typeAnnotation of
        FunctionTypeAnnotation left right ->
            -- TODO: recurse into right
            [ Node.value left, Node.value right ]

        other ->
            [ other ]


validateEnsureTypes : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
validateEnsureTypes node context =
    ( case Node.value node of
        Declaration.FunctionDeclaration function ->
            case getNamedFunctionType "ensure" node of
                Just ( name, actualArgs ) ->
                    case Maybe.map (Node.value << .typeAnnotation << Node.value) function.signature of
                        Just (FunctionTypeAnnotation left right) ->
                            case Dict.get name context.expectFunctionArguments of
                                Just [ first ] ->
                                    if (Node.value <| removeTypeAnnotationRange left) == first then
                                        []

                                    else
                                        [ Rule.error
                                            { message = "ensure" ++ name ++ " should take the same arguments as expect" ++ name
                                            , details =
                                                [ "Assuming the type annotation for expect" ++ name ++ " is correct, the type annotation for ensure" ++ name ++ " should be:"
                                                , "String -> ProgramTest msg model effect -> ProgramTest msg model effect"
                                                ]
                                            }
                                            (Node.range left)
                                        ]

                                _ ->
                                    -- TODO: work correctly when there's more than one initial argument
                                    -- TODO: report when the corresponding expect function doesn't exist
                                    []

                        _ ->
                            -- TODO: report that ensure* must have type annotation
                            -- TODO: report that ensure* must be a function
                            []

                _ ->
                    []

        _ ->
            []
    , context
    )
