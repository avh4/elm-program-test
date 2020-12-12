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
            -- TODO: report when expect functions aren't valid
            List.filterMap
                (\decl ->
                    case getNamedFunctionType "expect" decl of
                        Just ( name, Ok args ) ->
                            Just ( name, args )

                        _ ->
                            Nothing
                )
                declarations
                |> Dict.fromList
      }
    )


getNamedFunctionType : String -> Node Declaration -> Maybe ( String, Result FunctionParseError (List TypeAnnotation) )
getNamedFunctionType prefix declaration =
    case getFunctionType declaration of
        Just ( functionName, annotation ) ->
            if String.startsWith prefix (Node.value functionName) then
                Just
                    ( String.dropLeft (String.length prefix) (Node.value functionName)
                    , case annotation of
                        Just args ->
                            -- TODO: validate that return value is Expectation
                            -- TODO: validate that last arg is ProgramTest msg model effect
                            -- TODO: work correctly when there is more than one initial arg
                            Ok (List.take 1 args)

                        Nothing ->
                            Err (NoTypeAnnotation functionName)
                    )

            else
                Nothing

        Nothing ->
            Nothing


type FunctionParseError
    = NotAFunction
    | NoTypeAnnotation (Node String)


getFunctionType : Node Declaration -> Maybe ( Node String, Maybe (List TypeAnnotation) )
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
                Just ( name, Ok actualArgs ) ->
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
                            -- TODO: report that ensure* must be a function
                            []

                Just ( name, Err (NoTypeAnnotation functionName) ) ->
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
                    []

        _ ->
            []
    , context
    )
