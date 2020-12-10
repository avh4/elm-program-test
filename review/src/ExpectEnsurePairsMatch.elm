module ExpectEnsurePairsMatch exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
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
            -- TODO: build form the actual declarations
            Dict.fromList
                [ ( "Something"
                  , [ Typed (Node emptyRange ( [], "String" )) []
                    ]
                  )
                ]
      }
    )


validateEnsureTypes : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
validateEnsureTypes node context =
    ( case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                functionName =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value

                ensureName =
                    if String.startsWith "ensure" functionName then
                        Just (String.dropLeft 6 functionName)

                    else
                        Nothing
            in
            case ensureName of
                Just name ->
                    case Maybe.map (Node.value << .typeAnnotation << Node.value) function.signature of
                        Just (FunctionTypeAnnotation left right) ->
                            case Dict.get name context.expectFunctionArguments of
                                Just [ first ] ->
                                    if (Node.value <| removeTypeAnnotationRange left) == first then
                                        []

                                    else
                                        [ Rule.error
                                            { message = "ensureSomething should take the same arguments as expectSomething"
                                            , details =
                                                [ "Assuming the type annotation for expectSomething is correct, the type annotation for ensureSomething should be:"
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
