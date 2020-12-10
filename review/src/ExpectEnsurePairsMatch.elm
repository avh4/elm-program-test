module ExpectEnsurePairsMatch exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Review.Rule as Rule exposing (Rule)


{-| Makes sure that `expect*`/`ensure*` function pairs are consistent.
-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "ExpectEnsurePairsMatch" initContext
        |> Rule.withDeclarationEnterVisitor visitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    {}


initContext : Context
initContext =
    {}


visitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
visitor node context =
    ( case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                functionName =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            -- TODO: only check the prefix
            if functionName == "ensureSomething" then
                case Maybe.map (Node.value << .typeAnnotation << Node.value) function.signature of
                    Just (FunctionTypeAnnotation left right) ->
                        -- TODO: work correctly when there's more than one initial argument
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
                        -- TODO: report that ensure* must have type annotation
                        -- TODO: report that ensure* must be a function
                        []

            else
                []

        _ ->
            []
    , context
    )
