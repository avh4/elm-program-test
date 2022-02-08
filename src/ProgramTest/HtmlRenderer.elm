module ProgramTest.HtmlRenderer exposing (render)

import Html.Parser exposing (Node(..))


render : Int -> List Html.Parser.Node -> String
render indent nodes =
    case nodes of
        [] ->
            ""

        (Text text) :: rest ->
            case String.trim (String.replace "\n" " " text) of
                "" ->
                    render indent rest

                trimmed ->
                    String.repeat indent " " ++ trimmed ++ "\n" ++ render indent rest

        (Comment text) :: rest ->
            String.repeat indent " " ++ "<!--" ++ text ++ "-->\n" ++ render indent rest

        (Element tag attrs children) :: rest ->
            String.repeat indent " "
                ++ "<"
                ++ tag
                ++ renderAttrs attrs
                ++ ">\n"
                ++ render (indent + 4) children
                ++ String.repeat indent " "
                ++ "</"
                ++ tag
                ++ ">\n"
                ++ render indent rest


renderAttrs : List Html.Parser.Attribute -> String
renderAttrs attrs =
    case attrs of
        [] ->
            ""

        some ->
            " " ++ String.join " " (List.map renderAttr some)


renderAttr : ( String, String ) -> String
renderAttr ( name, value ) =
    case ( name, value ) of
        ( "htmlfor", _ ) ->
            "for=\"" ++ value ++ "\""

        ( _, "true" ) ->
            name ++ "=true"

        ( _, "false" ) ->
            name ++ "=false"

        _ ->
            name ++ "=\"" ++ value ++ "\""
