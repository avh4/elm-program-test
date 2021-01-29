module ElmSyntaxHelper exposing (removeTypeAnnotationRange, typeAnnotationToString)

{-| from <https://gist.github.com/jfmengels/966c086eb79244ca55cf362fc2dd60cf>
-}

import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation exposing (RecordField, TypeAnnotation(..))


removeTypeAnnotationRange : Node TypeAnnotation -> Node TypeAnnotation
removeTypeAnnotationRange (Node _ value) =
    Node Range.emptyRange
        (case value of
            FunctionTypeAnnotation input output ->
                FunctionTypeAnnotation (removeTypeAnnotationRange input) (removeTypeAnnotationRange output)

            Typed (Node _ nameNode) params ->
                Typed (Node Range.emptyRange nameNode) (List.map removeTypeAnnotationRange params)

            GenericType string ->
                GenericType string

            Unit ->
                Unit

            Tupled nodes ->
                Tupled (List.map removeTypeAnnotationRange nodes)

            Record recordDefinition ->
                Record
                    (List.map
                        (Node.value
                            >> (\( Node _ field, Node _ type_ ) ->
                                    Node Range.emptyRange ( Node Range.emptyRange field, Node Range.emptyRange type_ )
                               )
                        )
                        recordDefinition
                    )

            GenericRecord (Node _ var) (Node _ recordDefinition) ->
                GenericRecord
                    (Node Range.emptyRange var)
                    (Node Range.emptyRange recordDefinition)
        )


typeAnnotationToString : Node TypeAnnotation -> String
typeAnnotationToString node =
    case Node.value node of
        GenericType string ->
            string

        Typed (Node _ ( [], name )) args ->
            String.join " " (name :: List.map typeAnnotationToString args)

        Typed (Node _ ( moduleName, name )) args ->
            String.join " " ((String.join "." moduleName ++ "." ++ name) :: List.map typeAnnotationToString args)

        Unit ->
            "()"

        Tupled items ->
            "( " ++ String.join ", " (List.map typeAnnotationToString items) ++ " )"

        Record fields ->
            "{ " ++ String.join ", " (List.map recordFieldToString fields) ++ " }"

        GenericRecord (Node _ base) (Node _ fields) ->
            "{ " ++ base ++ " | " ++ String.join ", " (List.map recordFieldToString fields) ++ " }"

        FunctionTypeAnnotation left right ->
            "(" ++ typeAnnotationToString left ++ " -> " ++ typeAnnotationToString right ++ ")"


recordFieldToString : Node RecordField -> String
recordFieldToString (Node _ ( Node _ key, value )) =
    key ++ " : " ++ typeAnnotationToString value
