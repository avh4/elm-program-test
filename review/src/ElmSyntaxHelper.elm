module ElmSyntaxHelper exposing (removeTypeAnnotationRange)

{-| from <https://gist.github.com/jfmengels/966c086eb79244ca55cf362fc2dd60cf>
-}

import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))


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
