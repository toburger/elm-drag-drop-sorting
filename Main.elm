module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, src, width)
import Html5.DragDrop as DragDrop
import Utils


type alias DragId =
    Int


type alias DropId =
    Int


type alias Item =
    { heading : String
    , body : String
    , image : String
    }


type alias Model =
    { items : List Item
    , dragDrop : DragDrop.Model DragId DropId
    }


type Msg
    = DragDropMsg (DragDrop.Msg DragId DropId)


model : Model
model =
    { items =
        List.range 1 10
            |> List.map
                (\x ->
                    Item ("Item #" ++ toString x)
                        "This is the body text"
                        (if x % 2 == 0 then
                            "http://seeklogo.com/images/E/elm-logo-9DEF2A830B-seeklogo.com.png"
                         else
                            "https://www.w3.org/html/logo/downloads/HTML5_Logo_256.png"
                        )
                )
    , dragDrop = DragDrop.init
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
                ({ model
                    | dragDrop = model_
                    , items =
                        case result of
                            Nothing ->
                                model.items

                            Just ( currPos, newPos ) ->
                                Utils.sliceInto (currPos // 2) (newPos // 2) model.items
                 }
                )


viewSourceItem : Item -> List (Attribute Msg) -> Html Msg
viewSourceItem item draggable =
    li
        ([ style
            [ ( "border", "solid 1px black" )
            , ( "padding", "5px" )
            ]
         ]
            ++ draggable
        )
        [ div
            [ style [ ( "pointer-events", "none" ) ] ]
            [ h4 [] [ text item.heading ]
            , p []
                [ text item.body ]
            , img [ src item.image, width 20 ] []
            ]
        ]


viewTargetItem : List (Attribute Msg) -> Html Msg
viewTargetItem droppable =
    li
        ([ style
            [ ( "padding", "10px 5px" )
            , ( "color", "gray" )
            , ( "text-align", "center" )
            , ( "font-style", "italic" )
            ]
         ]
            ++ droppable
        )
        [ text "drop here..." ]


viewNeutralItem : List (Attribute Msg) -> Html Msg
viewNeutralItem droppable =
    li ([ style [ ( "height", "10px" ) ] ] ++ droppable) [ text "" ]


viewContainer : List (Html Msg) -> Html Msg
viewContainer children =
    div []
        [ ul
            [ style
                [ ( "list-style", "none" )
                , ( "margin", "10px" )
                , ( "padding", "0" )
                ]
            ]
            children
        ]


viewDragSource : Maybe dropId -> dragId -> (DragDrop.Msg dragId dropId -> msg) -> (List (Attribute msg) -> Html msg) -> Html msg
viewDragSource dropId dragId msg sourceItem =
    let
        draggable =
            DragDrop.draggable msg dragId
    in
        sourceItem draggable


viewDropTarget : Maybe dropId -> dropId -> (DragDrop.Msg dragId dropId -> msg) -> (List (Attribute msg) -> Html msg) -> (List (Attribute msg) -> Html msg) -> Html msg
viewDropTarget dropId dropId_ msg neutralItem dropItem =
    let
        droppable =
            DragDrop.droppable msg dropId_
    in
        if dropId == Just dropId_ then
            dropItem droppable
        else
            neutralItem droppable


type DragDropElement a
    = Draggable a
    | Droppable


view : Model -> Html Msg
view model =
    let
        dropId =
            DragDrop.getDropId model.dragDrop

        dragId =
            DragDrop.getDragId model.dragDrop

        isDragging =
            dragId /= Nothing
    in
        viewContainer
            (model.items
                |> List.map Draggable
                |> Utils.intersperseInverted Droppable
                |> List.indexedMap
                    (\dragId_ elem ->
                        case elem of
                            Draggable elem ->
                                viewDragSource dropId dragId_ DragDropMsg (viewSourceItem elem)

                            Droppable ->
                                if isDragging then
                                    viewDropTarget dropId dragId_ DragDropMsg viewNeutralItem viewTargetItem
                                else
                                    viewNeutralItem []
                    )
            )


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , update = update
        , view = view
        }
