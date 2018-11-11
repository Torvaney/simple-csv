module Main exposing (..)

import Browser
import Csv
import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import List

import Data

import Debug


main =
    Browser.element
    { init   = init
    , update = update
    , view   = view
    , subscriptions = subscriptions
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { data = Data.mtcars
      , mode = Edit
      }
    , Cmd.none
    )


type alias Model =
    { data : Csv.Csv
    , mode : Mode
    }


type Mode
    = Edit
    | Delete


type Msg
    = AddColumn String
    | RemoveColumn Int
    | EditCell Cell
    | ChangeMode Mode


type alias Cell =
    { value : String
    , location : CellLocation
    }


type CellLocation
    = Header Int     -- Column
    | Body Int Int   -- Column, Row


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddColumn name ->
            ( { model
              | data = addColumn model.data name
              }
            , Cmd.none
            )

        EditCell new ->
            ( updateCell new model, Cmd.none )

        ChangeMode mode ->
            ( { model
              | mode = mode
              }
            , Cmd.none
            )

        RemoveColumn ix ->
            ( { model
              | data = removeColumn model.data ix
              }
            , Cmd.none
            )



-- TODO: Test the spreadhseet updating logic
-- Also, consider submitting a PR to lovasoa/elm-csv?

updateCell : Cell -> Model -> Model
updateCell cell model =
    case cell.location of
        Header col ->
            { model
            | data = setHeader model.data <|
                changeAt col cell.value model.data.headers
            }

        Body col row ->
            { model
            | data = setBody model.data <|
                    applyAt col (changeAt row cell.value) model.data.records
            }


setHeader : Csv.Csv -> List String -> Csv.Csv
setHeader data newHeaders =
    { data | headers = newHeaders }


setBody : Csv.Csv -> List (List String) -> Csv.Csv
setBody data newRecords =
    { data | records = newRecords }


addColumn : Csv.Csv -> String -> Csv.Csv
addColumn data name =
    { headers = data.headers ++ [ name ]
    , records = List.map (\x -> x ++ [ "" ]) data.records
    }


removeColumn : Csv.Csv -> Int -> Csv.Csv
removeColumn data ix =
    { headers = removeFromList ix data.headers
    , records = List.map (removeFromList ix) data.records
    }


changeAt : Int -> a -> List a -> List a
changeAt ix val list =
    List.indexedMap
        (\i v -> if (i == ix) then val else v)
        list


applyAt : Int -> (a -> a) -> List a -> List a
applyAt ix f list =
    List.indexedMap
        (\i v -> if (i == ix) then f v else v)
        list


removeFromList : Int -> List a -> List a
removeFromList i xs =
  (List.take i xs) ++ (List.drop (i+1) xs)


-- View
-- TODO: Factorise styles


view model =
    El.layout [ ] <|
        El.column
            [ El.centerX ] -- [ El.centerX, El.spacing 10 ]
            [ El.el
                [ El.centerX
                , El.padding 10
                , Font.size 24
                , Font.bold
                ]
                (El.text "Simple Csv Editor")
            , El.row
                [ El.alignRight, El.padding 10, El.spacing 5 ]
                [ deleteButton model.mode, addColumnButton ]
            , viewSpreadsheet model
            ]


addColumnButton =
    Input.button
        [ Background.color (El.rgb255 240 240 240)
        , Border.color (El.rgb255 220 220 220)
        , Border.rounded 20
        , Border.width 1
        , El.padding 10
        , Font.size 16
        ]
        { onPress = Just (AddColumn "FIXME")
        , label   = El.text "Add column"
        }


deleteButton mode =
    case mode of
        Edit ->
            enterDeleteMode

        Delete ->
            cancelDeleteMode


enterDeleteMode =
    Input.button
        [ Background.color (El.rgb255 240 200 200)
        , Border.color (El.rgb255 200 50 50)
        , Border.rounded 20
        , Border.width 1
        , El.padding 10
        , Font.size 16
        , Font.color (El.rgb255 150 50 50)
        ]
        { onPress = Just (ChangeMode Delete)
        , label   = El.text "Delete"
        }


cancelDeleteMode =
    Input.button
        [ Background.color (El.rgb255 150 50 50)
        , Border.color (El.rgb255 150 50 50)
        , Border.rounded 20
        , Border.width 1
        , El.padding 10
        , Font.size 16
        , Font.color (El.rgb255 240 200 200)
        ]
        { onPress = Just (ChangeMode Edit)
        , label   = El.text "Cancel"
        }


viewSpreadsheet model =
    El.column []
        [ El.row
            [ El.centerX, El.spacing 15 ]
            [ viewHeader model.mode model.data.headers ]
        , El.row
            [ El.centerX ]
            [ viewCells model.data.records ]
        ]


viewHeader mode headers =
    El.row
        [ El.padding 0
        , El.spacing 0
        ] <|
        List.indexedMap (viewHeaderCell mode) headers


viewCells records =
    El.column
        [ El.spacing 0 ] <|
        List.indexedMap viewRow records


viewRow ix record =
    El.row
        [ El.spacing 0
        , El.centerX
        , El.centerY
        ] <|
        List.indexedMap (viewBodyCell ix) record


viewHeaderCell mode colIx value =
    case mode of
        Edit ->
            viewCell
                (El.rgb255 240 240 240)
                { value = value
                , location = Header colIx
                }

        Delete ->
            viewCellButton
                (RemoveColumn colIx)
                { value = value
                , location = Header colIx
                }


viewBodyCell colIx rowIx value =
    viewCell
        (El.rgb255 255 255 255)
        { value = value
        , location = Body colIx rowIx
        }


viewCell fill cell =
    Input.text
        [ El.padding 10
        , El.spacing 0
        , El.width <| El.px 80
        , El.height<| El.px 30
        , Background.color fill
        , Border.width 1
        , Border.color (El.rgb255 220 220 220)
        , Font.size 12
        , Font.family
            [ Font.typeface "Monaco"
            , Font.monospace
            ]
        ]
        { onChange    = \s -> EditCell { cell | value = s}
        , text        = cell.value
        , placeholder = Nothing
        , label       = Input.labelHidden cell.value
        }


viewCellButton msg cell =
    Input.button
        [ El.padding 10
        , El.spacing 0
        , El.width <| El.px 80
        , El.height<| El.px 30
        , Background.color (El.rgb255 150 50 50)
        , Border.rounded 2
        , Border.width 1
        , Font.size 12
        , Font.family
            [ Font.typeface "Monaco"
            , Font.monospace
            ]
        ]
        { onPress = Just msg
        , label   = El.text cell.value
        }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
     Sub.none
