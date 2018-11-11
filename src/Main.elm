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
    ( { data = Data.mtcars }
    , Cmd.none
    )


type alias Model =
    { data : Csv.Csv
    }


type Msg
    = AddColumn
    | EditCell CellInfo


type alias CellInfo =
    { value : String
    , location : CellLocation
    }


type CellLocation
    = Header Int     -- Column
    | Body Int Int   -- Column, Row


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddColumn ->
            ( model, Cmd.none )

        EditCell new ->
            Debug.log new.value
            ( updateCell new model, Cmd.none )


-- TODO: Test the spreadhseet updating logic
-- Also, consider submitting a PR to lovasoa/elm-csv?

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


-- View


view model =
    El.layout [ ] <|
        El.column
            [ El.centerX ] -- [ El.centerX, El.spacing 10 ]
            [ El.el
                [ El.centerX, El.padding 10 ]
                (El.text "Simple Csv Editor")
            , viewSpreadsheet model
            ]


viewSpreadsheet model =
    El.column []
        [ El.row
            [ El.centerX, El.spacing 15 ]
            [ viewHeader model.data.headers ]
        , El.row
            [ El.centerX ]
            [ viewCells model.data.records ]
        ]


viewHeader headers =
    El.row
        [ El.padding 0
        , El.spacing 0
        ] <|
        List.indexedMap viewHeaderCell headers


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


viewHeaderCell colIx value =
    viewCell
        (El.rgb255 240 240 240)
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


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
     Sub.none
