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
    = AddCell
    | EditCell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCell ->
            ( model, Cmd.none )

        option2 ->
            ( model, Cmd.none )


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
        List.map viewHeaderCell headers


viewCells records =
    El.column
        [ El.spacing 0 ] <|
        List.map viewRow records


viewRow record =
    El.row
        [ El.spacing 0
        , El.centerX
        , El.centerY
        ] <|
        List.map viewValueCell record


viewHeaderCell =
    viewCell (El.rgb255 240 240 240)


viewValueCell =
    viewCell (El.rgb255 255 255 255)


viewCell fill cell =
    El.el
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
        ] <|
        El.text cell


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
     Sub.none
