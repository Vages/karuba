module Main exposing (..)

import Random exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)
import Dict exposing (fromList)
import Set
import Array
import Generators exposing (meepleGenerator, templeGenerator, tileGenerator)
import Model exposing (Model)
import Css exposing (..)
import Css.Colors


colors =
    [ "yellow", "purple", "brown", "blue" ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getMeepleSpace initialModel )
        , update = update
        , view = view >> toUnstyled
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = GotMeepleSpace Int
    | GotTempleSpace Int
    | GetNextTile
    | GotNextTile ( Int, Array.Array Int )
    | DrawNewStartingPositions
    | ResetTiles
    | UndoLastDraw


getMeepleSpace : Model -> Cmd Msg
getMeepleSpace model =
    Random.generate GotMeepleSpace (meepleGenerator model)


initialTileSet =
    Array.fromList (List.range 1 36)


initialModel : Model
initialModel =
    { lastGeneratedMeepleSpace = 0
    , meepleAndTempleSpaces = []
    , remainingTiles = initialTileSet
    , drawnTiles = []
    }


meepleIntToDisplayCoordinates : Int -> ( Int, Int )
meepleIntToDisplayCoordinates n =
    let
        xPosition =
            max 0 (n - 5)

        yPosition =
            min n 6
    in
        ( 2 + xPosition, 2 + yPosition )


templeIntToDisplayCoordinates : Int -> ( Int, Int )
templeIntToDisplayCoordinates n =
    let
        xPosition =
            min 7 n

        yPosition =
            max (n - 6) 0
    in
        ( 2 + xPosition, 2 + yPosition )


view : Model -> Html Msg
view model =
    let
        temples =
            model.meepleAndTempleSpaces |> List.map .temple

        meeples =
            model.meepleAndTempleSpaces |> List.map .meeple

        drawMeeples =
            (List.map2 (,)
                meeples
                colors
            )
                |> List.map
                    (\( m, c ) ->
                        let
                            ( column, row ) =
                                meepleIntToDisplayCoordinates m
                        in
                            div
                                [ css
                                    [ property "grid-column" ((toString column) ++ " / span 1")
                                    , property "grid-row" ((toString row) ++ " / span 1")
                                    , property "color" c
                                    ]
                                ]
                                [ img
                                    [ src ("assets/noun_1269_" ++ c ++ ".svg")
                                    , css [ property "fill" "red", color Css.Colors.red ]
                                    ]
                                    []
                                ]
                    )

        drawTemples =
            (List.map2 (,)
                temples
                colors
            )
                |> List.map
                    (\( t, c ) ->
                        let
                            ( column, row ) =
                                templeIntToDisplayCoordinates t
                        in
                            div
                                [ css
                                    [ property "grid-column" ((toString column) ++ " / span 1")
                                    , property "grid-row" ((toString row) ++ " / span 1")
                                    , property "color" c
                                    ]
                                ]
                                [ img
                                    [ src ("assets/pyramid_" ++ c ++ ".svg")
                                    , css [ width (pct 100), property "fill" "red", color Css.Colors.red ]
                                    ]
                                    []
                                ]
                    )
    in
        div []
            [ h1 [] [ text "Starting positions" ]
            , div
                []
                (model.meepleAndTempleSpaces
                    |> List.map (\mt -> div [] [ (toString (mt.meeple * 10) ++ " " ++ toString (mt.temple * 10)) |> text ])
                )
            , div
                [ css
                    [ property "display" "grid"
                    , property "grid-template-rows" "40px repeat(7, 40px) 40px"
                    , property "grid-template-columns" "40px repeat(8, 40px) 40px"
                    ]
                ]
                (List.append
                    drawMeeples
                    drawTemples
                )
            , button [ onClick DrawNewStartingPositions ] [ text "Draw new starting positions" ]
            , h1 [] [ text "Number of remaining tiles" ]
            , text (toString (Array.length model.remainingTiles))
            , if (Array.length model.remainingTiles > 0) then
                button [ onClick GetNextTile ] [ text "Get next tile" ]
              else
                text ""
            , button [ onClick UndoLastDraw ] [ text "Undo last draw" ]
            , h1 [] [ text "Drawn tiles" ]
            , ul [] (List.map (\i -> li [] [ text (toString i) ]) model.drawnTiles)
            , if (List.length model.drawnTiles > 0) then
                button [ onClick ResetTiles ] [ text "Reset tiles" ]
              else
                text ""
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMeepleSpace x ->
            let
                newModel =
                    { model | lastGeneratedMeepleSpace = x }
            in
                ( newModel, Random.generate GotTempleSpace (templeGenerator newModel) )

        GotTempleSpace x ->
            let
                templeAndMeeple =
                    { meeple = model.lastGeneratedMeepleSpace, temple = x }

                newMeepleAndTempleSpaces =
                    templeAndMeeple :: model.meepleAndTempleSpaces

                newModel =
                    { model | meepleAndTempleSpaces = newMeepleAndTempleSpaces }

                nextMessage =
                    if List.length newModel.meepleAndTempleSpaces >= 4 then
                        Cmd.none
                    else
                        getMeepleSpace newModel
            in
                ( newModel, nextMessage )

        GetNextTile ->
            ( model, Random.generate GotNextTile (tileGenerator model.remainingTiles) )

        GotNextTile ( tile, remainingTiles ) ->
            ( { model | drawnTiles = [ tile ] ++ model.drawnTiles, remainingTiles = remainingTiles }, Cmd.none )

        DrawNewStartingPositions ->
            let
                newModel =
                    { model | meepleAndTempleSpaces = [] }
            in
                ( newModel, getMeepleSpace newModel )

        ResetTiles ->
            ( { model | remainingTiles = initialTileSet, drawnTiles = [] }, Cmd.none )

        UndoLastDraw ->
            let
                drawnTiles =
                    model.drawnTiles

                newModel =
                    case drawnTiles of
                        h :: t ->
                            { model | remainingTiles = Array.push h model.remainingTiles, drawnTiles = t }

                        _ ->
                            model
            in
                ( newModel, Cmd.none )
