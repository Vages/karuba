module View exposing (view)

import Model exposing (..)
import Html
import Css exposing (..)
import Css.Colors
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (src, css, alt)
import Html.Styled.Events exposing (onClick)
import Array


colors =
    [ "yellow", "purple", "brown", "blue" ]


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
    div []
        [ drawBoard model
        , drawTiles model
        ]


drawTiles model =
    div []
        [ h2 [] [ text "Draw tiles" ]
        , if (Array.length model.remainingTiles > 0) then
            button [ onClick GetNextTile ] [ text "Get next tile" ]
          else
            text ""
        , button [ onClick UndoLastDraw ] [ text "Undo last draw" ]
        , ul [] (List.map (\i -> li [] [ text (toString i) ]) model.drawnTiles)
        , if (List.length model.drawnTiles > 0) then
            button [ onClick ResetTiles ] [ text "Reset tiles" ]
          else
            text ""
        ]


drawBoard model =
    let
        temples =
            model.meepleAndTempleSpaces |> List.map .temple

        meeples =
            model.meepleAndTempleSpaces |> List.map .meeple

        meepleAndTempleSize =
            pct 70

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

                            alignOption =
                                if (column == leftBorderColumn) then
                                    "center"
                                else
                                    "end"

                            justifyOption =
                                if (column == leftBorderColumn) then
                                    "start"
                                else
                                    "center"
                        in
                            img
                                [ src ("assets/meeple_" ++ c ++ ".svg")
                                , alt (c ++ " meeple")
                                , css
                                    [ width meepleAndTempleSize
                                    , property "grid-column" ((toString column) ++ " / span 1")
                                    , property "grid-row" ((toString row) ++ " / span 1")
                                    , property "color" c
                                    , property "align-self" alignOption
                                    , property "justify-self" justifyOption
                                    ]
                                ]
                                []
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

                            alignOption =
                                if (column == rightBorderColumn) then
                                    "center"
                                else
                                    "start"

                            justifyOption =
                                if (column == rightBorderColumn) then
                                    "end"
                                else
                                    "center"
                        in
                            img
                                [ src ("assets/pyramid_" ++ c ++ ".svg")
                                , alt (c ++ " temple")
                                , css
                                    [ width meepleAndTempleSize
                                    , property "grid-column" ((toString column) ++ " / span 1")
                                    , property "grid-row" ((toString row) ++ " / span 1")
                                    , property "color" c
                                    , property "align-self" alignOption
                                    , property "justify-self" justifyOption
                                    ]
                                ]
                                []
                    )

        topBorderRow =
            2

        bottomBorderRow =
            8

        rightBorderColumn =
            9

        leftBorderColumn =
            2

        coordinateFontSize =
            Css.rem 1

        meepleCoordinatePositions =
            let
                bbr =
                    bottomBorderRow

                lbc =
                    leftBorderColumn
            in
                [ ( lbc, 3 ), ( lbc, 4 ), ( lbc, 5 ), ( lbc, 6 ), ( lbc, 7 ), ( 3, bbr ), ( 4, bbr ), ( 5, bbr ), ( 6, bbr ), ( 7, bbr ), ( 8, bbr ), ( 9, 9 ) ]

        templeCoordinatePositions =
            let
                tbr =
                    topBorderRow

                rbc =
                    rightBorderColumn
            in
                [ ( 3, tbr ), ( 4, tbr ), ( 5, tbr ), ( 6, tbr ), ( 7, tbr ), ( 8, tbr ), ( rbc, 3 ), ( rbc, 4 ), ( rbc, 5 ), ( rbc, 6 ), ( rbc, 7 ) ]

        textCoordinateValues =
            List.range 1 11 |> List.map ((*) 10)

        meepleCoordinateMarkers =
            (List.map2 (,) textCoordinateValues meepleCoordinatePositions)
                |> List.map
                    (\( t, ( x, y ) ) ->
                        let
                            alignOption =
                                if (x == leftBorderColumn) then
                                    "center"
                                else
                                    "start"

                            justifyOption =
                                if (x == leftBorderColumn) then
                                    "end"
                                else
                                    "center"
                        in
                            div
                                [ css
                                    [ fontSize coordinateFontSize
                                    , property "grid-column" ((toString x) ++ " / span 1")
                                    , property "grid-row" ((toString y) ++ " / span 1")
                                    , property "align-self" alignOption
                                    , property "justify-self" justifyOption
                                    ]
                                ]
                                [ text (toString t)
                                ]
                    )

        templeCoordinateMarkers =
            (List.map2 (,) textCoordinateValues templeCoordinatePositions)
                |> List.map
                    (\( t, ( x, y ) ) ->
                        let
                            alignOption =
                                if (x == rightBorderColumn) then
                                    "center"
                                else
                                    "end"

                            justifyOption =
                                if (x == rightBorderColumn) then
                                    "start"
                                else
                                    "center"
                        in
                            div
                                [ css
                                    [ fontSize coordinateFontSize
                                    , property "grid-column" ((toString x) ++ " / span 1")
                                    , property "grid-row" ((toString y) ++ " / span 1")
                                    , property "align-self" alignOption
                                    , property "justify-self" justifyOption
                                    ]
                                ]
                                [ text (toString t)
                                ]
                    )
    in
        div []
            [ h1 [] [ text "Karuba!" ]
            , h2 [] [ text "Starting positions" ]
            , div
                [ css
                    [ property "display" "grid"
                    , property "grid-template-rows" "0 repeat(7, 60px) 0"
                    , property "grid-template-columns" "0 repeat(8, 60px) 0"
                    , property "align-items" "center"
                    , property "justify-items" "center"
                    ]
                ]
                (List.concat
                    [ drawMeeples
                    , drawTemples
                    , meepleCoordinateMarkers
                    , templeCoordinateMarkers
                    ]
                )
            , button [ onClick DrawNewStartingPositions ] [ text "Draw new starting positions" ]
            ]
