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
            if (n <= 5) then
                1
            else
                n - 3

        yPosition =
            if (n <= 5) then
                n + 2
            else
                9
    in
        ( xPosition, yPosition )


templeIntToDisplayCoordinates : Int -> ( Int, Int )
templeIntToDisplayCoordinates n =
    let
        xPosition =
            if (n >= 7) then
                10
            else
                n + 2

        yPosition =
            if (n >= 7) then
                n - 4
            else
                1
    in
        ( xPosition, yPosition )


view : Model -> Html Msg
view model =
    div [ css [ fontSize (px 64) ] ]
        [ h1 [ css [ marginTop (px 0), marginBottom (px 0) ] ] [ text "Karuba!" ]
        , viewBoard model
        , viewTiles model
        ]


viewTiles model =
    let
        buttonSize =
            Css.em 0.8

        drawnTilesDisplay =
            case model.drawnTiles of
                h :: t ->
                    ul [] (li [ css [ fontSize (Css.em 1.6) ] ] [ text (toString h) ] :: (List.map (\i -> li [] [ text (toString i) ]) t))

                _ ->
                    text ""

        drawButton =
            if (Array.length model.remainingTiles > 0) then
                button [ css [ fontSize buttonSize ], onClick GetNextTile ] [ text "Get next" ]
            else
                text ""

        undoButton =
            if (List.length model.drawnTiles > 0 && Array.length model.remainingTiles > 0) then
                button [ css [ fontSize buttonSize ], onClick UndoLastDraw ] [ text "Undo" ]
            else
                text ""

        resetButton =
            if (List.length model.drawnTiles > 0) then
                button [ css [ fontSize buttonSize ], onClick ResetTiles ] [ text "Reset" ]
            else
                text ""
    in
        div []
            [ h2
                [ css [ marginTop (Css.em 0.2), marginBottom (px 0) ] ]
                [ text ("Tiles (" ++ (toString <| Array.length model.remainingTiles) ++ ")") ]
            , drawButton
            , undoButton
            , resetButton
            , drawnTilesDisplay
            ]


viewBoard model =
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
                        in
                            img
                                [ src ("assets/meeple_" ++ c ++ ".svg")
                                , alt (c ++ " meeple")
                                , css
                                    [ width meepleAndTempleSize
                                    , property "grid-column" ((toString column) ++ " / span 1")
                                    , property "grid-row" ((toString row) ++ " / span 1")
                                    , property "color" c
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
                        in
                            img
                                [ src ("assets/pyramid_" ++ c ++ ".svg")
                                , alt (c ++ " temple")
                                , css
                                    [ width meepleAndTempleSize
                                    , property "grid-column" ((toString column) ++ " / span 1")
                                    , property "grid-row" ((toString row) ++ " / span 1")
                                    , property "color" c
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
            Css.em 0.7

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
                        div
                            [ css
                                [ fontSize coordinateFontSize
                                , property "grid-column" ((toString x) ++ " / span 1")
                                , property "grid-row" ((toString y) ++ " / span 1")
                                ]
                            ]
                            [ text (toString t)
                            ]
                    )

        templeCoordinateMarkers =
            (List.map2 (,) textCoordinateValues templeCoordinatePositions)
                |> List.map
                    (\( t, ( x, y ) ) ->
                        div
                            [ css
                                [ fontSize coordinateFontSize
                                , property "grid-column" ((toString x) ++ " / span 1")
                                , property "grid-row" ((toString y) ++ " / span 1")
                                ]
                            ]
                            [ text (toString t)
                            ]
                    )
    in
        div []
            [ h2 [ css [ marginTop (px 0), marginBottom (px 0) ] ] [ text "Board" ]
            , div
                [ css
                    [ property "display" "grid"
                    , property "grid-template-rows" "1fr auto repeat(5, 1fr) auto 1fr"
                    , property "grid-template-columns" "1fr auto repeat(6, 1fr) auto 1fr"
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
            , button [ css [ fontSize (Css.em 1) ], onClick DrawNewStartingPositions ] [ text "New board" ]
            ]
