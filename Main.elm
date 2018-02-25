module Main exposing (..)

import Random exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Dict exposing (fromList)
import Set
import Array
import Generators exposing (meepleGenerator, templeGenerator, tileGenerator)
import Model exposing (Model)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getMeepleSpace initialModel )
        , update = update
        , view = view
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


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Starting positions" ]
        , div
            []
            (model.meepleAndTempleSpaces
                |> List.map (\mt -> div [] [ (toString (mt.meeple * 10) ++ " " ++ toString (mt.temple * 10)) |> text ])
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
                ( newModel, Random.generate GotTempleSpace (templeGenerator model) )

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
