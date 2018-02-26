module Main exposing (..)

import Random exposing (..)
import Html
import Html.Styled exposing (..)
import Dict exposing (fromList)
import Set
import Array
import Generators exposing (meepleGenerator, templeGenerator, tileGenerator)
import Model exposing (..)
import View exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getMeepleSpace initialModel )
        , update = update
        , view = view >> toUnstyled
        , subscriptions = \_ -> Sub.none
        }


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
