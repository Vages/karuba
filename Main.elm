module Main exposing (..)

import Random exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Dict exposing (fromList)
import Set
import Array


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getMeepleSpace initialModel )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


getMeepleSpace : Model -> Cmd Msg
getMeepleSpace model =
    Random.generate GotMeepleSpace (meepleGenerator model)


type alias Model =
    { lastGeneratedMeepleSpace : Int
    , meepleAndTempleSpaces : List { meeple : Int, temple : Int }
    , remainingTiles : Array.Array Int
    , pickedTiles : List Int
    }


initialModel : Model
initialModel =
    { lastGeneratedMeepleSpace = 0
    , meepleAndTempleSpaces = []
    , remainingTiles = Array.fromList (List.range 1 36)
    , pickedTiles = []
    }


type Msg
    = GotMeepleSpace Int
    | GotTempleSpace Int
    | GetNextTile
    | GotNextTile ( Int, Array.Array Int )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Starting positions" ]
        , div
            []
            (model.meepleAndTempleSpaces
                |> List.map (\mt -> div [] [ (toString (mt.meeple * 10) ++ " " ++ toString (mt.temple * 10)) |> text ])
            )
        , h1 [] [ text "Number of remaining tiles" ]
        , text (toString (Array.length model.remainingTiles))
        , if (Array.length model.remainingTiles > 0) then
            button [ onClick GetNextTile ] [ text "Get next tile" ]
          else
            text ""
        , h1 [] [ text "Drawn tiles" ]
        , ul [] (List.map (\i -> li [] [ text (toString i) ]) model.pickedTiles)
        ]


templeGenerator : Model -> Generator Int
templeGenerator model =
    let
        meepleSpace =
            model.lastGeneratedMeepleSpace

        minTempleSpace =
            max (4 - meepleSpace) 1

        maxTempleSpace =
            min (20 - meepleSpace) 11

        occupiedTempleSpaces =
            model.meepleAndTempleSpaces |> List.map .temple
    in
        randomIntWithExceptionList minTempleSpace maxTempleSpace occupiedTempleSpaces


meepleGenerator : Model -> Generator Int
meepleGenerator model =
    let
        occupiedMeepleSpaces =
            model.meepleAndTempleSpaces |> List.map .meeple
    in
        randomIntWithExceptionList 1 11 occupiedMeepleSpaces


randomIntWithExceptionList : Int -> Int -> List Int -> Generator Int
randomIntWithExceptionList inclusiveFrom inclusiveTo exceptionList =
    let
        exceptionSet =
            exceptionList |> Set.fromList

        availableInts =
            (List.range inclusiveFrom inclusiveTo)
                |> List.filter (\s -> not (Set.member s exceptionSet))
                |> Array.fromList

        maxArrayIndex =
            (Array.length availableInts) - 1

        getIntAtIndex i =
            Maybe.withDefault -1 (Array.get i availableInts)
    in
        Random.map getIntAtIndex (int 0 maxArrayIndex)


tileGenerator : Array.Array Int -> Generator ( Int, Array.Array Int )
tileGenerator someArray =
    let
        index =
            int 0 ((Array.length someArray) - 1)

        getIntAtIndex i =
            Maybe.withDefault -1 (Array.get i someArray)

        removeItemAtIndex i =
            Array.append (Array.slice 0 i someArray) (Array.slice (i + 1) (Array.length someArray) someArray)
    in
        Random.map (\i -> ( getIntAtIndex i, removeItemAtIndex i )) index


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
            ( { model | pickedTiles = [ tile ] ++ model.pickedTiles, remainingTiles = remainingTiles }, Cmd.none )
