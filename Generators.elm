module Generators exposing (templeGenerator, meepleGenerator, tileGenerator)

import Model exposing (Model)
import Random exposing (Generator, int)
import Set
import Array


templeGenerator : Model -> Generator Int
templeGenerator model =
    let
        meepleSpace =
            model.lastGeneratedMeepleSpace

        minTempleSpace =
            max (4 - meepleSpace) 1

        maxTempleSpace =
            min (19 - meepleSpace) 11

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
