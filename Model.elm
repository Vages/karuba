module Model exposing (Model)

import Array


type alias Model =
    { lastGeneratedMeepleSpace : Int
    , meepleAndTempleSpaces : List { meeple : Int, temple : Int }
    , remainingTiles : Array.Array Int
    , drawnTiles : List Int
    }
