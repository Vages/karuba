module Model exposing (..)

import Array


type alias Model =
    { lastGeneratedMeepleSpace : Int
    , meepleAndTempleSpaces : List { meeple : Int, temple : Int }
    , remainingTiles : Array.Array Int
    , drawnTiles : List Int
    }


type Msg
    = GotMeepleSpace Int
    | GotTempleSpace Int
    | GetNextTile
    | GotNextTile ( Int, Array.Array Int )
    | DrawNewStartingPositions
    | ResetTiles
    | UndoLastDraw
