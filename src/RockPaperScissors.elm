module RockPaperScissors exposing (main)

import Browser
import Html exposing (..)

import CounterfactualRegretMinimisation exposing (..)

------------------------------------------------------------------------------
-- Game Definition
------------------------------------------------------------------------------

type Player
  = Player1
  | Player2

type Action
  = Rock
  | Paper
  | Scissors

type Info
  = InfoAction Action

rockPaperScissors : Game Player (List Info) Action b
rockPaperScissors =
  Decision
    Player1
    [Rock, Paper, Scissors]
    (\(hist1, action) -> hist1 ++ [InfoAction action])
    (\(hist2, _) -> hist2 ++ [])
    (\action ->
      Decision
        Player2
        [Rock, Paper, Scissors]
        (\(hist1, _) -> hist1 ++ [])
        (\(hist2, action2) -> hist2 ++ [InfoAction action2])
        (\action2 ->
          case action of
            Rock ->
              case action2 of
                Rock ->
                  Terminal (zeroSum Player1 0)
                Paper ->
                  Terminal (zeroSum Player2 1)
                Scissors ->
                  Terminal (zeroSum Player1 1)
            Paper ->
              case action2 of
                Rock ->
                  Terminal (zeroSum Player1 1)
                Paper ->
                  Terminal (zeroSum Player1 0)
                Scissors ->
                  Terminal (zeroSum Player2 1)
            Scissors ->
              case action2 of
                Rock ->
                  Terminal (zeroSum Player2 1)
                Paper ->
                  Terminal (zeroSum Player1 1)
                Scissors ->
                  Terminal (zeroSum Player1 0)
        )
    )


------------------------------------------------------------------------------
-- Application
------------------------------------------------------------------------------

main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }

init = 0
update _ _ = 0
view _ = text "Rock paper scissors!"
