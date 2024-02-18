module KuhnPoker exposing (main)

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
  = Bet
  | Pass

type Info
  = InfoCard Int
  | InfoAction Player Action

kuhnPoker : Game Player (List Info) (Int, Int) Action
kuhnPoker =
  Chance
    [(1,2), (1,3), (2,1), (2,3), (3,1), (3,2)]
    (\(hist1, (card1, _)) -> hist1 ++ [InfoCard card1])
    (\(hist2, (_, card2)) -> hist2 ++ [InfoCard card2])
    (\(card1, card2) ->
      Decision
        Player1
        [Bet, Pass]
        (\(hist1, action1) -> hist1 ++ [InfoAction Player1 action1])
        (\(hist2, action1) -> hist2 ++ [InfoAction Player1 action1])
        (\action1 ->
          Decision
            Player2
            [Bet, Pass]
            (\(hist1, action2) -> hist1 ++ [InfoAction Player2 action2])
            (\(hist2, action2) -> hist2 ++ [InfoAction Player2 action2])
            (\action2 ->
              case (action1, action2) of
                (Pass, Pass) ->
                  if card1 > card2 then
                    Terminal (zeroSum Player1 1)
                  else
                    Terminal (zeroSum Player2 1)
                (Bet, Bet) ->
                  if card1 > card2 then
                    Terminal (zeroSum Player1 2)
                  else
                    Terminal (zeroSum Player2 2)
                (Bet, Pass) ->
                  Terminal (zeroSum Player1 1)
                (Pass, Bet) ->
                  Decision
                    Player1
                    [Bet, Pass]
                    (\(hist1, action3) -> hist1 ++ [InfoAction Player1 action3])
                    (\(hist2, action3) -> hist2 ++ [InfoAction Player1 action3])
                    (\action3 ->
                      case action3 of
                        Pass ->
                          Terminal (zeroSum Player1 1)
                        Bet ->
                          if card1 > card2 then
                            Terminal (zeroSum Player1 2)
                          else
                            Terminal (zeroSum Player2 2)
                    )
            )
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
view _ = text "Kuhn poker!"

