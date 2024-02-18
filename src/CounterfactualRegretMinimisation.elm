module CounterfactualRegretMinimisation exposing (..)

zeroSum : a -> Float -> (a -> Float)
zeroSum ref payoff = \player ->
  if player == ref then
    payoff
  else
    -1 * payoff

type Game player hist a b
  = Chance
      (List a)
      ((hist, a) -> hist)
      ((hist, a) -> hist)
      (a -> Game player hist b a)
  | Decision
      player
      (List a)
      ((hist, a) -> hist)
      ((hist, a) -> hist)
      (a -> Game player hist a b)
  | Terminal
      (player -> Float)


