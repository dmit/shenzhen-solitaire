module Main

import Data.Fin
import Data.Vect

data Color = Green | Red | Blue
allColors : Vect 3 Color
allColors = [Green, Red, Blue]

data Value = MkValue (Fin 9)
allValues : Vect 9 Value
allValues = map MkValue range

data Card = Flower
          | Number Color Value
          | Dragon Color
Show Card where
  show Flower = "FF"

  show (Number c v) = color c ++ value v
    where
      color : Color -> String
      color Green = "G"
      color Red   = "R"
      color Blue  = "B"

      value : Value -> String
      value (MkValue n) = show $ (toIntegerNat $ finToNat n) + 1

  show (Dragon Green) = "GD"
  show (Dragon Red  ) = "RD"
  show (Dragon Blue ) = "BD"

allCards : Vect 40 Card
allCards = Flower :: (numbers ++ dragons)
  where
    cycle : (n : Nat) -> Vect m a -> Vect (n*m) a
    cycle n v = concat $ replicate n v

    combine : (a -> b -> c) -> Vect n a -> Vect m b -> Vect (n*m) c
    combine {n} {m} f as bs = zipWith f as' bs'
      where
        as' = concat $ map (replicate m) as
        bs' = concat $ replicate n bs

    numbers : Vect 27 Card
    numbers = combine Number allColors allValues

    dragons : Vect 12 Card
    dragons = cycle 4 $ map Dragon allColors

main : IO ()
main = do
  putStrLn $ unwords $ map show $ toList allCards
