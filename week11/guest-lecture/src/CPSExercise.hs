module CPSExercise where

-- add two numbers, CPS style
addCPS :: Int -> Int -> (Int -> r) -> r
addCPS x y k = k (x + y)

example :: (Int -> r) -> r
example k =
  addCPS
    3
    4
    ( \sum1 ->
        addCPS
          sum1
          5
          ( \sum2 ->
              k sum2
          )
    )

-- add two numbers, but also record the operation; very baby dumbed down version of what the ad library does for tracking gradients.
addCPSTape :: Int -> Int -> [(String, Int, Int)] -> ([(String, Int, Int)] -> Int -> r) -> r
addCPSTape x y tape k =
  let result = x + y
      tapeNew = tape ++ [("add", x, y)]
   in k tapeNew result

example' :: ([(String, Int, Int)] -> Int -> r) -> r
example' k =
  addCPSTape
    3
    4
    []
    ( \tape1 sum1 ->
        addCPSTape
          sum1
          5
          tape1
          ( \tape2 sum2 ->
              k tape2 sum2
          )
    )

main :: IO ()
main =
  example'
    ( \tape result ->
        do
          print result
          print tape
    )

-- implement subtraction, CPS style
subtractCPS :: Int -> Int -> (Int -> r) -> r
subtractCPS = undefined

-- combine addCPS and subtractCPS to compute (3 + 4) - 2
exampleAddSubtract :: (Int -> r) -> r
exampleAddSubtract k = undefined
