module Main where

import Control.Monad (foldM, forM_)
import Data.List (nub, sort)
import qualified Data.Map as Map
import MicroExercise
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import Text.Printf (printf)

-- Weight flattening/rebuilding (for optimizer updates)

flattenWeights :: ModelWeights -> [Value]
flattenWeights weights = concatMap (concat . snd) (Map.toAscList weights)

weightShapes :: ModelWeights -> [(String, Int, Int)]
weightShapes weights =
  [ (name, length rows, if null rows then 0 else length (head rows))
    | (name, rows) <- Map.toAscList weights
  ]

rebuildWeights :: [(String, Int, Int)] -> [Value] -> ModelWeights
rebuildWeights [] _ = Map.empty
rebuildWeights ((name, numRows, numCols) : rest) params =
  let (these, remaining) = splitAt (numRows * numCols) params
      matrix = chunksOf numCols these
   in Map.insert name matrix (rebuildWeights rest remaining)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest

-- Tokenization (character-level)

tokenize :: String -> Int -> String -> [Int]
tokenize alphabet bosToken document =
  [bosToken] ++ map charToIndex document ++ [bosToken]
  where
    charToIndex c = length (takeWhile (/= c) alphabet)

-- Training

-- Forward pass over all positions in a document, returning per-position losses
processDocument :: Config -> ModelWeights -> [Int] -> Trace [Value]
processDocument cfg weights tokens = do
  let numPositions = min (blockSize cfg) (length tokens - 1)
  (losses, _, _) <-
    foldM
      ( \(losses, keys, vals) position -> do
          let currentToken = tokens !! position
              targetToken = tokens !! (position + 1)
          (logits, keys', vals') <- gpt cfg currentToken position weights keys vals
          loss <- crossEntropyLoss logits targetToken
          return (losses ++ [loss], keys', vals')
      )
      ([], [], [])
      [0 .. numPositions - 1]
  return losses

-- One training step: forward pass, compute loss, update weights with Adam
trainStep ::
  Config ->
  Int ->
  Int ->
  [String] ->
  String ->
  Int ->
  (ModelWeights, [Double], [Double]) ->
  [(String, Int, Int)] ->
  Trace (ModelWeights, [Double], [Double])
trainStep cfg step totalSteps documents alphabet bosToken (weights, momentum, variance) shapes = do
  let document = documents !! (step `mod` length documents)
      tokens = tokenize alphabet bosToken document

  losses <- processDocument cfg weights tokens
  let numPositions = length losses
  lossSum <- foldM addV (head losses) (tail losses)
  numVal <- leaf (fromIntegral numPositions)
  avgLoss <- divV lossSum numVal

  io $ printf "step %4d / %4d | loss %.4f\r" (step + 1) totalSteps (valueData avgLoss)
  io $ hFlush stdout

  let gradients = computeGradients avgLoss
      params = flattenWeights weights
  (newParams, newMom, newVar) <-
    adamOptimizerStep 0.01 0.85 0.99 1e-8 step totalSteps gradients params momentum variance
  let newWeights = rebuildWeights shapes newParams
  return (newWeights, newMom, newVar)

-- Inference: generate new text from the trained model

generate :: Config -> ModelWeights -> String -> Int -> Trace String
generate cfg weights alphabet bosToken =
  go bosToken 0 [] [] []
  where
    go currentToken position keys vals accumulated
      | position >= blockSize cfg = return (reverse accumulated)
      | otherwise = do
          (logits, keys', vals') <- gpt cfg currentToken position weights keys vals
          -- Temperature scaling (temperature = 0.5 makes the distribution sharper)
          scaledLogits <- mapM (\l -> do temp <- leaf 0.5; divV l temp) logits
          probabilities <- softmax scaledLogits
          randomVal <- io $ randomRIO (0.0, 1.0 :: Double)
          let nextToken = sampleFromDistribution randomVal 0 (map valueData probabilities)
          if nextToken == bosToken
            then return (reverse accumulated)
            else go nextToken (position + 1) keys' vals' (alphabet !! nextToken : accumulated)

    sampleFromDistribution _ idx [] = idx
    sampleFromDistribution remaining idx (prob : probs)
      | remaining <= prob = idx
      | otherwise = sampleFromDistribution (remaining - prob) (idx + 1) probs

-- Main: setup, train, and generate

main :: IO ()
main = do
  exists <- doesFileExist "input.txt"
  if not exists
    then error "input.txt not found. Download from: https://raw.githubusercontent.com/karpathy/makemore/988aa59/names.txt"
    else return ()
  content <- readFile "input.txt"

  let documents = filter (not . null) (lines content)
      alphabet = sort $ nub $ concat documents
      bosToken = length alphabet
      cfg =
        Config
          { vocabSize = length alphabet + 1,
            numLayers = 1,
            numHeads = 4,
            embedDim = 16,
            headDim = 4, -- embedDim `div` numHeads
            blockSize = 16
          }
      totalSteps = 1000

  putStrLn $ "num docs: " ++ show (length documents)
  putStrLn $ "vocab size: " ++ show (vocabSize cfg)

  runTrace $ do
    initialWeights <- initializeModel cfg
    let allParams = flattenWeights initialWeights
        shapes = weightShapes initialWeights
        numParams = length allParams
        initialMomentum = replicate numParams 0.0
        initialVariance = replicate numParams 0.0
    io $ putStrLn $ "num params: " ++ show numParams

    -- Training loop
    (trainedWeights, _, _) <-
      foldM
        (\state step -> trainStep cfg step totalSteps documents alphabet bosToken state shapes)
        (initialWeights, initialMomentum, initialVariance)
        [0 .. totalSteps - 1]

    -- Generate samples from the trained model
    io $ putStrLn "\n--- inference (new, hallucinated names) ---"
    forM_ [1 .. 20 :: Int] $ \sampleIndex -> do
      name <- generate cfg trainedWeights alphabet bosToken
      io $ printf "sample %2d: %s\n" sampleIndex name
