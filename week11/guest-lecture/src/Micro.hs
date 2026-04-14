module Micro where

import Control.Monad (foldM, replicateM, zipWithM)
import Data.List (foldl', transpose)
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapS
import qualified Data.Set as Set
import Data.Vector.Unboxed (fromList)
import System.Random.MWC
import System.Random.MWC.Distributions (normal)

-- Model Configuration
data Config = Config
  { vocabSize :: Int,
    numLayers :: Int,
    numHeads :: Int,
    headDim :: Int,
    embedDim :: Int,
    blockSize :: Int
  }

-- StateIO Monad: combines state-passing with IO
-- We need IO for random number generation, and we need state
-- to assign unique IDs to each Value in the computation graph.
-- You can also try refactoring to use this with monad transformers (then you wouldn't have to define all of this boilerplate). Hint: type Trace = StateT Int IO
-- Extra hard challenge - refactor this to use continuation passing style
newtype StateIO s a = StateIO {runStateIO :: s -> IO (a, s)}

instance Functor (StateIO s) where
  fmap f (StateIO g) = StateIO $ \s -> do
    (a, s') <- g s
    return (f a, s')

instance Applicative (StateIO s) where
  pure a = StateIO $ \s -> return (a, s)
  StateIO mf <*> StateIO ma = StateIO $ \s -> do
    (f, s') <- mf s
    (a, s'') <- ma s'
    return (f a, s'')

instance Monad (StateIO s) where
  StateIO g >>= f = StateIO $ \s -> do
    (a, s') <- g s
    runStateIO (f a) s'

-- Lift a plain IO action into StateIO (state passes through unchanged)
io :: IO a -> StateIO s a
io m = StateIO $ \s -> do a <- m; return (a, s)

-- Trace threads an Int counter to give each Value a unique ID
type Trace = StateIO Int

freshID :: Trace Int
freshID = StateIO $ \n -> return (n, n + 1)

runTrace :: Trace a -> IO a
runTrace m = fst <$> runStateIO m 0

-- Computation Graph: Values with automatic differentiation
-- Each Value records its numeric data, a unique ID, its child
-- nodes in the computation graph, and the local gradients
-- (partial derivatives with respect to each child).

data Value = Value
  { valueId :: !Int,
    valueData :: Double,
    children :: [Value],
    localGradients :: [Double]
  }

instance Eq Value where a == b = valueId a == valueId b

instance Ord Value where compare a b = compare (valueId a) (valueId b)

-- Maps parameter names (e.g. "tokenEmbeddings") to 2D weight matrices
type ModelWeights = Map.Map String [[Value]]

-- Primitive operations on Values
-- Each operation creates a new Value node, records its children,
-- and stores the local gradient (partial derivative) for backprop.

leaf :: Double -> Trace Value
leaf x = do i <- freshID; return $ Value i x [] []

addV :: Value -> Value -> Trace Value
addV a b = do
  i <- freshID
  return $ Value i (valueData a + valueData b) [a, b] [1.0, 1.0]

mulV :: Value -> Value -> Trace Value
mulV a b = do
  i <- freshID
  return $ Value i (valueData a * valueData b) [a, b] [valueData b, valueData a]

powV :: Value -> Double -> Trace Value
powV a n = do
  i <- freshID
  return $ Value i (valueData a ** n) [a] [n * valueData a ** (n - 1)]

logV :: Value -> Trace Value
logV a = do
  i <- freshID
  return $ Value i (log (valueData a)) [a] [1 / valueData a]

expV :: Value -> Trace Value
expV a = do
  i <- freshID
  let result = exp (valueData a)
  return $ Value i result [a] [result]

reluV :: Value -> Trace Value
reluV a = do
  i <- freshID
  return $ Value i (max 0 (valueData a)) [a] [if valueData a > 0 then 1.0 else 0.0]

negV :: Value -> Trace Value
negV a = do c <- leaf (-1); mulV a c

subV :: Value -> Value -> Trace Value
subV a b = do negated <- negV b; addV a negated

divV :: Value -> Value -> Trace Value
divV a b = do reciprocal <- powV b (-1); mulV a reciprocal

-- Backpropagation

topologicalSort :: Set.Set Int -> Value -> ([Value], Set.Set Int)
topologicalSort visited v
  | Set.member (valueId v) visited = ([], visited)
  | otherwise =
      let (childTopo, visited') =
            foldl
              (\(acc, vis) child -> let (t, vis') = topologicalSort vis child in (acc ++ t, vis'))
              ([], Set.insert (valueId v) visited)
              (children v)
       in (childTopo ++ [v], visited')

-- Compute all gradients via reverse-mode automatic differentiation
-- foldl' is the non-lazy version of foldl
computeGradients :: Value -> MapS.Map Int Double
computeGradients root =
  let (sortedNodes, _) = topologicalSort Set.empty root
   in foldl' accumulateGradients (MapS.singleton (valueId root) 1.0) (reverse sortedNodes)
  where
    accumulateGradients gradients node =
      let nodeGradient = MapS.findWithDefault 0 (valueId node) gradients
       in foldl'
            ( \grads (child, localGrad) ->
                MapS.insertWith (+) (valueId child) (localGrad * nodeGradient) grads
            )
            gradients
            (zip (children node) (localGradients node))

getGradient :: Value -> MapS.Map Int Double -> Double
getGradient node gradients = MapS.findWithDefault 0 (valueId node) gradients

-- Weight initialization
makeGenerator :: IO GenIO
makeGenerator = initialize (fromList [42])

randomMatrix :: Int -> Int -> Double -> Trace [[Value]]
randomMatrix numRows numCols stddev = do
  generator <- io makeGenerator
  mapM
    ( \_ ->
        mapM
          (\_ -> do x <- io (normal 0 stddev generator); leaf x)
          [1 .. numCols]
    )
    [1 .. numRows]

initializeModel :: Config -> Trace ModelWeights
initializeModel cfg = do
  tokenEmbeddings <- randomMatrix (vocabSize cfg) (embedDim cfg) 0.08 -- (Python: wte)
  positionEmbeddings <- randomMatrix (blockSize cfg) (embedDim cfg) 0.08 -- (Python: wpe)
  outputProjection <- randomMatrix (vocabSize cfg) (embedDim cfg) 0.08 -- (Python: lm_head)
  layers <- mapM (initializeLayer (embedDim cfg)) [0 .. numLayers cfg - 1]
  return $
    Map.fromList $
      [ ("tokenEmbeddings", tokenEmbeddings),
        ("positionEmbeddings", positionEmbeddings),
        ("outputProjection", outputProjection)
      ]
        ++ concat layers

initializeLayer :: Int -> Int -> Trace [(String, [[Value]])]
initializeLayer dim layerIndex = do
  attentionWeights <- replicateM 4 (randomMatrix dim dim 0.08)
  let [queryW, keyW, valueW, outputW] = attentionWeights
  upProjection <- randomMatrix (4 * dim) dim 0.08
  downProjection <- randomMatrix dim (4 * dim) 0.08
  -- (Python: attn_wq, attn_wk, attn_wv, attn_wo, mlp_fc1, mlp_fc2)
  let names = ["attn_query", "attn_key", "attn_value", "attn_output", "mlp_up", "mlp_down"]
      weights = [queryW, keyW, valueW, outputW, upProjection, downProjection]
      prefix s = "layer" ++ show layerIndex ++ "." ++ s
  return $ zip (map prefix names) weights

-- Neural network building blocks

-- Linear layer: matrix-vector multiply (each row dotted with the input)
linear :: [Value] -> [[Value]] -> Trace [Value]
linear input weights = mapM dotProduct weights
  where
    dotProduct row = do
      products <- zipWithM mulV row input
      foldM addV (head products) (tail products)

-- RMS normalization: normalize a vector to unit root-mean-square length
rmsNorm :: [Value] -> Trace [Value]
rmsNorm input = do
  squared <- zipWithM mulV input input
  sumSquared <- foldM addV (head squared) (tail squared)
  count <- leaf (fromIntegral (length input))
  meanSquared <- divV sumSquared count
  epsilon <- leaf 1e-5
  shifted <- addV meanSquared epsilon
  scale <- powV shifted (-0.5)
  mapM (mulV scale) input

-- Numerically stable softmax: converts logits to a probability distribution
softmax :: [Value] -> Trace [Value]
softmax logits = do
  maxLogit <- leaf $ maximum (map valueData logits)
  exponentials <- mapM (\l -> do shifted <- subV l maxLogit; expV shifted) logits
  total <- foldM addV (head exponentials) (tail exponentials)
  mapM (`divV` total) exponentials

-- Transformer components

-- Look up token + position embeddings, then normalize
embedToken :: Int -> Int -> ModelWeights -> Trace [Value]
embedToken tokenIndex position weights = do
  let tokenEmb = (weights Map.! "tokenEmbeddings") !! tokenIndex
      positionEmb = (weights Map.! "positionEmbeddings") !! position
  combined <- zipWithM addV tokenEmb positionEmb
  rmsNorm combined

slice :: Int -> Int -> [Value] -> [Value]
slice start len vec = take len (drop start vec)

-- Scaled dot-product attention: score(q, K) = (q . K^T) / sqrt(headDim)
attentionScores :: [Value] -> [[Value]] -> Int -> Trace [Value]
attentionScores query keys headDimension = do
  scaleFactor <- leaf (sqrt (fromIntegral headDimension))
  rawScores <- linear query keys
  mapM (`divV` scaleFactor) rawScores

-- Weighted combination of value vectors using attention weights
weightedSum :: [Value] -> [[Value]] -> Trace [Value]
weightedSum weights values = linear weights (transpose values)

-- Single attention head: slice Q/K/V to this head's range, score, combine
singleAttentionHead :: Int -> Int -> [Value] -> [[Value]] -> [[Value]] -> Trace [Value]
singleAttentionHead headIndex headDimension query allKeys allValues = do
  let offset = headIndex * headDimension
      querySlice = slice offset headDimension query
      keySlices = map (slice offset headDimension) allKeys
      valueSlices = map (slice offset headDimension) allValues
  scores <- attentionScores querySlice keySlices headDimension
  attnWeights <- softmax scores
  weightedSum attnWeights valueSlices

-- Multi-head attention: run all heads and concatenate their outputs
multiHeadAttention :: Config -> [Value] -> [[Value]] -> [[Value]] -> Trace [Value]
multiHeadAttention cfg query allKeys allValues = do
  headOutputs <-
    mapM
      (\h -> singleAttentionHead h (headDim cfg) query allKeys allValues)
      [0 .. numHeads cfg - 1]
  return $ concat headOutputs

-- Multilayer perceptron with ReLU activation and residual connection
mlp :: [Value] -> [[Value]] -> [[Value]] -> Trace [Value]
mlp input weightsUp weightsDown = do
  normalized <- rmsNorm input
  hidden <- linear normalized weightsUp
  activated <- mapM reluV hidden
  projected <- linear activated weightsDown
  zipWithM addV input projected

-- One full transformer block: self-attention + feed-forward
transformerBlock ::
  Config ->
  ([Value], [[Value]], [[Value]]) ->
  Int ->
  ModelWeights ->
  Trace ([Value], [[Value]], [[Value]])
transformerBlock cfg (input, cachedKeys, cachedValues) layerIndex weights = do
  let getWeight name = weights Map.! ("layer" ++ show layerIndex ++ "." ++ name)
  -- Self-attention
  query <- linear input (getWeight "attn_query")
  key <- linear input (getWeight "attn_key")
  value <- linear input (getWeight "attn_value")
  let updatedKeys = cachedKeys ++ [key]
      updatedValues = cachedValues ++ [value]
  attentionOutput <- multiHeadAttention cfg query updatedKeys updatedValues
  projectedAttention <- linear attentionOutput (getWeight "attn_output")
  afterAttention <- zipWithM addV input projectedAttention
  -- Feed-forward
  afterFF <- mlp afterAttention (getWeight "mlp_up") (getWeight "mlp_down")
  return (afterFF, updatedKeys, updatedValues)

-- Full forward pass: embed token -> transformer blocks -> project to vocab (Python: gpt)
gpt ::
  Config ->
  Int ->
  Int ->
  ModelWeights ->
  [[Value]] ->
  [[Value]] ->
  Trace ([Value], [[Value]], [[Value]])
gpt cfg tokenIndex position weights cachedKeys cachedValues = do
  embedded <- embedToken tokenIndex position weights
  (finalOutput, finalKeys, finalValues) <-
    foldM
      (\acc layerIdx -> transformerBlock cfg acc layerIdx weights)
      (embedded, cachedKeys, cachedValues)
      [0 .. numLayers cfg - 1]
  logits <- linear finalOutput (weights Map.! "outputProjection")
  return (logits, finalKeys, finalValues)

-- Loss and optimization

-- Cross-entropy loss: -log(probability of the correct class)
crossEntropyLoss :: [Value] -> Int -> Trace Value
crossEntropyLoss logits targetIndex = do
  probabilities <- softmax logits
  logProb <- logV (probabilities !! targetIndex)
  negV logProb

-- Adam optimizer: one step of parameter update with momentum
adamOptimizerStep ::
  Double ->
  Double ->
  Double ->
  Double ->
  Int ->
  Int ->
  MapS.Map Int Double ->
  [Value] ->
  [Double] ->
  [Double] ->
  Trace ([Value], [Double], [Double])
adamOptimizerStep learningRate beta1 beta2 epsilon stepNum totalSteps gradients params momentum variance = do
  results <- mapM updateParam (zip3 params momentum variance)
  let (newParams, newMomentum, newVariance) = unzip3 results
  return (newParams, newMomentum, newVariance)
  where
    adjustedLR = learningRate * (1 - fromIntegral stepNum / fromIntegral totalSteps)
    updateParam (param, mom, var) = do
      let grad = getGradient param gradients
          newMom = beta1 * mom + (1 - beta1) * grad
          newVar = beta2 * var + (1 - beta2) * grad ** 2
          correctedMom = newMom / (1 - beta1 ^ (stepNum + 1))
          correctedVar = newVar / (1 - beta2 ^ (stepNum + 1))
          updatedValue = valueData param - adjustedLR * correctedMom / (sqrt correctedVar + epsilon)
      newParam <- leaf updatedValue
      return (newParam, newMom, newVar)
