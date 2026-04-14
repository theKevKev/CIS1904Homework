module MicroExercise where

import Control.Monad (foldM, replicateM, zipWithM)
import Data.List (foldl', transpose)
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapS
import qualified Data.Set as Set
import Data.Vector.Unboxed (fromList)
import System.Random.MWC
import System.Random.MWC.Distributions (normal)

-- Model configuration
{- line 75 microgpt.py
# Initialize the parameters, to store the knowledge of the model
n_layer = 1     # depth of the transformer neural network (number of layers)
n_embd = 16     # width of the network (embedding dimension)
block_size = 16 # maximum context length of the attention window (note: the longest name is 15 characters)
n_head = 4      # number of attention heads
head_dim = n_embd // n_head # derived dimension of each head
-}

data Config = Config
  { vocabSize :: Int,
    numLayers :: Int,
    numHeads :: Int,
    headDim :: Int,
    embedDim :: Int,
    blockSize :: Int
  }

-- We need IO for random number generation, and we need state
-- to assign unique IDs to each Value in the computation graph.
{- StateIO is a monad that combines state-passing with IO.

   runStateIO unwraps it: given an initial state s, it runs the
   computation in IO and returns both the result `a` and the updated state `s`.

   Think of it as:  s -> IO (a, s)
     - Takes in a state
     - Does some IO (e.g. random number generation)
     - Returns a result and a (possibly updated) state
-}
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

{- Trace is our specific StateIO where the state is an Int counter.
   Every time we create a new Value node, we increment the counter
   to get a unique ID. This lets us identify nodes in the computation graph.
-}
type Trace = StateIO Int

{- freshID: returns the current counter value and increments it by 1.
   Used internally by addV, mulV, etc. to assign unique IDs to new Values.
-}
freshID :: Trace Int
freshID = StateIO $ \n -> return (n, n + 1)

{- runTrace: runs a Trace computation starting from counter 0.
   Returns only the result (discards the final counter value).
-}
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

-- Primitive operations on Values - see class Value in microgpt.py

{- leaf: creates a Value with no children (a constant or parameter).
-}
leaf :: Double -> Trace Value
leaf x = do i <- freshID; return $ Value i x [] []

{- addV: addition.  result = a + b
-}
addV :: Value -> Value -> Trace Value
addV a b = do
  i <- freshID
  return $ Value i (valueData a + valueData b) [a, b] [1.0, 1.0]

{- mulV: multiplication.  result = a * b
-}
mulV :: Value -> Value -> Trace Value
mulV a b = do
  i <- freshID
  return $ Value i (valueData a * valueData b) [a, b] [valueData b, valueData a]

{- powV: exponentiation by a constant.  result = a^n
-}
powV :: Value -> Double -> Trace Value
powV a n = do
  i <- freshID
  return $ Value i (valueData a ** n) [a] [n * valueData a ** (n - 1)]

{- logV: natural logarithm.  result = ln(a)
-}
logV :: Value -> Trace Value
logV a = do
  i <- freshID
  return $ Value i (log (valueData a)) [a] [1 / valueData a]

{- expV: exponential.  result = e^a
-}
expV :: Value -> Trace Value
expV a = do
  i <- freshID
  let result = exp (valueData a)
  return $ Value i result [a] [result]

{- Exercise 1  

   ReLU is a simple activation function used in neural networks.
   Formula:  relu(a) = max(0, a)

   Steps:
     1. Get a fresh ID:  i <- freshID
     2. Compute the result: max 0 (valueData a)
     3. Record children: [a]
     4. Record local gradient: 1.0 if a > 0, else 0.0
        (gradient passes through unchanged when positive, blocked when negative)

   Return: Value i result [a] [gradient]

   Useful functions: freshID, valueData, max
-}
reluV :: Value -> Trace Value
reluV = undefined

{- negV: negation.  result = -a
-}
negV :: Value -> Trace Value
negV a = do c <- leaf (-1); mulV a c

{- subV: subtraction.  result = a - b
-}
subV :: Value -> Value -> Trace Value
subV a b = do negated <- negV b; addV a negated

{- divV: division.  result = a / b
   Implemented as: raise b to the -1 power, then multiply by a.
-}
divV :: Value -> Value -> Trace Value
divV a b = do reciprocal <- powV b (-1); mulV a reciprocal

-- ============================================================
-- Backpropagation
-- ============================================================


{- Exercise 2 

Topological sort via DFS post-order.
Returns the nodes reachable from 'v' in dependency-first order,
along with the updated visited set.
Key idea: we thread a 'visited' set through the traversal so that
shared nodes (nodes reachable by multiple paths) are only processed once.
For each node:
1. If already visited, return nothing — we've handled it.
2. Otherwise, mark it visited, process all children left-to-right
(passing the visited set from one child to the next), then add the current node at the end (post-order).
-}

topologicalSort :: Set.Set Int -> Value -> ([Value], Set.Set Int)
{-topologicalSort visited v
    | Set.member (valueId v) visited = _todo1 -- what do we return when we already visited the node?
    | otherwise = -- Fold over children, accumulating: 1) the ordered list of nodes so far, 2) the visited set
        let (childTopo, visited') =
              foldl
                (\(acc, vis) child -> _todo2) -- step function: what to do with each child
                _todo3 -- initial accumulator: what do we start with?
                (children v) -- list we're iterating over
         in _todo4  -- where does the current node go?
-}
topologicalSort = undefined

{- computeGradients: reverse-mode automatic differentiation (backpropagation).

   Given the root of a computation graph (e.g. the loss value), computes
   the gradient of the root with respect to every node in the graph.

   Algorithm:
     1. Topologically sort the graph (children before parents)
     2. Initialize the root's gradient to 1.0 (d(root)/d(root) = 1)
     3. Walk the sorted nodes in REVERSE order (parents before children)
     4. For each node, propagate its gradient to its children using
        the chain rule:  childGrad += parentGrad * localGrad

   Returns a Map from valueId -> gradient (Double).
-}
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

{- getGradient: look up the gradient for a specific Value in the gradient map.
   Returns 0 if the Value wasn't reached during backprop.
-}
getGradient :: Value -> MapS.Map Int Double -> Double
getGradient node gradients = MapS.findWithDefault 0 (valueId node) gradients

-- Weight initialization

{- makeGenerator: creates a deterministic random number generator.
   The seed [42] ensures reproducibility — same seed = same "random" weights every time.
-}
makeGenerator :: IO GenIO
makeGenerator = initialize (fromList [42])

{- randomMatrix: creates a numRows x numCols matrix of random Value nodes.
   Each value is drawn from a normal distribution with mean 0 and the given stddev.
   Small stddev (like 0.08) gives small initial weights, which helps training stability.

   Uses nested mapM: outer mapM creates rows, inner mapM creates columns.
   Each element is: draw a random Double from IO, then wrap it as a leaf Value.
-}
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

{- initializeModel: creates all the weight matrices for the transformer.
   The model has three top-level weight matrices:
     - tokenEmbeddings:    vocabSize x embedDim   (one row per token in vocabulary)
     - positionEmbeddings: blockSize x embedDim   (one row per position in sequence)
     - outputProjection:   vocabSize x embedDim   (projects back to vocabulary for prediction)
   Plus per-layer weights created by initializeLayer.

   All weights are stored in a Map keyed by name, so we can look them up later.
-}
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

{- initializeLayer: creates the weight matrices for one transformer layer.
   Each layer has 6 weight matrices:
     - attn_query, attn_key, attn_value, attn_output: for self-attention (all dim x dim)
       (Python: attn_wq, attn_wk, attn_wv, attn_wo)
     - mlp_up:   projects from embedDim to 4*embedDim (expand)   (Python: mlp_fc1)
     - mlp_down: projects from 4*embedDim back to embedDim (compress)  (Python: mlp_fc2)
   Names are prefixed with "layerN." so each layer's weights are distinct in the Map.
-}
initializeLayer :: Int -> Int -> Trace [(String, [[Value]])]
initializeLayer dim layerIndex = do
  attentionWeights <- replicateM 4 (randomMatrix dim dim 0.08)
  let [queryW, keyW, valueW, outputW] = attentionWeights
  upProjection <- randomMatrix (4 * dim) dim 0.08
  downProjection <- randomMatrix dim (4 * dim) 0.08
  let names = ["attn_query", "attn_key", "attn_value", "attn_output", "mlp_up", "mlp_down"]
      weights = [queryW, keyW, valueW, outputW, upProjection, downProjection]
      prefix s = "layer" ++ show layerIndex ++ "." ++ s
  return $ zip (map prefix names) weights

-- ============================================================
-- Neural network building blocks
-- ============================================================

{- Exercise 3: linear (linear layer / matrix-vector multiplication)

   A linear layer multiplies a weight matrix by an input vector.
   Each row of the matrix is dot-producted with the input to produce one output.

   If weights is nOut x nIn and input is length nIn, output is length nOut.

   Steps:
     1. For each row in weights, compute the dot product with input:
          a. Element-wise multiply:  products <- zipWithM mulV row input
          b. Sum the products:       foldM addV (head products) (tail products)
     2. Use mapM to apply this dotProduct to every row

   Structure:
     linear input weights = mapM dotProduct weights
       where
         dotProduct row = do ...

   Useful functions: mapM, zipWithM, foldM, mulV, addV
-}
linear :: [Value] -> [[Value]] -> Trace [Value]
linear = undefined

{- Exercise 4: rmsNorm (Root Mean Square Layer Normalization)

   RMS norm stabilizes training by normalizing each vector to unit RMS length.
   Formula:  rmsNorm(x) = x / sqrt(mean(x^2) + epsilon)

   Steps (all in the Trace monad using do-notation):
     1. Square each element:
     3. Divide by vector length n:  create n with `leaf`, then use divV
     4. Add epsilon (1e-5):         create epsilon with `leaf`, then use addV
     5. Compute scale = s^(-0.5):   use powV
     6. Scale each element:         mapM (mulV scale) input

   Useful functions: mulV, addV, divV, powV, leaf, foldM, mapM, zipWithM
-}
rmsNorm :: [Value] -> Trace [Value]
rmsNorm = undefined

{- Exercise 5: softmax

   Softmax turns raw logits into a probability distribution.
   Formula:  softmax(x_i) = exp(x_i - max(x)) / sum_j(exp(x_j - max(x)))

   The "subtract max" trick prevents overflow in exp().

   Steps:
     1. Find the max value and wrap it as a leaf:
            maxLogit <- leaf $ maximum (map valueData logits)
     2. For each logit, subtract maxLogit then exponentiate:
            exponentials <- mapM (\l -> do shifted <- subV l maxLogit; expV shifted) logits
     3. Sum all the exponentials
     4. Divide each exponential by the sum from step 3

   Useful functions: subV, expV, divV, addV, leaf, foldM, mapM, valueData
-}
softmax :: [Value] -> Trace [Value]
softmax = undefined

-- ============================================================
-- Transformer components
-- ============================================================

{- Exercise 6: embedToken (token + positional embedding)

   In a transformer, each token gets two embeddings added together:
     - Token embedding: which word/character this is  (row `tokenIndex` of "tokenEmbeddings")
     - Position embedding: where it appears in the sequence (row `position` of "positionEmbeddings")

   Steps:
     1. Look up the token embedding row:    (weights Map.! "tokenEmbeddings") !! tokenIndex
     2. Look up the position embedding row: (weights Map.! "positionEmbeddings") !! position
     3. Add them element-wise:              combined <- zipWithM addV tokenEmb positionEmb
     4. Normalize the result:               rmsNorm combined

   Useful functions: Map.!, (!!), zipWithM, addV, rmsNorm
-}
embedToken :: Int -> Int -> ModelWeights -> Trace [Value]
embedToken = undefined

{- slice: extracts a contiguous sub-vector from a list.
   slice start len vec = take `len` elements starting at index `start`.
   Used to split a full embedding vector into per-head chunks for multi-head attention.
   Example: slice 4 4 [a,b,c,d,e,f,g,h] = [e,f,g,h]
-}
slice :: Int -> Int -> [Value] -> [Value]
slice start len vec = take len (drop start vec)

{- Exercise 7: attentionScores (scaled dot-product attention scores)

   Computes attention logits: score(q, K) = (q . K^T) / sqrt(headDim)

   Dividing by sqrt(headDim) keeps the variance stable as dimensions grow.

   Steps:
     1. Create the scale factor:  scaleFactor <- leaf (sqrt (fromIntegral headDimension))
     2. Compute dot products of query with each key vector:
            rawScores <- linear query keys
        (linear already does the dot product of the query against each row of keys)
     3. Divide each score by the scale factor:
            mapM (\s -> divV s scaleFactor) rawScores

   Useful functions: leaf, sqrt, fromIntegral, linear, divV, mapM
-}
attentionScores :: [Value] -> [[Value]] -> Int -> Trace [Value]
attentionScores = undefined

{- weightedSum: weighted combination of value vectors using attention weights.
   After softmax gives us attention weights [w1, w2, ...] and we have
   value vectors [v1, v2, ...], this computes:  result = w1*v1 + w2*v2 + ...

   The transpose is needed because `linear` expects rows to dot-product against,
   but our value vectors are stored as rows (one per position). Transposing
   turns positions-of-dimensions into dimensions-of-positions, so each "row"
   becomes one dimension across all positions — exactly what we want to
   weight-sum over.
-}
weightedSum :: [Value] -> [[Value]] -> Trace [Value]
weightedSum weights values = linear weights (transpose values)

{- singleAttentionHead: one head of multi-head attention.

   The full embedding is split into numHeads chunks. Each head operates
   on its own slice of the query, keys, and values independently.

   Steps:
     1. Compute the offset into the embedding for this head:
            offset = headIndex * headDimension
     2. Slice Q, K, V to just this head's dimensions:
            querySlice  = slice offset headDimension query
            keySlices   = map (slice ...) allKeys     (one slice per position)
            valueSlices = map (slice ...) allValues
     3. Compute attention scores: how relevant is each key to this query?
            scores <- attentionScores querySlice keySlices headDimension
     4. Normalize scores to probabilities:
            attnWeights <- softmax scores
     5. Weighted sum of values using those probabilities:
            weightedSum attnWeights valueSlices
-}
singleAttentionHead :: Int -> Int -> [Value] -> [[Value]] -> [[Value]] -> Trace [Value]
singleAttentionHead headIndex headDimension query allKeys allValues = do
  let offset = headIndex * headDimension
      querySlice = slice offset headDimension query
      keySlices = map (slice offset headDimension) allKeys
      valueSlices = map (slice offset headDimension) allValues
  scores <- attentionScores querySlice keySlices headDimension
  attnWeights <- softmax scores
  weightedSum attnWeights valueSlices

{- multiHeadAttention: runs all attention heads and concatenates their outputs.

   Instead of one big attention, we run numHeads smaller attentions in parallel.
   Each head looks at a different headDim-sized slice of the embedding.
   Concatenating all head outputs reconstructs a full embedDim-sized vector.

   Example with embedDim=16, numHeads=4, headDim=4:
     Head 0 processes dims [0..3],  Head 1 processes dims [4..7],
     Head 2 processes dims [8..11], Head 3 processes dims [12..15]
     Outputs are concatenated back to a 16-dim vector.
-}
multiHeadAttention :: Config -> [Value] -> [[Value]] -> [[Value]] -> Trace [Value]
multiHeadAttention cfg query allKeys allValues = do
  headOutputs <-
    mapM
      (\h -> singleAttentionHead h (headDim cfg) query allKeys allValues)
      [0 .. numHeads cfg - 1]
  return $ concat headOutputs

{- Exercise 8: mlp (multilayer perceptron with residual connection)

   The MLP block in a transformer:
     1. Normalize input:          normalized <- rmsNorm input
     2. Project up (expand):      hidden     <- linear normalized weightsUp     (embedDim -> 4*embedDim)
     3. Apply activation (ReLU):  activated  <- mapM reluV hidden
     4. Project down (compress):  projected  <- linear activated weightsDown    (4*embedDim -> embedDim)
     5. Residual connection:      zipWithM addV input projected
        (adding the original input back helps gradients flow through deep networks!)

   Useful functions: rmsNorm, linear, reluV, addV, mapM, zipWithM
-}
mlp :: [Value] -> [[Value]] -> [[Value]] -> Trace [Value]
mlp = undefined

{- transformerBlock: one full transformer layer (self-attention + MLP).

   This is the core repeating unit of a transformer. Each block:

   Self-attention phase:
     1. Project input into query, key, and value vectors using learned weights
     2. Append this position's key/value to the cache (for attending to past positions)
     3. Run multi-head attention: each position decides what to attend to
     4. Project the attention output back and add residual connection (input + result)

   MLP phase:
     5. Run the multilayer perceptron (with its own residual connection inside)

   The tuple (input, cachedKeys, cachedValues) threads the KV-cache through,
   which lets us process one token at a time during generation.
-}
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
  -- MLP
  afterMLP <- mlp afterAttention (getWeight "mlp_up") (getWeight "mlp_down")
  return (afterMLP, updatedKeys, updatedValues)

{- gpt: the complete forward pass of the transformer.

   Given a single token and its position, produces logits (raw scores)
   over the entire vocabulary for predicting the next token.

   Steps:
     1. Embed the token: look up token + position embeddings, normalize
     2. Pass through each transformer block (foldM chains them sequentially),
        threading the KV-cache through so attention can see previous positions
     3. Project the final hidden state to vocabulary size with outputProjection
        to get logits (one score per possible next token)

   Returns (logits, updatedKeys, updatedValues) so the caller can pass
   the KV-cache to the next position.
-}
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

-- ============================================================
-- Loss and optimization
-- ============================================================

{- Exercise 9: crossEntropyLoss (cross-entropy loss)

   Cross-entropy measures how well our predicted probabilities match the
   true label. Formula:  loss = -log(prob_of_correct_class)

   Steps:
     1. Convert logits to probabilities:  probabilities <- softmax logits
     2. Pick the probability of the target class:  probabilities !! targetIndex
     3. Take the log:  logV
     4. Negate it:      negV    (we minimize loss, and log(p) <= 0)

   Useful functions: softmax, logV, negV, (!!)
-}
crossEntropyLoss :: [Value] -> Int -> Trace Value
crossEntropyLoss = undefined

{- adamOptimizerStep: one step of the Adam optimizer.

   Adam is a popular optimization algorithm that maintains two running averages
   per parameter to adapt the learning rate:
     - momentum (1st moment): smoothed average of gradients (direction)
     - variance (2nd moment): smoothed average of squared gradients (magnitude)

   For each parameter, one update step:
     1. Look up this parameter's gradient from backprop
     2. Update momentum:  newMom = beta1 * oldMom + (1-beta1) * grad
     3. Update variance:  newVar = beta2 * oldVar + (1-beta2) * grad^2
     4. Bias-correct both (they start at 0, so early estimates are too small)
     5. Compute new parameter value:
            param' = param - adjustedLR * correctedMom / (sqrt(correctedVar) + epsilon)

   The learning rate is linearly decayed over training (adjustedLR).
   zip3 bundles each (param, momentum, variance) triple for mapM.
   unzip3 splits the results back into three separate lists.
-}
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
