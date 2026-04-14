# microgpt-cis1904

## Files
- `microgpt.py` - Andrej Karpathy's python implementation of GPT-2. [Accompanying blog post here](https://karpathy.github.io/2026/02/12/microgpt/)
- `src/Main.hs`, `src/Micro.hs` - transcribed Haskell version of `microgpt.py` 
- `src/MainExercise.hs`, `src/MicroExercise.hs` - in class exercises, that work towards the transcribed Haskell version of `microgpt.py`. 
- `src/CPSExercise.hs` - in class exercise on continuation passing style. 

Commands to run the code: 
```
-- python implementation example 
python microgpt.py 

-- build cabal project
cabal build

-- full implementation files 
cabal run micro 

-- exercise files
cabal run micro
cabal run cps-exercise 
```

## Resources 
- [3Brown1Blue Neural Network videos](https://www.youtube.com/watch?v=aircAruvnKk&list=PLZHQObOWTQDNU6R1_67000Dx_ZCJB-3pi)
- [PyTorch Transformer Libraries](https://pytorch.org/hub/huggingface_pytorch-transformers/) 
- [Jax Language](https://docs.jax.dev/en/latest/notebooks/thinking_in_jax.html)
- [ad Haskell library](https://github.com/ekmett/ad) - Popular Haskell library for automatic differentiation.
- [History of Neural Networks](https://en.wikipedia.org/wiki/History_of_artificial_neural_networks)