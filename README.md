
# haskell-quantum

## Description

Implementation of the `Quantum` Monad, which promotes an arbitrary data type to a quantum state.

Whenever a value of type `Quantum a` is modified, the modification is propagated to all
possible states of the underlying type.
Each possible state is weighted by a complex amplitude and the individual amplitudes can interfere
when the state undergoes a transformation.
This allows (in theory) the simulation of any discrete quantum process.
 
The `Quantum` Monad is analogous to the `Probability` Monad used for probabalistic programming.
The main difference is in the normalization of states: While using the L1 norm leads to probabalistic states, the L2 norm is responsible for quantum states.
 
Analogously, one can say that this package implements the concept of *quantum programming*.

## Tutorial

The following code defines a `Quantum Int` that is in a superposition of |0> and |1>,
performs a unitary variant of addition and prints the resulting measurement probabilities.
Non-unitary transformations currently lead to a runtime error.

```haskell
import Control.Quantum

state :: Quantum Int
state = quantize [(0, 1 / sqrt 2 ), (1, 1 / sqrt 2)]

-- All quantum transformations have to be unitary (i.e. reversible)
transform :: Quantum Int -> Quantum Int -> Quantum (Int,  Int)
transform qx qy = do
  x <- qx
  y <- qy
  return (x, x + y)

main = do
    print $ measurements $ transform state state
-- [((0,0),0.25),((0,1),0.25),((1,1),0.25),((1,2),0.25)]
```

