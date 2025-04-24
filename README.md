# Contextuality Effect

Here we propose a constructive, non-local hidden variable model based on various computational effects with Haskell implementation. It is designed to simulate and account for some "mysterious phenomenons" in quantum physics when it comes to observation and measurement, namely non-locality and contextuality, as revealed by Bell-Kochen-Specker theorem and its loophole-free experiments.

## Mermin's Experiment

To simplify the problem, we resort to a thought experiment proposed by David Mermin in his short article "Quantum Mysteries For Everyone" published in 1981.

The configuration of Mermin's experiment is quite simple:
- A device C from where a bipartite quantum system emerges (2 particles are emitted from C to the opposite directions, left and right).
- Two detectors, A and B, placed to the left and the right side of C respectively, flashing either red `R` or green `G` when a particle arrives.

Each detector has a switch which can be set in one of 3 different positions, so there are in total 6 observables in 2 groups, left and right. Each group has 3 observables: `l1`,`l2`,`l3` for the left group, and `r1`,`r2`,`r3` for the right group.

The measurement scenario (following Abramsky's terminology, see [slides4](https://dcclogin.github.io/notes/slides4.pdf)) for this experiment is:

$` X = \{l_1, l_2, l_3, r_1, r_2, r_3\} `$

$` M = \{\{l_1, r_1\}, \{l_1, r_2\}, \{l_1, r_3\}, \{l_2, r_1\}, \{l_2, r_2\}, \{l_2, r_3\}, \{l_3, r_1\}, \{l_3, r_2\}, \{l_3, r_3\}\} `$

$` O = \{True, False\} `$

The set `X` enumerates all observables involved in the experiment, while the measurement cover `M` lists all possible combinations of compatible observables, and each combination counts as a valid measurement context. The set `O` contains all possible outcomes of a single measurement, and here it is either `True` or `False`.


### Representation of quantum system

Essentially, a quantum system in this experiment is represented as 3 bits. In our haskell implementation, it is a 3-tuple of boolean types:

```haskell
data BoolRG where
    R :: BoolRG
    G :: BoolRG

type ExprRG = (BoolRG, BoolRG, BoolRG)
```

For example, `(R,G,G)`. However, this piece of information is never immediately accessible to the observer. Since we are technically simulating "the process and mechanism of observation", the "properties" of a quantum system must always be mediated by observables and contexts. We will show how observation in contexts "distorts", or "adjusts" these bits.

Therefore, essentialist statements/questions such as "what the quantum state really is (independently and immediately)" only make sense classically, under the "instruction sets" model described in Mermin's article. In fact, the lesson of Bell-Kochen-Specker theorem is precisely that a quantum system has no intrinsic pre-existing properties without specifying how we measure/observe it.

### Representation of observable and context

In textbook quantum mechanics, an observable is represented as a Hermitian matrix whose eigenvalue are possible outcomes. Here we instead represent an observable as a function with computational effect `M`:

```haskell
o :: ExprRG -> M Outcome
```

If we remove the computational effect `M`, the type `ExprRG -> Outcome` can be viewed as a predicate, a question asked by the observer to the system which yields an answer of type `Outcome`. The computational effect `M` opens up the space for "inter-observable communication" which is the key to reproduce contextuality and the expected result of Mermin's experiment.

A (measurement) context in this experiment is simply a pair (2-tuple) of observables:

```haskell
type Observable = ExprRG -> M Outcome
type Context = (Observable, Observable)
```

Intuitively, two observables within a context are "executed" simultaneously (two questions asked simultaneously) and the order should not matter, so in total there are 9 possible contexts as listed above. In order for contextuality to arise, two observables in the same context need to communicate and exchange information through a **hidden channel** before yielding the final outcomes. We implemented 3 variants of such hidden channels with 3 different computational effects, respectively.

(why is this hidden channel non-local? what is local hidden-variable?)


### The experiment and expected output

The experiment has multiple runs (10,000 by defaut). In each run, a random quantum system and a random context are generated independently from each other.

[TODO]


### Variant 0: effect-free

The effect-free variant implements the "instruction sets" model mentioned in Mermin's article. Specifically, each single bit of a quantum system fully dictates the outcome of 2 corresponding observables. For example, consider a quantum system `(x1,x2,x3)`, its first bit `x1` completely determines the outcomes of `l1` and `r1`, regardless of contexts, thus demonstrating non-contextuality.

There is no way to reproduce the results of Mermin's experiment without any effects. (how to prove it?)

### Variant 1: state monad

The most naive, but intuitive enough way is by introduing state monad.

[TODO]

### Variant 2: concurrency

[TODO]

### Variant 3: continuation monad / iterator

[TODO]


## Speculative Ideas

Single observable scenario: 
- self-revealing concealment (nothing is hidden, except "nothing is hidden")
- zero-level discursive exchange: question itself as the answer, receiving my own message in a reversed form
- zero-level economic exchange: giving itself as receiving / gift
- self-reflective move of including the observer's own subjective position into the series of observed phenomena
- "I am always-already in the picture I see in the guise of a blind-spot"
- motto of dialectics "Two divides into One" (not "Two unites into One"!)

Metaphorical concepts:
- sujet suppose savoir

Related:
- Quantum states as programs, observables as tests / static analyzers => Contextuality as bugs, bugs as features!

