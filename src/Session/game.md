## A Unifying Model (Game)

It is hence possible to reunify monad in programming language theory as computational effects, and monad in Leibniz's *La Monadologie* as a metaphysical atom.

It is quite similar to bitcoin mining scenario, or a client-server model, or MMORPG, as well as John Wheeler's game *Surprise 20 Questions*.  

To simplify, suppose there is a `Judge`, and two `Player`s. A `Player` is going to **mine** (alias: inspect, measure, observe, perceive, render) a **block** (alias: `Pixel`, `Outcome`) with the apparatus accessible `map :: Property -> M Decision`, where a free choice of `Property` is applied to get the `Decision`.

However, the result is first of all a monadic value `M Decision`, instead of a pure map encoding a collection of properties `Property -> Decision`.

Inside the monads `M Decision`, there are interesting computations happening:
- Both monads compute their primary mining, and submit the result `(Property, Decision)` to the `Judge`.
- The `Judge` decides whether to accept or not, according to a set of rules:
    - If accepted, it's all done, and the result is recorded to the "common server", or "common account book", which is shared across all monads.
    - If rejected, it's handled according to the rules. In our reconstructed models, an independent secondary mining is submitted and will be accepted anyway.
- The `Judge` then announces and broadcasts the final result, the data over "common server" or "common account book" co-mined by two monads in a cooperative-competitive fashion.

In our concurrency model, whoever submitted first will be accepted unconditionally, therefore two monads are racing on their first submission, which mimics the bitcoin mining where the one who has stronger computational force has higher chance to win the race.

### Perspective: The `Judge`

The `Judge`'s understanding of the game:
- There is nothing intrinsic and substantial myself. Even there is, I am ignorant of it. Therefore I cannot serve `Player`'s query simply by telling what I know I am.
- The `map :: Property -> M Decision` are split/copies/fork of me, what I disclose myself to `Player` as its interactive screen/interface.
- The goal is to forge the appearance on-the-fly and by demand, to use `Player`'s free choice of `Property` as a mediation, a mirror reflection, to know what myself, or what `Player` thinks I am.

### Perspective: The `Player`

The `Player`'s understanding of the game:
- There is something substantial, hidden, inaccessible, independent, pre-existing and objective on the side of the `Judge`. Le `Judge` est le sujet-supposé-savoir.
- The goal is to mine and reveal those intrinsic properties, via query with the interface accessible to me `map :: Property -> M Decision`.

**Remark**: Everything happening inside the monad is inaccessible, blind spot to `Player`'s, which means it is blind to its own contribution to the picture. In other words, the map is purely observational, extensional, and mere semblance.

There are two key moments:
- The moment global inconsistency is detected is the momement the knowledge of secret is unsupposed from the `Judge`.
- The moment I realize the real "substantial" truth I seek is (casused by) my very act of seeking it, is the moment of traverser la fantaisie.

### Perspective: The Monad

It represents the shared ignorance of both the `Judge` and the `Player`. For the `Judge`, it 

