## A Unifying Model (Game)

It is hence possible to reunify monad in programming language theory as computational effects, and monad in Leibniz's *La Monadologie* as a metaphysical atom.

It is quite similar to bitcoin mining scenario, or a client-server model, or MMORPG. To simplify, suppose there is a `Judge`, and two `Player`s. A `Player` might **mine** (alias: inspect, measure, observe, perceive, render) a **block** (alias: `Pixel`, `Outcome`) with the apparatus `sys :: Property -> M Decision`, where a free choice of `Property` is applied.

Inside the monad `M Decision`, interesting computations happen:
- Both `Player`s compute their primary mining, and submit the result `(Property, Decision)` to the `Judge`.
- The `Judge` decides whether to accept or not, according to a set of rules:
    - If accepted, it's all done, and the result is recorded to the "common server", or "common account book", which is shared across `Player`s, or monads.
    - If rejected, it's handled according to the rules. In our reconstructed models, an independet secondary mining is submitted and will be accepted anyway.
- The `Judge` then announces the final result, the data over "common server" or "common account book" co-mined by two `Player`s in a cooperative-competitive fashion.

In our concurrency model, whoever submitted first will be accepted unconditionally, therefore two `Player`s are racing on their first submission, which mimics the bitcoin mining where the one who has stronger computational force has higher chance to win the race.

