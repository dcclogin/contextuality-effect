### Forget Model + Nothing Model + Othing Model

There is some correlation between **existence** and **ignorance**:
- A paper ignored (i.e. inspected by none) remains its full *immediate* existence (no property forgotten).
- As our knowledge of it grows via measurement, its existence necessarily gets blurred (property forgotten).

Hence it adds to the two common ideologies regarding their relationship: 
1. **Cliché materialism**, or objective realism: Things exist independent of our knowledge of them.
2. **Subjective idealism** a là Berkeley: Things exist only insofar as they are known to, or perceived by us.
3. **???**: Things exist insofar as they are **NOT** known or measured or perceived or...

The key is to think of this weird thing (with the type `Paper`) in such a way that:
- our knowledge of it = its own reflective grasp of itself.
- our ignorance of it = its own failure to grasp itself.
- our erroneous grasp = its own erroneous mode.
- ...as if we are a mirror, a necessary mediation of the thing to know itself.
- ...to put it in dialectics, we are a moment of paper's self-realizing/antagonizing process.
- ...hence the gap separates/divides paper and us paradoxically unites/unifies both.
- ...this is what the motto of dialectics, **Two divides into One** means.

```haskell
type Copy = Property -> M Decision
```

As the type reveals, a `Copy` of `Paper` is still much a collection of predicates, but it is in the form of an effectful function, not a static, immediate map. It is hence a *process* that must be initialized via the action of measurement and inspection from reviewers' side.

Meanwhile, this weird thing also serves as a mirror/mediation of our attempt/interest to know.
- recall the suprise 20 questions, the answer starts with **nothing**.
- our action contributes into its constitution and we are blind to it...
- until the global contradiction forced us to speculate on such possibility.

In a strange sense, both side are not external and independent to each other.

From experimenter's perspective, there are 3 dialectical moments:
1. Something is hidden.
2. Nothing is hidden.
3. ...except [nothing is hidden].

At moment 1, we suppose a substantial paper independent of and inaccessible to us: It fully knows itself (with inherent properties and completely constituted), while we are external to it with an observer's safe distance. Our goal is to approach that secret something asymtotically.

At moment 2, after our diligently querying, seeking knowledge of it, and finally encountering inconsistencies and contradictions, we unsuppose the knowledge from it, forced to conclude its lacking intrinsic properties. But our inclusion in the picture we see is still blind to us.

At moment 3, we realizes our own contribution to the inconsistent appearances, which serves as a necessary blind spot. The **something** at moment 1 is precisely our inclusion and the process of querying, which was misrecognized by us to be something substantial and transcendental, a priori.


### TODOs

```
-- criteria for Pass/Fail decisions
-- impossible for <Nothing> to appear to the reviewers
judgeMargin :: Double -> Decision
judgeMargin m = if abs (m - 1.0) < 0.25 then Pass else Fail

judgeFontSize :: Double -> Decision
judgeFontSize fs = if abs (fs - 12.0) < 1.0 then Pass else Fail

judgeNumPages :: Int -> Decision
judgeNumPages np = if np < 20 then Pass else Fail

-- [TODO]: come up with a natural way to express forgetting model
```


1. define and refine the datatype of reviewers
2. define and refine the datatype and constraints of source/paper/reference/copy


### Extra ideas

1. A program, or even an effectful program as hidden variable in `Property -> M Decision`.