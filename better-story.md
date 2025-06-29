Yes, that’s a great idea.

Including a compressed, analogy-based reference to the Mermin square can enrich the argument—as long as it’s presented accessibly, without assuming the reader knows quantum mechanics. The key is to highlight the structure of the paradox (conflicting constraints on consistent truth assignments) without invoking unfamiliar quantum formalism.

Here’s a revision of the introduction that does this. It keeps the POPL audience in mind: they appreciate structured paradoxes, abstract reasoning, and the computational implications—but not physics jargon.


Why is paper reviewing quantum? Not because it’s probabilistic, obscure, or occasionally unintelligible—though all of these may be true—but because it exhibits contextuality: the property that an outcome can depend on what else is being asked. In classical logic, truth is context-free: propositions have fixed values regardless of how or when they are evaluated. In contextual systems, by contrast, answers may vary with the surrounding evaluation context. The Kochen–Specker theorem formalizes this phenomenon in quantum mechanics, proving that no globally consistent assignment of truth values can explain all observable results.

But contextuality is not exclusive to physics. Consider the more mundane setting of reviewing papers. At a programming languages conference, each paper is evaluated along three axes: (Q1) Is the paper technically sound? (Q2) Is the contribution significant? (Q3) Is the writing clear? These seem like independent yes/no questions—or so we like to believe.

In reality, they interact. A reviewer’s perception of significance (Q2) may rise or fall with their judgment of technical soundness (Q1). The clarity of writing (Q3) may depend on how many difficult papers they’ve already reviewed. Judgments are shaped by context.

Now imagine we run a study: for a set of papers, we collect yes/no answers to Q1, Q2, and Q3 from multiple reviewers. We arrange these answers into a 3×3 grid—each row representing a reviewer’s answers, each column a particular question. Surprisingly, we find a pattern: the product of the three yes/no values (interpreted as ±1) in each row and column is +1—except in one column, where the product is –1. This resembles a well-known paradox from quantum logic known as the Mermin square.

Why is this remarkable? Because the pattern cannot arise if each paper had fixed, objective answers to the three questions. Any consistent assignment of ±1 values must satisfy the same parity across all rows and columns. The observed pattern violates this. The contradiction shows that the answers cannot all be pre-assigned independently; they depend on how and when the questions are posed.

In this paper, we argue that contextuality—manifest here in peer review—is best understood as a computational effect. Just as exceptions, state, or nondeterminism capture interactions with ambient computation, contextuality captures dependence on evaluation context. Using algebraic effects and handlers, we formalize a semantics where answers are not static values, but computations sensitive to their surroundings.

We identify a minimal structure capturing contextual paradoxes and show how it can be encoded using algebraic theories. We apply this to abstract models of judgment, logical frameworks, and semantic systems in which evaluation context influences observable outcomes.

Reframing contextuality as a computational effect enables us to reason formally about systems where truth is inseparable from interaction. The Kochen–Specker phenomenon, and its analogs in reviewing and beyond, becomes not a failure of logic but an instance of ambient computation.

We assure our reviewers that this paper is both technically sound and significant. Whether it is clearly written, however, may depend on what you read next.

