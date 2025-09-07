# Effectful Contextuality

Here we propose a constructive, non-local hidden variable model based on various computational effects with Haskell implementation. It is designed to simulate and account for some "mysterious phenomenons" in quantum physics when it comes to observation and measurement, namely non-locality and contextuality, as revealed by Bell-Kochen-Specker theorem and its loophole-free experiments.


## Summary

DONE: Mermin's experiment with many different effects were fully explored. Other famous scenarios of contextuality are briefly explored with terminology of sheaf-theory (see `runners/sheaf`).

TODO: Design a language that tracks a few observables including time, space, and those for measurement. Find a proper way to integrate seamlessly with Abramsky's sheaf-framework.

The very basic, unitary idea is to represent a particle (or qubit) as `Property -> m Decision` where `m` stands for a monadic effect. It counts as a processual representation natually, that is, instead of a (pure) value, a particle is represeted as a abstract, effectful computation which generates and determinates a value when interacted in a certain way (eg. measurement by an abstract device).

However, when it extends to quantum system instead of a single particle, it becomes ambigious (this ambigiuty best manifests in the case of Mermin's square). The applicative functor `f` in `f (Propery -> m Decision)` can either mean "contexts" or "partites", and they coincide in the case of Mermin's experiment.





