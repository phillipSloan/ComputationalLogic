## 2021 Computational Logic for Artificial Intelligence Coursework
##### Philip Sloan and Jonathan Erskine

In this coursework we attempt to extend the reasoning capabilities of Prolexa to include negation.

#### Negation

In it's current state, Prolexa cannot handle negation semantically, or in terms of reasoning e.g. given a statement "Tweety does not fly" or "Tweety is not a bird", Prolexa will fail to interpret the natural language of the query due to the unknown effects of "not" on a predicate in terms of sentence structure, and it cannot associate this language term with the meaning of the word not.

To implement negation grammatically we have to include negative verb phrases:

```
verb_phrase(s,M) --> [is],property(s,M).
verb_phrase(s,not(M)) --> [is],[not],property(s,M).
verb_phrase(s,not(M)) --> [not],property(s,M).
```
Here we have only dealt with the singular case, so phrases like "All teachers are not happy" aren't currently handled by Prolog - this can be replaced with "Every teacher is not happy" so we will not attempt to extend the grammar for the purposes of this demonstration. We can now handle phrases like "Donald is not happy", but they have no bearing with respect to reasoning. This can be observed if we input some conflicting information:

```
prolexa> "tell me everything".
*** utterance(tell me everything)
*** goal(all_rules(_7414))
*** answer(I know nothing)
I know nothing
prolexa> "donald is happy".
*** utterance(donald is happy)
*** rule([(happy(donald):-true)])
*** answer(I will remember that donald is happy)
I will remember that donald is happy
prolexa> "donald is not happy".
*** utterance(donald is not happy)
*** rule([(not(happy(donald)):-true)])
*** answer(I will remember that donald is not happy)
I will remember that donald is not happy
prolexa> "tell me everything".
*** utterance(tell me everything)
*** goal(all_rules(_9184))
*** answer(donald is happy. donald is not happy)
donald is happy. donald is not happy
```


