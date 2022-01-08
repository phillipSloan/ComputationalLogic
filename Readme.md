## 2021 Computational Logic for Artificial Intelligence Coursework
##### Philip Sloan and Jonathan Erskine

In this coursework we attempt to extend the reasoning capabilities of Prolexa to include negation.

### Negation

In it's current state, Prolexa cannot handle negation semantically, or in terms of reasoning e.g. given a statement "Tweety does not fly" or "Tweety is not a bird", Prolexa will fail to interpret the natural language of the query due to the unknown effects of "not" within sentence structure, and it cannot associate "not" with any meaning regarding a clause or set of clauses.

#### Negation - Grammar

To implement negation grammatically we have to modify prolexa_grammar.pl to include negative verb phrases:
```
verb_phrase(s,M) --> [is],property(s,M).
verb_phrase(s,not(M)) --> [is],[not],property(s,M).
verb_phrase(s,not(M)) --> [not],property(s,M).
```
Introducing "not(M)" into the verb_phrase requires us to extend our definition of sentence1 to include negative cases:
```
sentence1(C) --> determiner(N,M1,M2,C),noun(N,M1),verb_phrase(N,M2). 
sentence1([(H:-not(B))]) --> determiner(N,M1,M2,[(H:-B)]),noun(N,M1),verb_phrase(N,not(M2)).
```
This implements a special case where, if the verb phrase is negative, we pass the negative rule but apply the determiner of the positive case. The modification is more straight forward for proper nouns:
```
sentence1([(L:-true)]) --> proper_noun(N,X),verb_phrase(N,X=>L).
sentence1([(not(L):-true)]) --> proper_noun(N,X),verb_phrase(N,not(X=>L)).
```

Here we have only dealt with the singular case, so phrases like "All teachers are not happy" aren't currently handled by Prolog - this can be replaced with "Every teacher is not happy" so we will not attempt to extend the grammar for the purposes of this demonstration. 

#### Negation - Reasoning

We can now handle phrases like "Donald is not happy", but they have no bearing with respect to reasoning. This can be observed if we input some conflicting information:

```
user: "tell me everything".
prolexa: I know nothing

user: "donald is happy".
prolexa: I will remember that donald is happy

user: "donald is not happy".
prolexa: I will remember that donald is not happy
user: "tell me everything".
prolexa: donald is happy. donald is not happy
```
Not only does Prolexa fail to recognize,



