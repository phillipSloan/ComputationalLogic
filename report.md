# Report for COMSM0022 Computational Logic for Artificial Intelligence (2021)

## Authors
[Phillip Sloan](https://github.com/phillipSloan) and [Jonathan Erksine](https://github.com/jmerskine1)

## Contents
1. [Negation](#negation)
2. [Command Line Code](#command-line-code)
3. [Future Work](#future-work)

### Negation
To complete this assignment, we looked into adding negation as a functionality to prolexa. As per the assignment brief we tried to implement the following:

>Every teacher is happy. Donald is not happy. Therefore, Donald is not a teacher.

For this we suplemented the existing prolexa .pl files, prolexa_grammar.pl was extended with the following code:

```prolog
:-op(900,fy,not).

sentence1([(H:-not(B))]) --> determiner(N,M1,M2,[(H:-B)]),noun(N,M1),verb_phrase(N,not(M2)).
sentence1([(not(L):-true)]) --> proper_noun(N,X),verb_phrase(N,not(X=>L)).

verb_phrase(s,not(M)) --> [is],[not],property(s,M).
```
The op:- not code was used from the simply logical book / lectures. sentence1 was extended to allow negation of rules such as `not(happy(donald)):-true.` and `teacher(X):-not(happy(X)).`. Although the first is not needed for the assignments task, it was added for completeness and for adding our own rules. verb_phrase was also extended to allow the grammar to cope with `<noun> is not <verb>` as input.

The following was also added to prolexa_engine.pl:

```prolog
prove_rb(not B,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(not A,Rulebase,[p(not B,Rule)|P0],P).
```
This code was derived from the existing prove_rb clauses. The current prove_rb meta-interpreter unify a known head with a body from the stored rules using the built in predicate copy_term/2.
When investigating/debugging, it was noticed that it was not able to create the required proof tree and additions needed to be made. The proof tree needed a way to link `not(teacher(donald)).` to `happy(X):-teacher(X).` and finally to `not(happy(donald)):-true.`. By looking at this set, it was realised a way to link
these together, would be to remove the not from `not(teacher(donald)).`, and unify `teacher(donald)` to `happy(X):-teacher(X).`. To do this, the clause would need to be matched to the body, rather than the head of the stored rules.
Finally, another not was needed to move from `happy(X)` to `not(happy(X)` which was performed by the existing meta-interpreter predicates.


### Command Line Code
The majority of work done for this assignment was performed using the prolexa command line interface. When prolexa stored_rules are instantiated using prolexa_cli, it is currently possible to put two conflicting rules into the rule-base, such as:
  ```prolog
  happy(donald):-true.
  ```
  and
   ```prolog
  not(happy(donald)):-true.
  ```
The following code was implemted into prolexa_engine.pl to stop this from happening:
  
  ```prolog
  remove_conflicting_rules([Head:-Body]):-
	(conflicting_not_rules(Head:-Body)
	; retractall(prolexa:stored_rule(_,[(not(Head):-Body)])),
	     retractall(prolexa:stored_rule(_,[(Head:-not(Body))])) ).

conflicting_not_rules(not(Head):-Body):-
	retractall(prolexa:stored_rule(_,[(Head:-Body)])).

conflicting_not_rules(Head:-not(Body)):-
	retractall(prolexa:stored_rule(_,[(Head:-Body)])).
  ```
  
For example, if `happy(donald):-true.` is being added, it will seek to remove any versions of `not(happy(donald)):-true.` from the rule store.
### Future Work
