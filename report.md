# Report for COMSM0022 Computational Logic for Artificial Intelligence (2021)

[![Open Report In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/phillipSloan/ComputationalLogic/blob/prolexa-plus/Report%20Notebook.ipynb)
## Authors
[Phillip Sloan](https://github.com/phillipSloan) and [Jonathan Erksine](https://github.com/jmerskine1)

## Contents
1. [Introduction](#introduction)
2. [Meta-interpreter](#metainterpreter)
3. [Negation](#negation)
4. [Command Line Code](#command-line-code)
5. [Limitations](#limitations)

### Introduction
This method demonstrates our approach to this assignment, walking through how we implemented negation and some other command line code. It seeks to explain our thought process, which was wrong at certain points, causing unwanted proofs from the meta-interpreter. There are several snippets of code throughout this report of how our code was created, with the Colab notebook demonstrating the final implementation. A link to it can be found at the top of this report.

### Metainterpreter
When trying to understand the meta interpreter it was realised that the prove_rb predicate only worked one way, For example, the current meta-interpreter is able to solve the following request:
>Explain why peter is mortal

Due to the the stored rule: 
```prolog
stored_rule(1,[(mortal(X):-human(X))]).
```

However the following could not be proved with the current meta interpreter:
>Explain why donald is a teacher

Which uses the following rule:
```prolog
stored_rule(1,[(happy(X):-teacher(X))]).
```
We thought it was strange that the meta-interpreter could not handle the second case. Looking at the existing prove_rb meta interpreter:

```prolog
prove_rb(true,_Rulebase,P,P):-!.
prove_rb((A,B),Rulebase,P0,P):-!,
	find_clause((A:-C),Rule,Rulebase),
	conj_append(C,B,D),
    prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_rb(A,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
	prove_rb(B,Rulebase,[p(A,Rule)|P0],P).
```

When looking at the final predicate, it can be seen that prove_rb/4 tries to unify B with a body of a stored rule which head matches A. It will cycle through all stored_rules and try and find a matching rule, but in the second case this will not happen, as it will be trying to match teacher(donald) to happy(X), so it fails. We realised that a new predicate needed to be created, that mirrored the existing one and unified the head A, given a body B. The following predicate was added to prove_rb/4.

```prolog
prove_rb(B,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(A,Rulebase,[p(B,Rule)|P0],P).
```
Adding this statement allows Prolexa to prove the second statement.

### Negation
We looked into adding negation as a functionality to prolexa. As per the assignment brief we tried to implement the following:

>Every teacher is happy. Donald is not happy. Therefore, Donald is not a teacher.

For this we suplemented the existing prolexa .pl files, prolexa_grammar.pl was extended with the following code:

```prolog
:-op(900,fy,not).

sentence1([(H:-not(B))]) --> determiner(N,M1,M2,[(H:-B)]),noun(N,M1),verb_phrase(N,not(M2)).
sentence1([(not(L):-true)]) --> proper_noun(N,X),verb_phrase(N,not(X=>L)).

verb_phrase(s,not(M)) --> [is],[not],property(s,M).

question1(not(Q)) --> [who],verb_phrase(s,not(_X=>Q)).
```
The op:- not code was used from the simply logical book / lectures. sentence1 was extended to allow negation of rules such as `not(happy(donald)):-true.` and `teacher(X):-not(happy(X)).`. Although the first is not needed for the assignments task, it was added for completeness and for adding our own rules. verb_phrase was also extended to allow the grammar to cope with `<noun> is not <verb>` as input. question1's addition allows `who is not <verb>` to be utilized.

The following was also added to prolexa_engine.pl:

```prolog
prove_rb(not A,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(not B,Rulebase,[p( not A,Rule)|P0],P).

prove_rb(not B,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(not A,Rulebase,[p(not B,Rule)|P0],P).
```
This code was derived from the existing and new standard prove_rb clauses. Two versions are used to allow both a head or a body to be unified by the meta-interpreter. It works by trying to prove the opposite position, so inverting what it is trying to prove. An example would be if its trying to prove `happy(X):-teacher(X)` it will seek to prove `not(teacher(X)):-not(happy(x))`.

It was noticed that the new meta-interpreter only worked when the predicate succeeded, otherwise it would fall into an infinite recursive loop. To prevent this, a counter was added similar to what is found in the predicate `anti_unify_args` from 9.1 of too simply logical. This gives us the meta-interpreter:

```prolog
prove_rb(_N, true,_Rulebase,P,P):-!.
prove_rb(0, _A,_Rulebase,[],[]):-!.

prove_rb(N, (A,B),Rulebase,P0,P):-!,
  N>0,N1 is N-1,
	find_clause((A:-C),Rule,Rulebase),
	conj_append(C,B,D),
  prove_rb(N1, D,Rulebase,[p((A,B),Rule)|P0],P).
prove_rb(N, A,Rulebase,P0,P):-
	N>0,N1 is N-1,
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(N1, B,Rulebase,[p(A,Rule)|P0],P).

prove_rb(N, B,Rulebase,P0,P):-
	N>0,N1 is N-1,
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(N1, A,Rulebase,[p(B,Rule)|P0],P).

prove_rb(N, not A,Rulebase,P0,P):-
	N>0,N1 is N-1,
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(N1, not B,Rulebase,[p( not A,Rule)|P0],P).

prove_rb(N, not B,Rulebase,P0,P):-
	N>0,N1 is N-1,
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(N1, not A,Rulebase,[p(not B,Rule)|P0],P).

% top-level version that ignores proof
prove_rb(Q,RB):-
	prove_rb(2, Q,RB,[],_P).
```

However, when discussing this meta-interpreter we realised there was a flaw in our intial logic. We should not be trying to prove `donald is a teacher` from `stored_rule(1,[(happy(X):-teacher(X))]).` as it doesn't make follow logically from it.  The rule means `Every teacher is happy`. Teachers are a subset of happy X's, but the rule doesn't provide the information that all happy X's are teachers. The current meta-interpret was not logically sound, which led us to cutting down the metainterpreter into it's final, more compact state:

```prolog
prove_rb(true,_Rulebase,P,P):-!.

prove_rb((A,B),Rulebase,P0,P):-!,
	find_clause((A:-C),Rule,Rulebase),
	conj_append(C,B,D),
  prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).

prove_rb(A,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(B,Rulebase,[p(A,Rule)|P0],P).

% Added to allow negation to work
prove_rb(not B,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(not A,Rulebase,[p(not B,Rule)|P0],P).

% top-level version that ignores proof
prove_rb(Q,RB):-
	prove_rb(Q,RB,[],_P).
```

The meta-interpreter works, proving from A->B or proving its inverse when asked to prove negation. Another issue was found with the interpreter, in that it did not currently work with handle_utterances questions section. questions such as:
>"is donald a teacher".

Did not work, which can be seen in the log below:

```
prolexa> "tell me everything you know".
*** utterance(tell me everything you know)
*** goal(all_rules(_9430))
*** answer(every human is mortal. every teacher is happy. donald is not happy. peter is mortal)
every human is mortal. every teacher is happy. donald is not happy. peter is mortal
prolexa> "is donald a teacher".
*** utterance(is donald a teacher)
*** query(teacher(donald))
*** answer(Sorry, I don't think this is the case)
Sorry, I don't think this is the case
```

To get questions like this working with prolexa we needed to change the prove_question which is shown below:

```prolog
prove_question(Query,SessionId,Answer):-
    findall(R,prolexa:stored_rule(SessionId,R),Rulebase),     % create a list of all the rules and store them in RuleBase
    ( prove_rb(Query,Rulebase) ->
        transform(Query,Clauses),
        phrase(sentence(Clauses),AnswerAtomList),
        atomics_to_string(AnswerAtomList," ",Answer)
    ; prove_rb(not Query,Rulebase) ->
        transform(not Query,Clauses),
        phrase(sentence(Clauses),AnswerAtomList),
        atomics_to_string(AnswerAtomList," ",Answer)
    ; Answer = 'Sorry, I don\'t think this is the case'
    ).
```

The working example of this question can be found in the notebook.

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

### Limitations
When using Prolexa Plus it has been noticed that sometimes in some cases, it will add stored rule twice. This issue does not occur with standard prolexa, so we are assuming it is a conflict between the two?
