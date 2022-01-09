## 2021 Computational Logic for Artificial Intelligence Coursework
##### Philip Sloan and Jonathan Erskine

In this coursework we attempt to extend the reasoning capabilities of Prolexa to include negation.

### Negation

In it's current state, Prolexa cannot handle negation semantically, or in terms of reasoning e.g. given a statement "Tweety does not fly" or "Tweety is not a bird", Prolexa will fail to interpret the natural language of the query due to the unknown effects of "not" within sentence structure, and it cannot associate "not" with any meaning regarding a clause or set of clauses.

---
#### Negation - Grammar
---
To implement negation grammatically we have to modify prolexa_grammar.pl to include negated verb phrases:
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
This implements a special case where, if the verb phrase is negative, we pass the negative rule but borrow the determiner of the positive case. The modification is more straight forward for proper nouns:
```
sentence1([(L:-true)]) --> proper_noun(N,X),verb_phrase(N,X=>L).
sentence1([(not(L):-true)]) --> proper_noun(N,X),verb_phrase(N,not(X=>L)).
```

Here we have only dealt with the singular case, so negated phrases like "All teachers are not happy" aren't currently handled by Prolog - this can be replaced with "Every teacher is not happy" so we will not attempt to extend the grammar for the purposes of this demonstration. 

Finally, we need to extend the question interpreter to understand "not" within a query:
```
question1(Q) --> [who],verb_phrase(s,_X=>Q).
question1(Q) --> [is], proper_noun(N,X),property(N,X=>Q).
question1(Q) --> [does],proper_noun(_,X),verb_phrase(_,X=>Q).

question1(not(Q)) --> [who],verb_phrase(s,not(_X=>Q)).
question1(not(Q)) --> [is],proper_noun(N,X),verb_phrase(N,not(X=>Q)).
```
---
#### Negation - Reasoning
---
We can now handle phrases like "Donald is not happy" and "Every teacher is not happy", but they have no bearing with respect to reasoning. This can be observed if we input some conflicting information:

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
Prolexa cannot recognise the confliction between donald being happy and unhappy ("not happy") at the same time. To enable this we need to apply a function which removes conflicting rules. The following is added to prolexa_engine.pl :
```
remove_conflicting_rules([Head:-Body]):-
	(conflicting_not_rules(Head:-Body)
	; retractall(prolexa:stored_rule(_,[(not(Head):-Body)])),
	     retractall(prolexa:stored_rule(_,[(Head:-not(Body))])) ).

conflicting_not_rules(not(Head):-Body):-
	retractall(prolexa:stored_rule(_,[(Head:-Body)])).

conflicting_not_rules(Head:-not(Body)):-
	retractall(prolexa:stored_rule(_,[(Head:-Body)])).
```
This new function takes a rule and searches the current rulebase to remove any which are in direct conflict. This function is called from prolexa.pl when a new rule is added.
```
% A. Utterance is a sentence
	( phrase(sentence(Rule),UtteranceList),
	  write_debug(rule(Rule)),
	  ( known_rule(Rule,SessionId) -> % A1. It follows from known rules
			atomic_list_concat(['I already knew that',Utterance],' ',Answer)
	  ; otherwise -> % A2. It doesn't follow, so add to stored rules
```
```ruby
		        remove_conflicting_rules(Rule),
```
```
			assertz(prolexa:stored_rule(SessionId,Rule)),
			atomic_list_concat(['I will remember that',Utterance],' ',Answer)
	  )
```
---

Removal of conflicting rules can be observed by the following:
```
user: "forget everything".
prolexa: I am a blank slate

user: "donald is happy".
prolexa: I will remember that donald is happy

user: "donald is not happy".
prolexa: I will remember that donald is not happy

user: "tell me everything".
prolexa: donald is not happy
```
Prolexa can now properly store and remove rules by considering new, conflicting information, but we can still not infer answers to negated questions from positive literals, or vice versa. For example:
```
user: "donald is not happy".
prolexa: I will remember that donald is not happy

user: "is donald happy"
prolexa: Sorry, I don't think this is the case
```
The question answering engine will attempt to prove the query, and if it cannot will provide a response indicating that the answer is not found in the knowledge base ("Sorry, I don't..."). 

However, in this case, clearly "Is donald happy?" should have an answer as we know that Donald is not happy. To remedy this, we add an extra step in the question answering process to check if the negative of a query can be proven. 

This requires modification of prove_question/2, prove_question/3 and explain_question to duplicate the first check but with the negative version of the query.

```
%%% Main question-answering engine adapted from nl_shell.pl %%%

prove_question(Query,SessionId,Answer):-
    findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
    ( prove_rb(Query,Rulebase) ->
        transform(Query,Clauses),
        phrase(sentence(Clauses),AnswerAtomList),
        atomics_to_string(AnswerAtomList," ",Answer)
```
```ruby
    ; prove_rb(not Query,Rulebase) ->
        transform(not Query,Clauses),
        phrase(sentence(Clauses),AnswerAtomList),
        atomics_to_string(AnswerAtomList," ",Answer)
```
```
    ; Answer = 'Sorry, I don\'t think this is the case'
    ).
    
% two-argument version that can be used in maplist/3 (see all_answers/2)
prove_question(Query,Answer):-
	findall(R,prolexa:stored_rule(_SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
```
```ruby
	; prove_rb(not Query,Rulebase) ->
			transform(not Query,Clauses),
			phrase(sentence(Clauses),AnswerAtomList),
			atomics_to_string(AnswerAtomList," ",Answer)
```
```
	; Answer = ""
	).


%%% Extended version of prove_question/3 that constructs a proof tree %%%
explain_question(Query,SessionId,Answer):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase,[],Proof) ->
		maplist(pstep2message,Proof,Msg),
		phrase(sentence1([(Query:-true)]),L),
		atomic_list_concat([therefore|L]," ",Last),
		append(Msg,[Last],Messages),
		atomic_list_concat(Messages,"; ",Answer)
```
```ruby
	; prove_rb(not(Query),Rulebase,[],Proof) ->
		maplist(pstep2message,Proof,Msg),
		phrase(sentence1([(not(Query):-true)]),L),
		atomic_list_concat([therefore|L]," ",Last),
		append(Msg,[Last],Messages),
		atomic_list_concat(Messages," ; ",Answer)
```
```
	; Answer = 'Sorry, I don\'t think this is the case'
	).


```
Prolog can now handle our earlier example with a correct answer:
```
user: "tell me everything".
prolexa: donald is not happy

user:  "is donald happy".
prolexa: donald is not happy
```
The assignment asks us to prove:
> Every teacher is happy. Donald is not happy. Therefore, Donald is not a teacher.

This test fails, yielding the following dialogue:
```
user: "tell me everything".
prolexa: donald is not happy. every teacher is happy

user: "is donald a teacher".
prolexa: Sorry, I don't think this is the case
```
Clearly, there is an issue with the method of explanation. If donald is happy, then we can not say for certain whether he is or is not a teacher. However, knowing that donald is not happy confirms that he cannot be a teacher, as all teachers are happy. 

Whereas for a positive term, e.g. teacher(donald) the proof tree will do X, search for any set of rules and ground truths which indicate that donald is a teacher e.g. 
> teacher(donald):-true 
or
> teacher(X):-happy(X), happy(donald):-true 

We now have to add a step which attempts to unify `not(teacher(donald))` with the rulebase 
> Every teacher is happy. Donald is not happy.

#Explanation needed here

When looking at the final predicate, it can be seen that prove_rb/4 tries to unify B with a body of a stored rule whos head matches the given A. It will cycle through all stored rules and try and find a matching rule, but in the second case this will not happen, as it will be trying to match teacher(donald) to happy(X), so it fails. We realised that a new predicate needed to be created, that mirrored the existing one and unified the head A, given a body B. The following predicate was added to prove_rb/4.:
```
prove_rb(true,_Rulebase,P,P):-!.
prove_rb((A,B),Rulebase,P0,P):-!,
    find_clause((A:-C),Rule,Rulebase),
    conj_append(C,B,D),
    prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_rb(A,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
    prove_rb(B,Rulebase,[p(A,Rule)|P0],P).
```
```ruby
prove_rb(not B,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(not A,Rulebase,[p(not B,Rule)|P0],P).
```


