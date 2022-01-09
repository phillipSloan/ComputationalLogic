# Report for Computational Logic for Artificial Intelligence (2021)


## Authors [Phillip Sloan](https://github.com/phillipSloan) and [Jonathan Erksine](https://github.com/jmerskine1)

A notebook containing a thorough description of the changes is provided via the Google Colab notebook: [![Open Report In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/phillipSloan/ComputationalLogic/blob/prolexa-plus/Report%20Notebook.ipynb) 

This repository contains the code required to extend the reasoning capabilities of prolexa to cover negation. Prolexa can handle the following query:

> user: donald is not happy
> 
> user: every teacher is happy
> 
> user: explain why donald is not a teacher
> 
> prolexa: donald is not happy; every teacher is happy; therefore donald is not a teacher

Changes/modifications to the default prolexa code are as given below for each file which is modified.

### prolexa_grammar.pl

```prolog
:-op(900,fy,not).

verb_phrase(s,not(M)) --> [is],[not],property(s,M).
verb_phrase(s,not(M)) --> [not],property(s,M).

sentence1([(H:-not(B))]) --> determiner(N,M1,M2,[(H:-B)]),noun(N,M1),verb_phrase(N,not(M2)).

sentence1([(not(L):-true)]) --> proper_noun(N,X),verb_phrase(N,not(X=>L)).

question1(not(Q)) --> [who],verb_phrase(s,not(_X=>Q)).
question1(not(Q)) --> [is],proper_noun(N,X),verb_phrase(N,not(X=>Q)).
```

### prolexa_engine.pl

```prolog
remove_conflicting_rules([Head:-Body]):-
	(conflicting_not_rules(Head:-Body)
	; retractall(prolexa:stored_rule(_,[(not(Head):-Body)])),
	     retractall(prolexa:stored_rule(_,[(Head:-not(Body))])) ).

conflicting_not_rules(not(Head):-Body):-
	retractall(prolexa:stored_rule(_,[(Head:-Body)])).

conflicting_not_rules(Head:-not(Body)):-
	retractall(prolexa:stored_rule(_,[(Head:-Body)])).


%%% Main question-answering engine adapted from nl_shell.pl %%%

prove_question(Query,SessionId,Answer):-
    findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
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
    
% two-argument version that can be used in maplist/3 (see all_answers/2)
prove_question(Query,Answer):-
	findall(R,prolexa:stored_rule(_SessionId,R),Rulebase),
	( prove_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)

	; prove_rb(not Query,Rulebase) ->
			transform(not Query,Clauses),
			phrase(sentence(Clauses),AnswerAtomList),
			atomics_to_string(AnswerAtomList," ",Answer)

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

	; prove_rb(not(Query),Rulebase,[],Proof) ->
		maplist(pstep2message,Proof,Msg),
		phrase(sentence1([(not(Query):-true)]),L),
		atomic_list_concat([therefore|L]," ",Last),
		append(Msg,[Last],Messages),
		atomic_list_concat(Messages," ; ",Answer)

	; Answer = 'Sorry, I don\'t think this is the case'
	).


%for double negatives
prove_rb(not(not(A)),Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(B,Rulebase,[p(A,Rule)|P0],P).

% transform instantiated, possibly conjunctive, query to list of clauses
transform((A,B),[(A:-true)|Rest]):-!,
    transform(B,Rest).
transform(not(not(A)),B):-!,
	transform(A,B).
transform(A,[(A:-true)]).


prove_rb(true,_Rulebase,P,P):-!.
prove_rb((A,B),Rulebase,P0,P):-!,
    find_clause((A:-C),Rule,Rulebase),
    conj_append(C,B,D),
    prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_rb(A,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
    prove_rb(B,Rulebase,[p(A,Rule)|P0],P).

prove_rb(not B,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(not A,Rulebase,[p(not B,Rule)|P0],P).
```

### prolexa.pl
```prolog
% A. Utterance is a sentence
	( phrase(sentence(Rule),UtteranceList),
	  write_debug(rule(Rule)),
	  ( known_rule(Rule,SessionId) -> % A1. It follows from known rules
			atomic_list_concat(['I already knew that',Utterance],' ',Answer)
	  ; otherwise -> % A2. It doesn't follow, so add to stored rules

		        remove_conflicting_rules(Rule),


			assertz(prolexa:stored_rule(SessionId,Rule)),
			atomic_list_concat(['I will remember that',Utterance],' ',Answer)
	  )
```
