## 2021 Computational Logic for Artificial Intelligence Coursework
# Philip Sloan and Jonathan Erskine

In this coursework we attempt to extend the reasoning capabilities of Prolexa to include negation.

#Negation

In it's current state, Prolexa cannot handle negation semantically, or in terms of reasoning e.g. given a statement "Tweety does not fly" or "Tweety is not a bird", Prolexa will fail to interpret the natural language of the query due to the unknown effects of "not" on a predicate in terms of sentence structure, and it cannot associate this language term with the meaning of the word not.

To implement negation grammatically 

'''
verb_phrase(s,M) --> [is],property(s,M).
verb_phrase(s,not(M)) --> [is],[not],property(s,M).
verb_phrase(s,not(M)) --> [not],property(s,M).
'''
