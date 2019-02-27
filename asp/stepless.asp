is(fluentOcc(F,1)) :- fluent(F).
is(actOcc(A,1)) :- action(A).
is(fluentOcc(F,0)) :- init(F).


% ============================= Problem description ============================

% Helper function to recognize subsequent occurences of the same
% fluent/action.
nextOcc(fluentOcc(F,0),fluentOcc(F,1)) :- fluent(F).
nextOcc(fluentOcc(F,M),fluentOcc(F,M+1)) :- is(fluentOcc(F,M)).
nextOcc(actOcc(A,N),actOcc(A,N+1)) :- is(actOcc(A,N)).

% Any fluent above level 0 which holds must have exactly one causing
% action.
{causes(actOcc(A,N),fluentOcc(F,M)) : add(A,F), is(actOcc(A,N))}=1 :-
  holds(fluentOcc(F,M)); M > 0.
% If an action causes a fluent, it happens.
happens(AO) :- causes(AO,_).
% An action cannot cause more than one occurrence of the same fluent.
:- {causes(AO,fluentOcc(F,M))} > 1; is(AO); fluent(F).
% For each precondition an action occurrence has, some occurrence of
% that fluent must permit it.
{permits(fluentOcc(F,M),actOcc(A,N)) : is(fluentOcc(F,M))}=1 :-
  happens(actOcc(A,N)); pre(A,F).
% A fluent occurrence which permits an action must hold.
holds(FO) :- permits(FO,_).
% A fluent which is used to satisfy a subgoal condition "permits" it.
% For each subgoal condition, exactly one occurrence of that fluent
% permits it.
{permits(fluentOcc(F,M),subgoal(F)) : is(fluentOcc(F,M))}=1 :-
  subgoal(F).
% A fluent which permits a subgoal condition cannot be deleted.
:- deleted(FO); permits(FO,subgoal(_)).

% An occurrence of an action deletes an occurrence of a fluent if
% it permits it and that action has the fluent as a delete effect.
deletes(actOcc(A,N),fluentOcc(F,M)) :-
  permits(fluentOcc(F,M),actOcc(A,N)); del(A,F).
% No fluent may be deleted by more than one action.
:- {deletes(_, FO)} > 1; is(FO).
% An action which deletes a fluent, but doesn't have it as a
% precondition follows some occurrence of that fluent. Can possibly
% follow occurrence index 0 even if the fluent is not an initial
% fluent (indicates this action occurs before any occurrence of
% that fluent).
{follows(actOcc(A,N),fluentOcc(F,M)) : holds(fluentOcc(F,M));
  follows(actOcc(A,N),fluentOcc(F,0))}=1 :-
  del(A,F); not pre(A,F); happens(actOcc(A,N)).
% Fluent occurrences 0 which aren't initial fluents count as "deleted".
deleted(fluentOcc(F,0)) :- fluent(F); not init(F).
% A fluent is deleted if something deletes it.
deleted(FO) :- deletes(_, FO).
% A fluent is deleted if something follows it.
deleted(FO) :- follows(_, FO).

% Weak constraint charging the cost of an action occurrence.
:~ happens(actOcc(A,N)); cost(A,V).[V,A,N]

% An occurrence of a fluent doesn't hold if its previous occurrence
% doesn't hold.
:- holds(fluentOcc(F,M+1)); not holds(fluentOcc(F,M));
    is(fluentOcc(F,M)); M > 0.
% An occurrence of an action doesn't happen if its previous occurrence
% didn't happen.
:- happens(BO); not happens(AO); nextOcc(AO,BO).


% ============================= Plan Event Graph ===============================

% Events in the graph; these will be grouped into vertices
event(start(FO)) :- holds(FO).
event(end(FO)) :- holds(FO).
event(end(fluentOcc(F,0))) :- fluent(F).
event(AO) :- happens(AO).
% subgoals are events
event(subgoal(F)) :- subgoal(F).

% Triggering actions
% The start of a fluent by its causing action.
actionTriggers(AO,start(FO)) :- causes(AO,FO).
% The end of a fluent by its deleting action.
actionTriggers(AO,end(FO)) :- deletes(AO,FO).

% Vertices
% If no action triggers an event, then it gets a vertex by itself.
triggered(V) :- actionTriggers(A,V).
vertex(V) :- event(V); not triggered(V).
% Otherwise it belongs to the vertex for its trigger action.
inVertex(E,V) :- actionTriggers(V,E).
% Every event which is the name of a vertex belongs to that vertex.
inVertex(V,V) :- vertex(V).

% Graph edges
% A fluent ends after it starts
edge(start(FO),end(FO)) :- holds(FO).
% If a fluent permits an action, then the action happens after
% the start of the fluent
edge(start(FO),AO) :- permits(FO,AO).
% If a fluent permits an action but the action doesn't delete the
% fluent, then the action happens before the end of the fluent.
edge(actOcc(A,N),end(fluentOcc(F,M))) :-
  permits(fluentOcc(F,M),actOcc(A,N)); not del(A,F).
edge(subgoal(F),end(fluentOcc(F,M))) :- permits(fluentOcc(F,M),subgoal(F)).
% An action happens after the fluent it follows
edge(end(FO),AO) :- follows(AO,FO).
% but before the next occurrence
edge(AO,start(GO)) :- follows(AO,FO); nextOcc(FO,GO); holds(GO).
% The start of the next occurrence of a fluent happens after the
% end of the previous occurrence
edge(end(FO),start(GO)) :- holds(GO); nextOcc(FO,GO).
% The next occurrence of an action happens after the previous
% occurrence
edge(AO,BO) :- happens(AO); happens(BO); nextOcc(AO,BO).

% And now we use stable models to assert that the graph is acyclic; sup(X)
% indicates that X has acyclic support going back to the root of the graph.

% The input for a given event has support if all events joined
% by any incoming edge have support.
sup(in(E)) :- sup(D) : edge(D,E); event(E).
% A vertex has support if all of its events' inputs have support.
sup(V) :- sup(in(E)) : inVertex(E,V); vertex(V).
% An event has support if its vertex has support.
sup(E) :- sup(V); inVertex(E,V).
% Every vertex must have support.
:- vertex(V); not sup(V).


% ============================= Strong minimality ==============================

% A counterexample to strong minimality consists of two cuts, cut1
% and cut2.
cut(cut1; cut2).

% For each vertex V and each cut C, V is on either the s-side or
% the t-side of V. Note this rule is disjunctive.
onSideOf(V,s,C) | onSideOf(V,t,C) :- vertex(V); cut(C).
% An event belongs to the cut side of its vertex.
onSideOf(E,X,C) :- inVertex(E,V); onSideOf(V,X,C).
% Any subgoal is always on the t-side of cut2.
onSideOf(subgoal(F),t,cut2) :- subgoal(F).
% If there's a directed edge from D to E, but D is on the t-side
% and E is on the s-side, this is not a cut (invalidating this
% counterexample to strong minimality).
not_counterexample :- edge(D,E); onSideOf(D,t,C); onSideOf(E,s,C).
% If a fluent starts on the s-side of cut2 and ends on the t-side,
% then it "holds over" cut2.
holdsOver(F,cut2) :-
  onSideOf(start(fluentOcc(F,M)),s,cut2);
  onSideOf(end(fluentOcc(F,M)),t,cut2).
% Similarly if it starts and ends on the same side of cut1, then it
% doesn't hold over cut1.
not_holdsOver(FO,cut1) :-
  onSideOf(start(FO),X,cut1); onSideOf(end(FO),X,cut1).
% Action occurrence AO is not between cut1 and cut2 if it's on the
% s-side of cut1 or the t-side of cut2.
not_betweenCuts(AO) :- onSideOf(AO,s,cut1).
not_betweenCuts(AO) :- onSideOf(AO,t,cut2).
% If no action occurs between the two cuts, then this is not a
% counterexample.
not_counterexample :- not_betweenCuts(AO) : happens(AO).
% If there exists a fluent for which some occurrence holds over cut2,
% but no occurrence holds over cut1, then this is not a counterexample.
not_counterexample :-
  holdsOver(F,cut2);
  not_holdsOver(fluentOcc(F,M),cut1) : holds(fluentOcc(F,M)).

% There should be no counterexample (sorry for the triple negative).
:- not not_counterexample.

% If this is not a counterexample, all atoms must hold.
onSideOf(V,s,C) :- vertex(V); cut(C); not_counterexample.
onSideOf(V,t,C) :- vertex(V); cut(C); not_counterexample.


% =============================== Suffix Layer =================================
#include "suffix.asp".

% ================================ Saturated ===================================

% A fluent is saturated if all occurrences of it hold (besides the 0th).
saturated(fluent(F)) :-
  holds(fluentOcc(F,M)) : is(fluentOcc(F,M)),M>0; fluent(F).
% An action is saturated if all occurrences of it happen.
saturated(action(A)) :-
  happens(actOcc(A,N)) : is(actOcc(A,N)); action(A).

% If an action happens in the suffix layer and all of its preconditions
% are subgoals, we designate it a "starting" action.
suffix(start(action(A))) :- subgoal(F) : pre(A,F); suffix(happens(A)).
% Any fluent caused by a starting action is designated a "starting"
% fluent.
suffix(start(fluent(F))) :- suffix(start(action(A))); suffix(causes(A,F)).

% Guarantees that some starting action or fluent will be saturated.
:- useSuffix; not saturated(X) : suffix(start(X)).


% ==============================================================================

#show vertex/1.
#show inVertex/2.
#show edge/2.
#show saturated/1.

#program fluentOcc(f,m).
is(fluentOcc(f,m)).

#program actOcc(a,n).
is(actOcc(a,n)).
