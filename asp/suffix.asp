#program base.

% All goal fluents hold in the suffix layer.
suffix(holds(F)) :- goal(F).
% If a fluent holds in the suffix layer, either some action causes it or it is
% a subgoal.
{subgoal(F); suffix(causes(A,F)) : add(A,F),not preserving(A)} = 1 :- suffix(holds(F)).
% If an action causes a fluent in the suffix, it happens.
suffix(happens(A)) :- suffix(causes(A,_)).
% If an action occurs in the suffix layer, then all of its preconditions hold
% in ths suffix layer
suffix(permits(F,A)) :- suffix(happens(A)); pre(A,F).
suffix(holds(F)) :- suffix(permits(F,_)).

% If any action happens in the suffix layer, then we are using the suffix layer.
useSuffix :- suffix(happens(_)).

% A fluent is supported in the suffix if it's a subgoal
suffixSup(holds(F)) :- subgoal(F).
% An action is supported in the suffix if all of its preconditions are
suffixSup(happens(A)) :- suffixSup(holds(F)) : pre(A,F); suffix(happens(A)).
% A fluent is supported in the suffix if its causing action is
suffixSup(holds(F)) :- suffixSup(happens(A)); suffix(causes(A,F)).

% No action happens in the suffix without support
:- suffix(happens(A)); not suffixSup(happens(A)).
% No fluent happens in the suffix without support
:- suffix(holds(F)); not suffixSup(holds(F)).

% Actions that happen in the suffix layer impose their cost.
:~ suffix(happens(A)); cost(A,V).[V,A,suffix]

% Very weak preference to avoid using the suffix layer.
:~ useSuffix.[1@-1]

#show suffix/1.
