technology(
    bees;
    cherries;
    orange_crayons;
    catapults;
    gophers;
    abacus;
    combustion_engine;
    banshee
  ).

conjunctive_deps(bees; orange_crayons; catapults; abacus; banshee).
disjunctive_deps(cherries; gophers; combustion_engine).
dep(
    cherries,bees;
    catapults,cherries;
    gophers,catapults;
    gophers,bees;
    abacus,bees;
    abacus,orange_crayons;
    abacus,catapults;
    combustion_engine,cherries;
    banshee,abacus;
    cherries,abacus
  ).
research_time(
  bees,5;
  cherries,5;
  orange_crayons,2;
  catapults,7;
  gophers,1;
  abacus,20;
  combustion_engine,17;
  banshee,0
  ).
objective(
  combustion_engine;
  banshee
  ).

fluent(researched(T)) :- technology(T).

action(research_conjunctive(T)) :- technology(T); conjunctive_deps(T).
pre(research_conjunctive(T),researched(S)) :- action(research_conjunctive(T)); dep(T,S).
add(research_conjunctive(T),researched(T)) :- action(research_conjunctive(T)).
cost(research_conjunctive(T),C) :- action(research_conjunctive(T)); research_time(T,C).

action(research_disjunctive_via(T,S)) :- technology(T); disjunctive_deps(T); dep(T,S).
pre(research_disjunctive_via(T,S),researched(S)) :- action(research_disjunctive_via(T,S)).
add(research_disjunctive_via(T,S),researched(T)) :- action(research_disjunctive_via(T,S)).
cost(research_disjunctive_via(T,S),C) :- action(research_disjunctive_via(T,S)); research_time(T,C).

goal(researched(T)) :- objective(T).

#show action/1.
#show fluent/1.
#show pre/2.
#show add/2.
#show cost/2.
#show goal/1.
