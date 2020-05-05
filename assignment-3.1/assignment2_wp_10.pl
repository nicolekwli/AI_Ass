% candidate_number(10).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> find_identity_o(A)
  ).

find_identity_2(A):-
  % initialises the list of all possible actors and the links on their pages
  findall((Actor, Links), (actor(Actor), wp(Actor, Text), findall(Link, wt_link(Text, Link), Links)), Bag),
  % recursively eliminate actors
  find_from_link(Bag, A).

% custom member check predicate to work with lists of tuples
memberCustom(X, (_A,[X|_])).
memberCustom(X, (A,[_|Xs])) :- memberCustom(X, (A,Xs)).

% predicate to eliminate actors without link L from the list
remove_actors(Bag, L, X):-
  include(memberCustom(L), Bag, Result),
  X=Result.

% recursive predicate to find actors by the links on their page
find_from_link(Bag, A):-
  % if Bag is length 1, then you've found the actor
  (length(Bag,1) -> [(X,_)] = Bag, A = X
  %otherwise, get a new link and remove from there
  ; otherwise -> agent_ask_oracle(oscar,o(1),link,L), remove_actors(Bag, L, Bag2), find_from_link(Bag2, A)
  ).

find_identity_o(A):-
  % initialises the list of all possible actors and the links on their pages
  findall((Actor, Links), (actor(Actor), wp(Actor, Text), findall(Link, wt_link(Text, Link), Links)), Bag),
  % recursively eliminate actors
  find_from_link(Bag, A).
