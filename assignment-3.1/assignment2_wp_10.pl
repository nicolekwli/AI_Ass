% candidate_number(10).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> my_agent(Agent), find_identity_o(Agent, A)
  ).

% ----
find_identity_2(A):-
  % initialises the list of all possible actors and the links on their pages
  findall((Actor, Links), (actor(Actor), wp(Actor, Text), findall(Link, wt_link(Text, Link), Links)), Bag),
  % recursively eliminate actors
  find_from_link(Bag, A).


% ----
% custom member check predicate to work with lists of tuples
memberCustom(X, (_A,[X|_])).
memberCustom(X, (A,[_|Xs])) :- memberCustom(X, (A,Xs)).


% ----
% predicate to eliminate actors without link L from the list
remove_actors(Bag, L, X):-
  include(memberCustom(L), Bag, Result),
  X=Result.


% ----
% recursive predicate to find actors by the links on their page
find_from_link(Bag, A):-
  % if Bag is length 1, then you've found the actor
  (length(Bag,1) -> [(X,_)] = Bag, A = X
  %otherwise, get a new link and remove from there
  ; otherwise -> agent_ask_oracle(oscar,o(1),link,L), remove_actors(Bag, L, Bag2), find_from_link(Bag2, A)
  ).

% recursive predicate to find actors by the links on their page
find_from_link_2(Agent, Bag, A):-
  % write($Bag),write('\n'),
  % if Bag is length 1, then you've found the actor
  (length(Bag,1) ->
    [(X,_)] = Bag,
    A = X
  %otherwise, get a new link and remove from there
  ; otherwise -> 
    query_world( agent_current_position, [Agent,P] ),
    query_world(agent_current_energy, [Agent, Energy]),
    ClosedList = [P],
    Start = n(P,0,0,[P]),
    a_star(Energy,identify,[Start],ClosedList,TempR,TempCost,Return),!,
    % need return because query world doesn't work with variables, so we need the exact oracle

    reverse(TempR,TempPath),
    TempPath = [_head|TempPath_t],

    query_world(agent_do_moves,[Agent,TempPath_t]),
    query_world(agent_ask_oracle,[Agent,Return,link,L]),

    remove_actors(Bag, L, Bag2),
    find_from_link_2(Agent, Bag2, A)
  ).


% ----
% THIS IS WHERE THE FUN BEGINS
find_identity_o(Agent, A):-
  % initialises the list of all possible actors and the links on their pages
  findall((Actor, Links), (actor(Actor), wp(Actor, Text), findall(Link, wt_link(Text, Link), Links)), Bag),
  % recursively eliminate actors
  find_from_link_2(Agent, Bag, A).
