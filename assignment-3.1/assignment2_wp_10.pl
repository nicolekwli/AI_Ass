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
find_from_link_2(Agent, Oracles, Bag, A):-
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

    % finds the closest charging station
    setof(Location, find_stations(Location), Stations),
    closest_station(P,Stations,Closest),

    (a_star(Energy,identify,[Start],ClosedList,TempR,TempCost,Oracles,Return) ->
      R = TempR
    ;
      % need to find charging station
      a_star(Energy,find(Closest),[Start],ClosedList,Temp2R,Temp2Cost,_Oracles,_Return),!,


      reverse(Temp2R,Temp2Path),
      Temp2Path = [_head|Temp2Path_t],

      query_world(agent_do_moves,[Agent,Temp2Path_t]),
      query_world(agent_topup_energy,[Agent,c(_)]),

      % continue
      query_world( agent_current_position, [Agent,NewP] ),
      query_world(agent_current_energy, [Agent,NewEnergy]),

      NewClosedList = [NewP],
      NewStart = n(NewP,0,0,[NewP]),

      a_star(NewEnergy,identify,[NewStart],NewClosedList,Temp3R,Temp3Cost,Oracles,Return),!,

      R = Temp3R
    ),

    reverse(R,TempPath),
    TempPath = [_head|TempPath_t],

    query_world(agent_do_moves,[Agent,TempPath_t]),
    query_world(agent_ask_oracle,[Agent,Return,link,L]),

    remove_actors(Bag, L, Bag2),
    find_from_link_2(Agent, Oracles, Bag2, A)
  ).


% finds the identity of an actor
% needs to find the position of all the oracles first, in order to search using a cleverer heuristic
find_identity_o(Agent, A):-
  setof(Location, find_oracles(Location), Oracles),

  % initialises the list of all possible actors and the links on their pages
  findall((Actor, Links), (actor(Actor), wp(Actor, Text), findall(Link, wt_link(Text, Link), Links)), Bag),
  % recursively eliminate actors
  find_from_link_2(Agent, Oracles, Bag, A).

% finds the location of the oracles
find_oracles(Location) :-
  between(1,20,X),
  between(1,20,Y),
  map_adjacent(p(X,Y),Pos,Object),
  Object = o(_),
  Location = Pos.
