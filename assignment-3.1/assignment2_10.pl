candidate_number(10).

% finds the minimal cost of solving any task of the form (go(p(x,y))), find(o(n),c(n))
% find path using energy available (part 1)
% find path considering more stuff (part3/4)

% for example solve_task(+go(Pos), -Cost).
% fail when task not feasible or not enough energy
solve_task(Task,Cost) :-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  query_world(agent_current_energy, [Agent, Energy]),

  ClosedList = [P],

  % Nodes are in the form n(Current position,Depth,Cost of path,RPath)
  Start = n(P,0,0,[P]),

  % finds the closest charging station
  setof(Location, find_stations(Location), Stations),
  closest_station(P,Stations,Closest),


  (a_star(Energy,Task,[Start],ClosedList,TempR,TempCost,_Oracles,_Return) ->
    R = TempR,
    Cost = TempCost
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

    a_star(NewEnergy,Task,[NewStart],NewClosedList,Temp3R,Temp3Cost,_Oracles,_Return),

    R = Temp3R,
    Cost is Temp2Cost + Temp3Cost
  ),

  reverse(R,[_Init|Path]), 
  query_world( agent_do_moves, [Agent, Path] ).  % This performs the path found


%  ----------
% base case for go
a_star(_Energy,Task,OpenList,_ClosedList,ReturnPath,TotalCost,Oracles,Return) :-
  Task = go(Final),
  OpenList = [Open_h|_Open_t],
  Open_h = n(Current,_Depth,Cost,RPath),
  (Final = none -> true
  ; otherwise -> Current = Final),
  ReturnPath = RPath,
  TotalCost = Cost.

% base case for find
a_star(_Energy,Task,OpenList,_ClosedList,ReturnPath,TotalCost,Oracles,Return) :-
  Task = find(Final),
  OpenList = [Open_h|_Open_t],
  Open_h = n(Current,_Depth,Cost,RPath),
  (Final = none -> true
  ; otherwise -> map_adjacent(Current,_,Final)),
  ReturnPath = RPath,
  TotalCost = Cost.

% base case for identify
a_star(_Energy,Task,OpenList,_ClosedList,ReturnPath,TotalCost,Oracles,Return) :-
  Task = identify,
  OpenList = [Open_h|_Open_t],
  Open_h = n(Current,_Depth,Cost,RPath),
  map_adjacent(Current,_,o(Oracle)),
  my_agent(Agent),
  \+query_world(agent_check_oracle,[Agent,o(Oracle)]),
  ReturnPath = RPath,
  TotalCost = Cost,
  Return = o(Oracle).

% recursive case
a_star(Energy,Task,OpenList,ClosedList,ReturnPath,TotalCost,Oracles,Return) :-
  OpenList = [Open_h|Open_t],
  Open_h = n(Current,Depth,Cost,RPath),
  %creates a list of all possible children
  findall(n(NewCurrent,NewDepth,NewCost,NewRPath),search(Oracles,Task,Current,Depth,RPath,ClosedList,NewCurrent,NewDepth,NewCost,NewRPath),Children),

  % if the list of children is empty, it's a dead end so add the position to ClosedList
  length(Children,L),
  (L < 1 ->
    NewClosedList = [Current|ClosedList]
  ;
    NewClosedList = ClosedList
  ),

  % check if current energy is too little, fail if true, continue if false
  % TODO: deal with case energy is equal to Cost
  % NewEnergy could be used for something else?
  _NewEnergy is Energy - Depth,
  
  % OR: if energy < threshold
  ( Energy < Cost -> fail
    ;
    true
  ),

  % manually insert all children into the OpenList, maintaining the order they're in
  insert_children(Children,Open_t,NewOpenList),

  a_star(Energy,Task,NewOpenList,NewClosedList,ReturnPath,TotalCost,Oracles,Return).


%  ----------
% base case for inserting empty list
insert_children([],OpenList,NewOpenList) :-
  NewOpenList = OpenList.

% recursive case by inserting each individual child
insert_children([Child|Children],OpenList,NewOpenList) :-
  insert_child(Child,OpenList,OpenList2),
  insert_children(Children,OpenList2,NewOpenList).

% base case for inserting into empty list
insert_child(Child,[],NewOpenList) :-
  NewOpenList = [Child].

% skips duplicate positions because otherwise the algorithm breaks
insert_child(Child,OpenList,NewOpenList) :-
  OpenList = [Open_h|_],
  Open_h = n(P1,_,_,_),
  Child = n(P2,_,_,_),
  P1 = P2,
  NewOpenList = OpenList.

% recursive case works by comparing the costs and inserting correspondingly
insert_child(Child,OpenList,NewOpenList) :-
  OpenList = [Open_h|Open_t],
  Open_h = n(_,_,C1,_),
  Child = n(_,_,C2,_),
  (C2 < C1 ->
    NewOpenList = [Child,Open_h|Open_t]
  ;
    insert_child(Child,Open_t,Open_t2),
    NewOpenList = [Open_h|Open_t2]
  ).


%  ----------
% searches for a possible child node and calculates its cost
search(Oracles,Task,Current,Depth,RPath,ClosedList,NewCurrent,NewDepth,NewCost,NewRPath) :-
  map_adjacent(Current,New,empty),
  \+memberchk(New,RPath),  % making sure New is not in RPath and ClosedList
  \+memberchk(New,ClosedList),
  NewCurrent = New,
  NewDepth is Depth + 1,
  total_cost(Oracles,NewCurrent,NewDepth,Task,NewCost),
  NewRPath = [NewCurrent|RPath].


%  ----------
% calculates the cost for go, with the manhattan distance as the heuristic
total_cost(Oracles,Current,Depth,Task,Cost) :-
  Task = go(Final),
  map_distance(Current, Final, Distance),
  Cost is Depth + Distance.

% calculates the cost for find, without a heuristic
total_cost(Oracles,_Current,Depth,Task,Cost) :-
  Task = find(_Final),
  Cost = Depth.

% calculates the cost for find, in the case of identify only.
% uses the average of distance to all oracles as the heuristic to
% choose paths that visit more oracles rather than closer oracles
total_cost(Oracles,Current,Depth,Task,Cost) :-
  Task = identify,
  calculate_distances(Current,Oracles,Distances),!,
  average(Distances,Average),
  Cost is Depth + Average.

% calculates the distance from a current position to all the values in a list
% i.e. the position of all the oracles/stations
calculate_distances(Current,[],Distances) :-
  Distances = [].

calculate_distances(Current,[Oracle],Distances) :-
  map_distance(Current,Oracle,Distance),
  Distances = [Distance].

calculate_distances(Current,[Oracle|Oracles],Distances) :-
  map_distance(Current,Oracle,Distance),
  calculate_distances(Current,Oracles,DistancesLeft),
  Distances = [Distance|DistancesLeft].

% for stations
calculate_distances(Current,[(Station,Pos)],Distances) :-
  map_distance(Current,Pos,Distance),
  Distances = [(Station,Distance)].

calculate_distances(Current,[(Station,Pos)|Stations],Distances) :-
  map_distance(Current,Pos,Distance),
  calculate_distances(Current,Stations,DistancesLeft),
  Distances = [(Station,Distance)|DistancesLeft].

% calculates the average of a list
average(List,Average) :-
  sumlist(List,Sum),
  length(List,Length),
  Length > 0,
  Average is Sum/Length.

% finds the location of the charging stations
find_stations(Location) :-
  between(1,20,X),
  between(1,20,Y),
  map_adjacent(p(X,Y),Pos,Object),
  Object = c(_),
  Location = (Object,Pos).

% finds the closest charging station to current position
closest_station(Current, Stations, Closest) :-
  calculate_distances(Current,Stations,Distances),!,
  sort(2,@=<,Distances,Sorted),
  Sorted = [Sorted_h|_Sorted_t],
  Sorted_h = (Closest,_Pos).