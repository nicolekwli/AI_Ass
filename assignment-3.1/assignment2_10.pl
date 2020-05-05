candidate_number(10).


% finds the minimal cost of solving any task of the form (go(p(x,y))), find(o(n),c(n))
% find path using energy available (part 1)
% find path considering more stuff (part3/4)

% for example solve_task(+go(Pos), -Cost).
% fail when task not feasible/ no energy
solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),

  ClosedList = [P],

  % Nodes are in the form n(Current position,Depth,Cost of path,RPath)
  Start = n(P,0,0,[P]),
  a_star(Task,[Start],ClosedList,R,Cost),!,

  reverse(R,[_Init|Path]),

  % This performs the path found
  query_world( agent_do_moves, [Agent,Path] ).

% base case for go
a_star(Task,OpenList,ClosedList,ReturnPath,TotalCost) :-
  Task = go(Final),
  OpenList = [Open_h|Open_t],
  Open_h = n(Current,Depth,Cost,RPath),
  Current = Final,
  ReturnPath = RPath,
  TotalCost = Cost.

% base case for find
a_star(Task,OpenList,ClosedList,ReturnPath,TotalCost) :-
  Task = find(Final),
  OpenList = [Open_h|Open_t],
  Open_h = n(Current,Depth,Cost,RPath),
  map_adjacent(Current,_,Final),
  ReturnPath = RPath,
  TotalCost = Cost.

% recursive case
a_star(Task,OpenList,ClosedList,ReturnPath,TotalCost) :-
  OpenList = [Open_h|Open_t],
  Open_h = n(Current,Depth,Cost,RPath),

  % write($OpenList),write('\n'),

  %creates a list of all possible children
  findall(n(NewCurrent,NewDepth,NewCost,NewRPath),search(Task,Current,Depth,RPath,ClosedList,NewCurrent,NewDepth,NewCost,NewRPath),Children),

  % if the list of children is empty, it's a dead end so add the position to ClosedList
  length(Children,L),
  (L < 1 ->
    NewClosedList = [Current|ClosedList]
  ;
    NewClosedList = ClosedList
  ),

  % manually insert all children into the OpenList, maintaining the order they're in
  insert_children(Children,Open_t,NewOpenList),

  a_star(Task,NewOpenList,NewClosedList,ReturnPath,TotalCost).

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

% searches for a possible child node and calculates its cost
search(Task,Current,Depth,RPath,ClosedList,NewCurrent,NewDepth,NewCost,NewRPath) :-
  map_adjacent(Current,New,empty),
  \+memberchk(New,RPath),
  \+memberchk(New,ClosedList),
  NewCurrent = New,
  NewDepth is Depth + 1,
  total_cost(NewCurrent,NewDepth,Task,NewCost),
  NewRPath = [NewCurrent|RPath].

% calculates the cost for go, with the manhattan distance as the heuristic
total_cost(Current,Depth,Task,Cost) :-
  Task = go(Final),
  map_distance(Current, Final, Distance),
  Cost is Depth + Distance.

% calculates the cost for find, without a heuristic
total_cost(Current,Depth,Task,Cost) :-
  Task = find(Final),
  Cost = Depth.