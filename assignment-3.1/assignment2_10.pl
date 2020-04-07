candidate_number(10).

% finds (some poor) solutions using a (naive) depth-first search algorithm
% finds the minimal cost of solving any task of the form (go(p(x,y))), find(o(n),c(n))
% find path using energy available (part 1)
% find path considering more stuff (part3/4)

% for example solve_task(+go(Pos), -Cost).
% fail when task not feasible/ no energy
solve_task(Task,Cost):-
  %openList = [],
  %closedList = [P],
  my_agent(Agent),

  % I assume this gets P returned as the current position
  query_world( agent_current_position, [Agent,P] ),
  
  solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),

  % This performs the path found 
  query_world( agent_do_moves, [Agent,Path] ).

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
% solve_task_bt(+Task,+Current,+Depth,-RPath,-Cost,-NewPos)
% Current is of the form [c(F,P)|RPath]; 
%    RPath is a list of positions denoting the current path (in reverse) 
%       from the start position (at the end) to the current position P (at the head)
%    F is the cost of getting from the start position to P
% Depth is the current search depth

solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).


% change this from BFS to A*
% Will need to calculate manhatten distance using map_distance(+Pos1,+Pos2,-Dist)
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
  % Why make it a current thing but its not used -- omg is it to split it up into 
    % variables inside the function that we can use within the function
  Current = [c(F,P)|RPath],

  % gives adjacent locations
    % (cur_pos, new_posS, new_posS, 1? )
  % !!might want to create a new search function that gives a list of new positions instead of 1
    % !!then calculate new cost for all of them
    % !!new cost = (cost so far) F + (heuristic: manhatten distance from current to final distance) H 
      % !!then get the smallest one
        % !!them recurse
  search(P,P1,R,C),

  % is the new positions in the path we've been
  \+ memberchk(R,RPath),  % check we have not been here already

  % ------------------- NOT FINAL --------------------------------
  map_distance(P, NewPos, H),
  newCost is F + H,
  
  % Otherwise continue so we increase depth
  D1 is D+1,

  % and also cost coz we moving one more step
  F1 is F+C,

  % and we find another move with low cost
    % oh wait old one doesn't do that new one needs to
  solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos).  % backtrack search

% achieved - detects when the specified task has been solved
achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).

achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

% (cur_pos_so_far, p1, R, 1)
search(F,N,N,1) :-
  map_adjacent(F,N,empty).


%%%%%%%%%% Solution %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% The point of Part 1 is to write a predicate solve_task(+Task,-Cost) that finds the minimal cost of 
% solving any task of the following form (where integers X,Y and N are naturals):
% Your predicate should either succeed and return the minimal cost of achieving the task, 
% or it should fail if the task is not feasible (either because obstacles prevent you from reaching the target, or because you would run out of energy before you got there).

% In Part 1, your code should fail if it cannot find a path to the destination using only the energy 
% that is initially available.


% your task is to write an improved A* algorithm that finds optimal solutions using the Manhattan heuristic 
% to speed up the search when the target location is known in advance (or using breadth-first search when it is not)
