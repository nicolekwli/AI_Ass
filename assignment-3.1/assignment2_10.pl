candidate_number(10).


% finds the minimal cost of solving any task of the form (go(p(x,y))), find(o(n),c(n))
% find path using energy available (part 1)
% find path considering more stuff (part3/4)

% for example solve_task(+go(Pos), -Cost).
% fail when task not feasible/ no energy
solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  
  % proceed to find the shortest path
  solve_task_bt(Task,[c(0,0,P),P],0,R,Cost,_NewPos),!,
  reverse(R,[_Init|Path]),

  % This performs the path found 
  query_world( agent_do_moves, [Agent,Path] ).


%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve_task_bt(+Task,+Current,+Depth,-RPath,-Cost,-NewPos)
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).


%% A* algorithm
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
  Current = [c(F, G, P) | RPath],

  % IF IT IS A GO -----!! This needs changing so it accomodates find() as well !!----- 
  Task = go(Final),

  % Gets a list of possible next positions using setof involving search
  % setof(+Template, +Goal, -Set) 
    % binds Set to the list of all instances of Template satisfying the goal Goal
  setof(c(NextPos, Next_G), search(P, Final, NextPos, Next_G, 1), PossChildren), 
  write($PossChildren),

  get_best_next_move(PossChildren, RPath, Final, 99, Move),
  
  D1 is D+1,
  G1 is G + 1,

  get_head(Move, NewMove),
  NewMove = c(MovePos, MoveG),

  map_distance(MovePos, Final, H1),
  F1 is G1 + H1,

  % and we have found the next position for the agent to move to
  solve_task_bt(Task,[c(F1, G1, MovePos), MovePos | RPath],D1,RR,Cost,NewPos).  % backtrack search


%% achieved - detects when the specified task has been solved
achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(F,G,NewPos)|RPath],
  Cost is G,
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).

achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(F,G,NewPos)|RPath],
  Cost is G,
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).


% this is called by setof
search(F, Final, N, H, 1) :-
  map_adjacent(F,N,empty),
  map_distance(N, Final, H).


% This is the main bit that gets the best aka lowest cost next move 
get_best_next_move([], RPath, Final, BestG, Move).

get_best_next_move([c(Pos, Pos_g)], RPath, Final, BestG, Move) :-
  \+memberchk(Pos, RPath),
  write($Pos),
  ( \+check_if_dead_end(Pos) -> write('This is a dead end.'), Move = [c(p(0,0), 99)]
    ;( Pos == Final -> write('Goal found'), Move = [c(Pos, 0)]
        ;( Pos_g < BestG -> write('G is less than'), Move = [c(Pos, Pos_g)]
        )
    )
  ).

get_best_next_move(PossChildren, RPath, Final, BestG, Move) :-
  PossChildren = [c(Pos, Pos_g)|Children],
  get_best_next_move(Children, RPath, Final, BestG, Move_t),
  Child = c(Pos, Pos_g),
  Move_t = [c(Pos_t, Pos_tg) | Move_ts],
  
  % \not +provable,
  \+memberchk(Child, RPath),

  % Check if it is the end
      % chek if child has more childs -> if not ignore
      % check if child is in the closed list(lets say RPath)  -> if is, ignore
      % check if child is in the open list -> if not then add
        % if it is, check g to destination, if bterr change value in open list

  ( \+check_if_dead_end(Pos) -> write('This is a dead end.'), Move = [Move_t]
    ;( Pos == Final -> write('Goal found'), Move = [c(Pos, 0)]
        ;( Pos_g < Pos_tg -> write('G is less than'), Move = [Child|Move_t]
          ; Move = Move_t
        )
    )
  ).

get_head([X|_], X).


% should maybe include oracles and shits
check_if_dead_end(P) :-
  setof(Child, map_adjacent(P, Child, empty), Poss).
