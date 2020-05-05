candidate_number(10).


% finds the minimal cost of solving any task of the form (go(p(x,y))), find(o(n),c(n))
% find path using energy available (part 1)
% find path considering more stuff (part3/4)

% for example solve_task(+go(Pos), -Cost).
% fail when task not feasible/ no energy
solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  query_world(agent_current_energy, [Agent, Energy]),

  ClosedList = [p(1,1)],
  
  % proceed to find the shortest path
  solve_task_bt(Energy, Task,[c(0,0,P),P], ClosedList, 0,R,Cost,_NewPos),!,
  reverse(R,[_Init|Path]),

  % This performs the path found 
  query_world( agent_do_moves, [Agent,Path] ).

solve_task_bt(Energy, Task,Current, ClosedList, Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).


% A* algorithm
solve_task_bt(Energy, Task,Current, ClosedList, D,RR,Cost,NewPos) :-

  % IF IT IS A GO -----!! This needs changing so it accomodates find() as well !!----- 
  Task = go(Final),
  Current = [c(F, G, P) | RPath],
  RPath = [RPath_h| RPath_t],

  ClosedList = [p(Pos_cx, Pos_cy) | Pos_ct],

  % Gets a list of possible next positions using setof involving search
  % setof(+Template, +Goal, -Set) 
    % binds Set to the list of all instances of Template satisfying the goal Goal
  setof(c(NextPos, Next_H), search(P, Task, NextPos, D, Next_H, 1), PossChildren), 
  write($PossChildren),
    
  get_best_next_move(PossChildren, RPath, Final, ClosedList, 99, Move),
  
  D1 is D+1,
  G1 is G + 1,
  
  get_head(Move, NewMove),
  NewMove = c(MovePos, MoveG),
  write($NewMove),
 
  ( MoveG == 999 -> write('NEED TO BACKTRACK TO LAST POS'), write($MovePos), 
                                                            RPath_t = [RPath_th | RPath_tt],
                                                            % create new predicate taking in task, return 0 if find
                                                            calc_dist(RPath_h, Task, H1),
                                                            % map_distance(RPath_h, Final, H1), 
                                                            F1 is G1 + H1, 
                                                            NextMovePos = RPath_th, 
                                                            write($NextMovePos),
                                                            
                                                            RPathNew = RPath_tt,

                                                            ClosedListNew = [P|ClosedList]
                                                                             
    ;calc_dist(MovePos, Task, H1), F1 is G1 + H1, NextMovePos = MovePos, RPathNew = RPath, ClosedListNew = ClosedList
  ),
  % and we have found the next position for the agent to move to
  solve_task_bt(Energy, Task,[c(F1, G1, NextMovePos), NextMovePos | RPathNew], ClosedListNew, D1,RR,Cost,NewPos).


% this does depth
solve_task_bt(Energy, Task,Current, ClosedList, D,RR,Cost,NewPos) :-
  Task = find(Final),
  Current = [c(F, G, P) | RPath],
  RPath = [RPath_h| RPath_t],
  write('\nPATH'),
  write(RPath),
  
  ClosedList = [p(Pos_cx, Pos_cy) | Pos_ct],

  setof(c(NextPos, Next_H, NextObj), search(P, Task, NextPos, D,Next_H, NextObj, 1), PossChildren), 

  write('\nPOsSCHILD'),
  write(PossChildren),

  get_best_next_move(PossChildren, RPath, Final, ClosedList, 999, Move),
  
  get_head(Move, NewMove),
  NewMove = c(MovePos, MoveG),
  ( Energy < MoveG -> Backtrack is 999
    ; write('no need to backtrack'), Backtrack is MoveG
  ),
  
  ( Backtrack == 999 -> write('NEED TO BACKTRACK TO LAST POS'), G1 is G-1, 
                                                            RPath_t = [RPath_th | RPath_tt],
                                                            calc_dist(RPath_h, Task, H1),
                                                            F1 is G1 + H1, 
                                                            NextMovePos = RPath_th, 
                                                            RPathNew = RPath_tt,
                                                            ClosedListNew = [P|ClosedList],
                                                            D1 is D-1

                                                                            
    ;calc_dist(MovePos, Task, H1), G1 is G+1,F1 is G1 + H1, NextMovePos = MovePos, RPathNew = RPath, ClosedListNew = ClosedList, D1 is D+1
  ),
  solve_task_bt(Energy, Task,[c(F1, G1, NextMovePos), NextMovePos | RPathNew], ClosedListNew, D1,RR,Cost,NewPos).

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
search(F, Task, N, D, H, 1) :-
  Task = go(Final),
  map_adjacent(F,N,empty),
  map_distance(N, Final, H).

search(F, Task, N, D, D, NextObj, 1) :-
  Task = find(Final),
  map_adjacent(F,N,NextObj),
  write(N).

search(P, Task, Pos, NextObj) :-
  Task = find(Final),
  map_adjacent(P, Pos, NextObj).

calc_dist(P, Task, H) :-
  Task = go(Final),
  map_distance(P, Final, H).

calc_dist(P, Task, 0) :-
  Task = find(Final).


% This is the main bit that gets the best aka lowest cost next move 
get_best_next_move([], RPath, Final, ClosedList, BestH, Move).


get_best_next_move([c(Pos, Pos_h, Object) | []], RPath, Final, [], BestH, Move) :-
  %write('rpathA\n'),
  %write($RPath),
  write('current!'),
  write($Pos),
  
  ( Object == empty ->
          (\+check_if_in_path(Pos, ClosedList) -> write('IS IN CLOSED LIST'), Move = [c(Pos, 999)]
            ;( \+check_if_in_path(Pos, RPath) -> write('IT IS IN PATH'), Move = [c(Pos, 999)]
              ;( \+check_if_dead_end(Pos) -> write('This is a dead end.'), Move = [c(p(0,0), 999)]
                ;( Pos == Final -> write('Goal found'), Move = [c(Pos, 0)]
                  ;( Object == Final -> write('OBJECT FOUND'), Move = [c(Pos, 0)]
                    ;( Pos_h < BestH -> write('G is less than1'), Move = [c(Pos, Pos_h)]
                    )
                  )
                )  
              )
            )
          )
    ; (Object == Final -> write('OBJECT FOUND'), Move = [c(Pos, 0)]
      ;Move = [c(Pos, 999)]
    )
  ).

get_best_next_move([c(Pos, Pos_h, Object) | []], RPath, Final, ClosedList, BestH, Move) :-
  %write('rpathA\n'),
  %write($RPath),

  write('current1'),
  write($Pos),

  %write('\nCLSOED LIST1'),
  %write($ClosedList),
  ( Object == empty ->
    (\+check_if_in_path(Pos, ClosedList) -> write('IS IN CLOSED LIST'), Move = [c(Pos, 999)]
      ;( \+check_if_in_path(Pos, RPath) -> write('IT IS IN PATH'), Move = [c(Pos, 999)] 
        ;( \+check_if_dead_end(Pos) -> write('This is a dead end.'), Move = [c(p(0,0), 999)]
          ;( Pos == Final -> write('Goal found'), Move = [c(Pos, 0)]
            ;( Object == Final -> write('OBJECT FOUND'), Move = [c(Pos, 0)]
              ;( Pos_h < BestH -> write('G is less than1'), Move = [c(Pos, Pos_h)]
              )
            )
          )
        )
      )
    )
  ; (Object == Final -> write('OBJECT FOUND'), Move = [c(Pos, 0)]
    ;Move = [c(Pos, 999)]
  )
).

% get_best_next_move([c(Pos, Pos_g)|Children], RPath, Final, BestG, Move) :-
get_best_next_move([c(Pos, Pos_h, Object)|Children], RPath, Final, ClosedList, BestH, Move) :-
  %write('rpathB\n'),
  %write($RPath),
  %PossChildren = [c(Pos, Pos_g)|Children],
  get_best_next_move(Children, RPath, Final, ClosedList, BestH, Move_t),

  Child = c(Pos, Pos_h),
  Move_t = [c(Pos_t, Pos_th) | Move_ts],
  
  write('current2'),
  write($Pos),
  write($Pos_h),

  write('current_TAIL'),
  write($Pos_t),
  write($Pos_th),

  %write('\nCLSOED LIST2'),
  %write($ClosedList),

  % Check if it is the end
      % chek if child has more childs -> if not ignore
      % check if child is in the closed list(lets say RPath)  -> if is, ignore
      % check if child is in the open list -> if not then add
        % if it is, check g to destination, if bterr change ,value in open list
  ( Object == empty ->
    (\+check_if_in_path(Pos, ClosedList) -> write('IS IN CLOSED LIST'), Move = Move_t
      ;( \+check_if_in_path(Pos, RPath) -> write('IT IS IN PATH'), Move = Move_t
        ;( \+check_if_dead_end(Pos) -> write('This is a dead end.'), Move = [Move_t]
          ;( Pos == Final -> write('Goal found'), Move = [c(Pos, 0)]
            ;( Object == Final -> write('OBJECT FOUND'), Move = [c(Pos, 0)]
              ;( Pos_h < Pos_th -> write('G is less than2'), Move = [Child|Move_t]
                ; write('THIS IS THE POS IT BREAK'), write(Pos), Move = Move_t
              )
            )
          )
        )
      )
    )
  ; (Object == Final -> write('OBJECT FOUND'), Move = [c(Pos, 0)]
    ;Move = Move_t
  )
).

get_head([X], X).
get_head([X|_], X).


check_if_in_path(Pos, RPath) :-
  \+memberchk(Pos, RPath).
% should maybe include oracles and shits
check_if_dead_end(P) :-
  setof(Child, map_adjacent(P, Child, empty), Poss).
  
