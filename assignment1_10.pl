candidate_number(10).

q1(P) :-
    ailp_start_position(P).

q2a(P) :-
    new_pos(p(1,1), e, P).

q2b(136).

q3([s,e,w,n]).

q4a([p(3,1),p(3,2),p(3,3),p(3,4)]).

q4b([p(3,1),p(3,2),p(3,3),p(3,4),p(4,4)]).

q4c([p(3,1),p(3,2),p(3,3),p(2,3),p(2,2),p(2,1),p(1,1),p(1,2),p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1)]).

q4d([p(3,1),p(3,2),p(2,2),p(2,1),p(1,1),p(1,2),p(1,3),p(1,4),p(2,4),p(2,3),p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1)]).

q5_corner_move() :-
    ailp_start_position(P),
    ailp_show_move(P, p(1,1)),
    ailp_show_move(p(1,1), p(1,4)),
    ailp_show_move(p(1,4), p(4,4)),
    ailp_show_move(p(4,4), p(4,1)).

q5_corner_move2() :-
    ailp_start_position(P),
    ailp_grid_size(N),
    ailp_show_move(P, p(1,1)),
    ailp_show_move(p(1,1), p(1,N)),
    ailp_show_move(p(1,N), p(N,N)),
    ailp_show_move(p(N,N), p(N,1)).

next_spiral(s, e).
next_spiral(s, w).
next_spiral(e, n).
next_spiral(e, s).
next_spiral(n, w).
next_spiral(n, e).
next_spiral(w, s).
next_spiral(w, n).

is_corner(P) :-
  ailp_grid_size(N),
  (P = p(1,1) ;
  P = p(1,N) ;
  P = p(N,1) ;
  P = p(N,N)).

init_move(p(1,1),s).
init_move(p(1,1),e).
init_move(p(N,1),w) :- ailp_grid_size(N).
init_move(p(N,1),s) :- ailp_grid_size(N).
init_move(p(N,N),n) :- ailp_grid_size(N).
init_move(p(N,N),w) :- ailp_grid_size(N).
init_move(p(1,N),e) :- ailp_grid_size(N).
init_move(p(1,N),n) :- ailp_grid_size(N).

q6_spiral(L) :-
  is_corner(P),
  q6_spiral(P,L).

q6_spiral(P,L) :-
  init_move(P,M),
  q6_spiral(P,[P],Ps,M),
  reverse(Ps,L).

q6_spiral(_,Ps,Ps,Prev) :- complete(Ps).

q6_spiral(P,Ps,R,Prev) :-
  next_spiral(Prev, Post),
  (new_pos(P,Prev,P1) ->
    (memberchk(P1,Ps) ->
      new_pos(P,Post,P2),
      \+ memberchk(P2,Ps),
      ailp_show_move(P,P2),
      q6_spiral(P2,[P2|Ps],R,Post)
    ;
      ailp_show_move(P,P1),
      q6_spiral(P1,[P1|Ps],R,Prev)
    )
  ; new_pos(P,Post,P1),
    \+ memberchk(P1,Ps),
    ailp_show_move(P,P1),
    q6_spiral(P1,[P1|Ps],R,Post)
  ), !.