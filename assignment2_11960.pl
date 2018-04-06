candidate_number(11960).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_bt(Task,[a(c(0,0,P),[P])],0,R,Cost,[P]),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Agenda,Depth,RPath,[cost(Cost),depth(Depth)],_) :-
  achieved(Task,Agenda,RPath,Cost).
solve_task_bt(Task,[Current|ARest],D,RR,Cost,V) :-
  possible_moves(Task,Current,V,Moves),
  add_to_agenda(Moves,ARest,NewAgenda),
  add_to_visited(Moves,V,V1),
  D1 is D+1,
  solve_task_bt(Task,NewAgenda,D1,RR,Cost,V1).  % backtrack search

achieved(go(Exit),[Current|_],RPath,Cost) :-
    Current = a(c(_,Cost,_),RPath),
    ( Exit=none            -> true
    ; oracle_pos(_,Exit)   -> RPath = [Adj|_], adjacent(Exit,Adj)
    ; charging_pos(_,Exit) -> RPath = [Adj|_], adjacent(Exit,Adj)
    ; otherwise            -> RPath = [Exit|_]
    ).
achieved(find(all),[],_,_).
achieved(find(O),[Current|_],RPath,Cost) :-
    Current = a(c(_,Cost,_),RPath),
    ( O=none    -> true
    ; O=all     -> RPath = [Last|_], check_and_assert_adj(Last), fail, !
    ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
    ).

possible_moves(go(Goal), Current, Visited, Moves) :-
    (setof(Move,search_agenda(Current,Goal,Visited,Move),Moves) %Desire for Deterministic Behaviour
    ; Moves = []).

possible_moves(find(_), Current, Visited, Moves) :-
    (setof(Move,search_agenda(Current,none,Visited,Move),Moves), !
    ; Moves = []).

% search_agenda(Current, Goal, NewMove) - Current/NewMove ~ a(c(F,G,P),RPath), Goal ~ p(X,Y)
search_agenda(a(c(_,G,P),RPath), Goal, Visited, a(c(F,G1,NewPos),[NewPos|RPath])) :-
    search(P,Goal,NewPos,H),
    G1 is G+1,
    F is G1+H,
    \+ memberchk(NewPos,Visited).

search(Pos,Goal,Next,ManhattanDist) :-
    map_adjacent(Pos,Next,empty),
    (Goal=none  -> ManhattanDist = 0
    ; otherwise -> map_distance(Goal,Next,ManhattanDist)
    ).

add_to_visited([],V,V).
add_to_visited([a(c(_,_,P),_)|Moves],V2,[P|V3]) :-
    add_to_visited(Moves,V2,V3).

% Uses Assumed Sorted Moves
add_to_agenda(Moves,Agenda,NewAgenda) :-
    add_to_agenda(Moves,[],Agenda,NewAgenda).

add_to_agenda([],Top,Bottom,Agenda) :-
    append(Top,Bottom,Agenda). % Base Case
add_to_agenda([Move|Moves],T1,Bottom,FinalAgenda) :-
    add_one(Move,Bottom,T2,B),
    append(T1,T2,Top),
    add_to_agenda(Moves,Top,B,FinalAgenda).

add_one(Move,[], [Move], []).
add_one(Move,[Node|Rest],[Move],[Node|Rest]) :-
    Move = a(c(FNew,_,_),_),
    Node = a(c(FTop,_,_),_),
    FNew =< FTop.
add_one(Move,[Node|Rest], [Node|T],B) :-
    Move = a(c(FNew,_,_),_),
    Node = a(c(FTop,_,_),_),
    FNew > FTop,
    add_one(Move,Rest,T,B).

% ------------------------- PART 3 -----------------------

:- dynamic
    charging_pos/2,
    oracle_pos/2,
    oracle_visited/1.

adjacent(p(X,Y), Adj) :-
    member(M, [n,s,e,w]),
    (M = n -> X1=X,Y1 is Y+1
    ;M = s -> X1=X,Y1 is Y-1
    ;M = e -> X1 is X+1,Y1=Y
    ;M = w -> X1 is X-1,Y1=Y
    ),
    Adj = p(X1,Y1).

check_and_assert_adj(Adj) :-
    adjacent(Adj,Pos),
    query_world( check_pos, [Pos, O]),
    (O=c(ID) -> assert_once(charging_pos(ID, Pos))
    ;O=o(ID) -> assert_once(oracle_pos(ID, Pos))
    ).

assert_once(Fact) :-
    (Fact, !
    ; assertz(Fact)
    ).

find_path(Agent, Task, Path, C) :-
    query_world( agent_current_position, [Agent,Start] ),
    find_path(Agent, Task, Start, Path, C).

find_path(Task, Start, Path, C, End) :-
    solve_task_bt(Task,[a(c(0,0,Start),[Start])],0,R,[cost(C)|_],[Start]),!,  % prune choice point for efficiency
    R = [End|_Path],
    reverse(R,[_Init|Path]).

take_path(Agent, Path) :-
    query_world( agent_do_moves, [Agent,Path] ).

find_all_items(Agent) :-
    query_world( agent_current_position, [Agent,P] ),
    solve_task_bt(find(all),[a(c(0,0,P),[P])],0,_,_,[P]).

route_to_all(Agent) :-
    query_world( agent_current_energy, [Agent,Energy] ), %Get current Energy
    query_world( agent_current_position, [Agent,P] ), %Get current position
    % setof(d(Dist,Id,Pos), (oracle_pos(Id,Pos),map_distance(P,Pos,Dist)), Oracles), %Get all oracle Location (Can sort by some heuristic)
    closest_oracle(P,Id),
    route_to_all(Agent,P, Id, Energy). %Plan Route to each

% plan_routes(_,_,[],_,A,A). %No Desitination Base case
% plan_routes(Agent,Start, [d(_,_Id,Dest)|Ps], Energy, _PreviousActions, FinalActions) :-
%     plan_route(Start, Dest, End, Energy, EnergyAfter, NewActions), %Plan route for first dest
%     % append(PreviousActions, NewActions, TmpActions), %Append new actions to previous
%     do_actions(Agent,NewActions),
%     plan_routes(Agent,End, Ps, EnergyAfter, _TmpActions, FinalActions). %Plan Route to remaining Locations

route_to_all(Agent,Start, OId, Energy) :-
    plan_route(Start, OId, End, Energy, EnergyAfter, NewActions), %Plan route for first dest
    do_actions(Agent,NewActions), %Do Route
    (closest_oracle(End,NextOId) ->
        route_to_all(Agent,End,NextOId,EnergyAfter) %Plan Route to remaining Locations
    ; otherwise -> true).

plan_route(Start, OId, End, Energy, EnergyAfter, Actions) :-
    oracle_pos(OId, Dest),
    find_path(go(Dest),Start,Path1,Cost1,End1), %Find Path to P
    (Energy - Cost1 > 50 ->
        EnergyAfter is Energy - Cost1, Actions = [m(Path1),s(OId)], End = End1
    ; otherwise ->
        closest_charge(Start,c(C_Id,C_Path,C_End)),
        % find_path(go(C_Dest),Start,C_Path,_,C_End),
        find_path(go(Dest),C_End,Path2,Cost2,End2),
        EnergyAfter is 100 - Cost2, Actions = [m(C_Path),tu(C_Id),m(Path2),s(OId)], End = End2
    ).

%Finds Path... Alternatively could use euclidian distance?
closest_charge(Start, c(BestId,BestPath,BestEnd)) :-
    setof(c(Cost,Id,Path,End), P^(charging_pos(Id,P),find_path(go(P),Start,Path,Cost,End)), Stations),
    Stations = [c(_,BestId,BestPath,BestEnd)|_].

unvisited_oracle(Id,P) :-
    oracle_pos(Id,P),
    \+ oracle_visited(Id).

closest_oracle(Start, BestId) :-
    setof(o(Dist,Id), P^(unvisited_oracle(Id,P),map_distance(Start,P,Dist)),Oracles),
    Oracles = [o(_,BestId)|_].

% Do actions returned by route planner
do_actions(_,[]).
do_actions(Agent, [Action|Rest]) :-
    (Action = m(Path)   -> take_path(Agent,Path)                             %Move Path
    ;Action = s(Id)     -> assert_once(oracle_visited(Id))                   %Talk to Oracle
    ;Action = tu(Id)    -> query_world( agent_topup_energy, [Agent, c(Id)])  %Top Up
    ),
    do_actions(Agent, Rest).

board_pos(p(X,Y)) :-
    between(1,20,X),
    between(1,20,Y).

scan_all_items() :-
    bagof(Tile,board_pos(Tile),Ps),
    bagof(obj(O,P), (member(P,Ps),query_world( check_pos, [P, O])), Objects),
    store_locations(Objects).

store_locations([]).
store_locations([obj(O,P)|Rest]) :-
    (O=c(Id)   -> assert_once(charging_pos(Id,P))
    ;O=o(Id)   -> assert_once(oracle_pos(Id,P))
    ;otherwise -> true),
    store_locations(Rest).

% Main Solve Function
solve() :-
    forget_all(),
    my_agent(Agent),
    try_and_try_again(
        find_and_go(Agent)
    ).

find_and_go(Agent) :-
    (part_module(4) -> scan_all_items()
    ;otherwise      -> find_all_items(Agent)),
    route_to_all(Agent).

try_and_try_again(Pred) :-
    (Pred, !
    ; try_and_try_again(Pred)).

forget_all() :-
    retractall(charging_pos(_,_)),
    retractall(oracle_pos(_,_)),
    retractall(oracle_visited(_)).

reload() :-
  make(), reset_game(), start_game(),
  forget_all().
