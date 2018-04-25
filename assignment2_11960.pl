candidate_number(11960).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_bt(Task,[a(c(0,0,P),[P])],0,R,Cost,[P],400),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Agenda,Depth,RPath,[cost(Cost),depth(Depth)],_,MaxDepth) :-
  achieved(Task,Agenda,RPath,Cost,MaxDepth).
solve_task_bt(Task,[Current|ARest],D,RR,Cost,V,MaxDepth) :-
  possible_moves(Task,Current,V,Moves),
  add_to_agenda(Moves,ARest,NewAgenda),
  add_to_visited(Moves,V,V1),
  D1 is D+1,
  solve_task_bt(Task,NewAgenda,D1,RR,Cost,V1,MaxDepth).  % backtrack search

achieved(go(Exit),[Current|_],RPath,Cost,MaxDepth) :-
    Current = a(c(_,Cost,_),RPath),
    ( Exit=none            -> true
    ; Cost > MaxDepth      -> true
    ; oracle_pos(_,Exit)   -> RPath = [Adj|_], adjacent(Exit,Adj)
    ; charging_pos(_,Exit) -> RPath = [Adj|_], adjacent(Exit,Adj)
    ; otherwise            -> RPath = [Exit|_]
    ).
achieved(find(all),[],_,_,_).
achieved(find(O),[Current|_],RPath,Cost,_MaxDepth) :-
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

adjacent(p(X,Y), p(X1,Y1)) :-
    member(M, [n,s,e,w]),
    (M = n -> X1=X,Y1 is Y+1
    ;M = s -> X1=X,Y1 is Y-1
    ;M = e -> X1 is X+1,Y1=Y
    ;M = w -> X1 is X-1,Y1=Y
    ).

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

find_path(Task, Start, Path, C, End) :-
    find_path(Task, Start, Path, C, End, 400).
find_path(Task, Start, Path, C, End, Max) :-
    solve_task_bt(Task,[a(c(0,0,Start),[Start])],0,R,[cost(C)|_],[Start], Max),!,  % prune choice point for efficiency
    R = [End|_Path],
    reverse(R,[_Init|Path]).

take_path(Agent, Path) :-
    query_world( agent_do_moves, [Agent,Path] ).

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

:- discontiguous talk/2.

talk(Agent, Id) :-
    query_world(agent_ask_oracle, [Agent, o(Id), link, L]),
    assert_once(oracle_link(L)),
    assert_once(oracle_visited(Id)).

% Utitily to clear dynamic predicates
forget_all() :-
    retractall(charging_pos(_,_)),
    retractall(oracle_pos(_,_)),
    retractall(oracle_visited(_)),
    retractall(oracle_unreachable(_)),
    retractall(oracle_link(_)).

reload() :-
  make(), reset_game(), start_game(),
  forget_all().

% ----------------- PART 4 --------------------
:- dynamic
    unreachable_oracle/1.

find_all_items(Agent) :-
    query_world( agent_current_position, [Agent,P] ),
    solve_task_bt(find(all),[a(c(0,0,P),[P])],0,_,_,[P], 4000).

scan_all_items(Agent) :-
    query_world( agent_current_position, [Agent,P] ),
    bagof(A, adjacent(P,A), Adjs),
    scan_pulse([P|Adjs],[P|Adjs],all), !. % We want it to succeed but there isn't a need for backtracking

scan_for_item(Agent, Goal) :-
    query_world( agent_current_position, [Agent,P] ),
    bagof(A, adjacent(P,A), Adjs),
    scan_pulse([P|Adjs],[P|Adjs],Goal), !. % We want it to succeed but there isn't a need for backtracking

scan_pulse([],_,_).
scan_pulse(Items,V1, Goal) :-
    findall(obj(O,P), (member(P,Items),query_world( check_pos, [P, O])), Objects),
    (store_locations(Objects, Goal)
    ;otherwise ->
        adjacent_to_objs(Objects, NewItems, V1),
        append(NewItems,V1,V2),
        scan_pulse(NewItems,V2, Goal)
    ).

adjacent_to_objs(Objs,Final,Visited) :- adjacent_to_objs(Objs, [], Final, Visited).
adjacent_to_objs([],A,A,_).
adjacent_to_objs([obj(_O,L)|Rest], A1, Final, V1) :-
    bagof(P, L^(adjacent(L,P), \+ memberchk(P,V1)), A2),
    append(A1, A2, A3),
    append(A2, V1, V2),
    adjacent_to_objs(Rest, A3, Final, V2).

% store_locations([],Goal).
store_locations([obj(O,P)|Rest],Goal) :-
    (O=c(Id)   -> retractall(charging_pos(Id,_)),assert_once(charging_pos(Id,P))
    ;O=o(Id)   -> retractall(oracle_pos(Id,_)),assert_once(oracle_pos(Id,P))
    ;otherwise -> true),
    (O=Goal
    ;otherwise -> store_locations(Rest,Goal)
    ).

agent_pos(Agent,P) :-
    query_world( agent_current_position, [Agent,P] ).

agent_energy(Agent,E) :-
    query_world( agent_current_energy, [Agent,E] ).

agent_topup(Agent,Id) :-
    query_world( agent_topup_energy, [Agent, c(Id)]).

visited_all(_) :-
    \+ bagof(P, Id^unvisited_oracle(Id,P), _).

reachable_unvisited_oracle(Id,P) :-
    unvisited_oracle(Id,P),
    \+ unreachable_oracle(Id).

find_closest(Start, BestId, BestDist) :-
    setof(o(Dist,Id), P^(reachable_unvisited_oracle(Id,P),map_distance(Start,P,Dist)),Oracles),
    Oracles = [o(BestDist,BestId)|_].

% Uses oracle(Id) to find fastest path. Path ~ path(Path, Cost)
% True -> Path = a path to Oracle (if path is queried it will be the fastest)
% Fail -> oracle Not reachable
reachable(Start, Id, path(Path,Cost), Dist) :-
    oracle_pos(Id, Dest),
    MaxDepth is Dist * 2,
    find_path(go(Dest),Start,Path,Cost,_End, MaxDepth).

% Scan and stop at objective
find_object(Agent, O) :-
    scan_for_item(Agent, O).

% Attempts to take path to closest charge station
recharge(Agent) :-
    agent_pos(Agent,P),
    closest_charge(P, c(Id,Path,_End)),
    (take_path(Agent, Path), agent_topup(Agent,Id), !
    ;recharge(Agent)
    ).


% Dynamically goes to Id.
% True -> oracle(Id) was visited and spoken too
% Fail -> oracle(Id) is unreachable at run time
go(Agent, Id) :-
    agent_pos(Agent,P),
    reachable(P,Id,Path),
    go(Agent,Id,Path).

% Initial Path is for efficiency as we have calculated it in reachable before
go(Agent, Id, path(Path,Cost)) :-
    agent_energy(Agent,E),
    EnergyAfter is E-Cost,
    (EnergyAfter > 40 ->
        (take_path(Agent, Path), !
        ; go(Agent,Id))
    ;otherwise ->
        recharge(Agent), go(Agent,Id)
    ),
    agent_pos(Agent,P),
    (map_adjacent(P,_,o(Id)), !
    ;otherwise -> find_object(Agent, o(Id)), go(Agent, Id)
    ).

% Recursively Visits every oracle
step_p4(Agent, L) :-
    \+ visited_all(Agent), \+ whoami(_I),
    agent_pos(Agent, CurrentPos),
    (find_closest(CurrentPos,Id,Dist),
      (reachable(CurrentPos,Id,Path,Dist) -> go(Agent, Id, Path), talk(Agent,Id)
      ; otherwise -> assert(unreachable_oracle(Id))
      ), L2 is L
    ;L < 10 -> retractall(unreachable_oracle(_)), L2 is L+1
    ), !,
    step_p4(Agent, L2).

solve(I) :-
    forget_all(),
    my_agent(Agent),
    ( part_module(4) -> scan_all_items(Agent)
    ; otherwise -> find_all_items(Agent)
    ),
    \+ step_p4(Agent, 0),
    whoami(I), !.


% ------------- OLD -----------------
% Do actions returned by route planner
% do_actions(_,[]).
% do_actions(Agent, [Action|Rest]) :-
%     (Action = m(Path)   -> take_path(Agent,Path)                             %Move Path
%     ;Action = s(Id)     -> talk(Agent, Id)                                   %Talk to Oracle
%     ;Action = tu(Id)    -> query_world( agent_topup_energy, [Agent, c(Id)])  %Top Up
%     ),
%     do_actions(Agent, Rest).

% Main Solve Function
% solve() :-
%     forget_all(),
%     my_agent(Agent),
%     try_and_try_again(
%         find_and_go(Agent)
%     ).
%
% find_and_go(Agent) :-
%     (part_module(4) -> scan_all_items(Agent)
%     ;otherwise      -> find_all_items(Agent)),
%     route_to_all(Agent).
%
% try_and_try_again(Pred) :-
%     (Pred, !
%     ; try_and_try_again(Pred)).


% route_to_all(Agent) :-
%     query_world( agent_current_energy, [Agent,Energy] ), %Get current Energy
%     query_world( agent_current_position, [Agent,P] ), %Get current position
%     closest_oracle(P,Id),
%     route_to_all(Agent,P, Id, Energy). %Plan Route to each
%
% route_to_all(Agent,Start, OId, Energy) :-
%     plan_route(Start, OId, End, Energy, EnergyAfter, NewActions), %Plan route for first dest
%     do_actions(Agent,NewActions), %Do Route
%     (closest_oracle(End,NextOId) ->
%         route_to_all(Agent,End,NextOId,EnergyAfter) %Plan Route to remaining Locations
%     ; otherwise -> true).
%
% plan_route(Start, OId, End, Energy, EnergyAfter, Actions) :-
%     oracle_pos(OId, Dest),
%     find_path(go(Dest),Start,Path1,Cost1,End1), %Find Path to P
%     (Energy - (Cost1 + 10) > 50 ->
%         EnergyAfter is Energy - Cost1, Actions = [m(Path1),s(OId)], End = End1
%     ; otherwise ->
%         closest_charge(Start,c(C_Id,C_Path,C_End)), %Nearest Charge Station
%         closest_oracle(C_End, NewOId), oracle_pos(NewOId, NewDest), %Closest Oracle to Charge Station
%         find_path(go(NewDest),C_End,Path2,Cost2,End2), %Take path
%         EnergyAfter is 100 - Cost2, Actions = [m(C_Path),tu(C_Id),m(Path2),s(OId)], End = End2
%     ).
