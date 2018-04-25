% candidate_number(11960).

find_identity(I) :-
  ( part_module(2) -> find_identity_2(I)
  ; otherwise      -> find_identity_o(I)
  % ; part_module(4) -> find_identity_4(I)
  ).

%%%%%%%%%% Part 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_links(Possibles, Actor, Output) :-
	bagof(L, (wp(Actor, W),wt_link(W,L)), LinkList),
	intersection(Possibles, LinkList, Output).

pairs(Pairs) :-
    bagof(A, actor(A), Actors),
	bagof(L, link(L), Links),
	maplist(find_links(Links), Actors, ActorLinks),
	pairs_keys_values(Pairs, Actors, ActorLinks).

find_identity_2(Final) :-
    pairs(Pairs),
    find_identity_2(Pairs, Final).
find_identity_2([(Key-_Value)|[]], Key).
find_identity_2(Pairs, Final) :-
    agent_ask_oracle(oscar, o(_), link, L),
    include(check(L), Pairs, Out),
    find_identity_2(Out, Final), !.

check(L, _Key-Value) :-
	memberchk(L,Value).

%%%%%%%%%% Part 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%% Part 3/4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic
    oracle_link/1.

solve_identity(Final) :-
    pairs(Pairs),
    bagof(L, oracle_link(L), Links),
    solve_identity(Pairs, Links, Final).
solve_identity([(Key-_Value)|[]], _, Key).
solve_identity(_,[],_):- fail.
solve_identity(Pairs, [L|Links], Final) :-
    include(check(L), Pairs, Out),
    solve_identity(Out, Links, Final), !.

whoami(I) :-
    solve_identity(I).

find_identity_o(I) :-
	solve(I).

%%%%%%%%%% Part 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%% Part 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find_identity_4(I) :-
% 	bagof(A, actor(A), Actors),
% 	bagof(L, link(L), Links),
% 	maplist(find_links(Links), Actors, ActorLinks),
% 	pairs_keys_values(Pairs, Actors, ActorLinks),
% 	query_part4(Pairs, I),
% 	!,writeln(I).
%
% %%%%%%%%%% Part 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
% %find and query oracles
% query_part3([X-_|Xs], X) :-
% 	Xs = [].
%
% query_part3(Pairs, I) :-
%     agent_current_energy(oscar, E),
%
% 	% if energy is below 50, we will let the agent go to a charging station
%
%     (E < 50 -> solve_task(find(c(A)),_),
%      agent_topup_energy(oscar,c(A)),
%      Out = Pairs, Found = c(A);
%
%      otherwise -> solve_task(find(o(A)),_),
%      agent_ask_oracle(oscar, o(A), link, L),
%      include(check(L), Pairs, Out),Found=o(A)),
%
%     query_part3(Out, I).
%
% query_part4([X-_|Xs], X) :-
% 	Xs = [].
%
% query_part4(Pairs, I) :-
% 	my_agent(Agent),
% 	query_world(agent_current_energy, [Agent, E] ),
%
% 	% if energy is below 50, we will let the agent go to a charging station
%
%     (E < 50 -> writeln('need to charge'),
% 	(solve_task(find(c(A)),_),search_adjacent(c(A)) -> true;
% 	otherwise -> writeln('encounter obstacles (find(c)) - re-query_part4'),query_part4(Pairs, I)),
%      query_world(agent_topup_energy,[Agent,c(A)]),
%      Out = Pairs, Found = c(A);
%
%      otherwise -> writeln('need to find a new oracle'),
% 	 (solve_task(find(o(A)),_),search_adjacent(o(A)) -> true;
% 	 otherwise -> writeln('encounter obstacles (find(o)) - re-query_part4'),query_part4(Pairs, I)),
% 	 query_world(agent_ask_oracle,[Agent, o(A), link, L]),
%      include(check(L), Pairs, Out),Found=o(A)),
%
%     query_part4(Out, I).
