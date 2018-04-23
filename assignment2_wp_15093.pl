% candidate_number(15093).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> find_identity_o(A)
  ).

find_identity_2(A):-
    bagof(A,actor(A),Actors),
    get_sorted_costs([find(o(1)),find(o(2)),find(o(3)),find(o(4)),find(o(5)),find(o(6)),find(o(7)),find(o(8)),find(o(9)),find(o(10)),find(o(11)),find(o(12)),find(o(13))],Costs_o),
    find_identity_o(Actors,Costs_o,Me),
    A = Me.

find_identity_o(A):-
  A='Not yet implemented'.

test(A):-
	writeln(A).
