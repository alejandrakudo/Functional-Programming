% CISC 260 - Assignemnt 6
% Alejandra Kudo, 10136014 

% moveFamily predicate that takes 4 paramters
% Moves- list of moves that would take the family across the river - unbound 
% Time - list of moves would take - unbound 

moveFamily(Familyname, Maxtime, Moves, Time) :-
	family(Familyname, People),
	moveSouth(People, [], Moves, Time),
	Time =< Maxtime.

% The moveNorth predicate solves the problem from an intermediate state
% create the set of people left on the south side

moveNorth([],_,[],0).
moveNorth(NorthPeople, SouthPeople, [Mover|MoverMoves], Time) :-
	member(Mover/Time1, SouthPeople),
	select(Mover/Time1, SouthPeople, SouthRemainder),
	moveSouth([Mover/Time1|NorthPeople], SouthRemainder, MoveMoves,Time2),
	Time is Time1 + Time2.


moveSouth([],_,[],0).
moveSouth(NorthPeople, SouthPeople, [Mover1+Mover2|MoreMoves], Time) :-
	member(Mover1/Time1, NorthPeople),
	select(Mover1/Time1, NorthPeople, NorthRemainder1),
	member(Mover2/Time2, NorthRemainder1),
	select(Mover2/Time2, NorthRemainder1, NorthRemainder2),
	Mover1 @< Mover2,
	moveNorth(NorthRemainder2, [Mover1/Time1,Mover2/Time2|SouthPeople],MoreMoves,RestOfTime),
	Time is max(Time1,Time2) + RestOfTime.

