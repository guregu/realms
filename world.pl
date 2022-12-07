:- module(world, [location/2]).

:- dynamic(location/2).
:- initialization(init).

init :-
	spawn(player),
	format("Ready!~n", []).

starting_room(1). % room id

spawn(Mob) :-
	starting_room(RoomID),
	spawn(Mob, RoomID).
spawn(Mob, RoomID) :-
	\+location(Mob, _),
	assertz(location(Mob, RoomID)).

move(Thing, Direction) :-
	location(Thing, RoomID),
	room_exit(RoomID, Direction, Destination),
	move_(Thing, Destination).

move_(Thing, RoomID) :-
	retract(location(Thing, _)),
	assertz(location(Thing, RoomID)).