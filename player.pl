:- module(player, []).
:- use_module(library(format)).

:- dynamic(location/2).
:- initialization(init).

starting_room(1). % room id

init :-
	starting_room(ID),
	assertz(location(player, ID)),
	format("Ready!~n", []).

command(go(Direction)) --> "go", space, direction(Direction).
command(look) --> "look".
command(look(Object)) --> "look at", space, object(Object).
command(take(Object)) --> "take", space, object(Object).
command(use(Object)) --> "use", space, object(Object).
command(help) --> "help".
command(quit) --> "quit".
direction(Dir) --> string(Dir).
object(Thing) --> string(Thing).
string(Cs) --> seq(Cs).
space --> " ".

handle_input :-
	format("> ", []),
	getline(Line),
	(  phrase(command(Cmd), Line) 
	-> execute(Cmd)
	;  format("Huh?~n", [])
	),
	handle_input.

execute(go(Direction)) :-
	move(player, Direction),
	location(player, RoomID),
	look_at(RoomID).
execute(look) :-
	location(player, RoomID),
	look_at(RoomID).
execute(look(Target)) :-
	location(player, RoomID),
	room_detail(RoomID, Target, Detail),
	format("~s~n", [Detail]).
execute(look(Target)) :-
	location(player, RoomID),
	\+room_detail(RoomID, Target, _),
	format("You don't see that anywhere.~n", []).
execute(help) :- format("Commands: go <dir>, look, look at <thing>, quit.~n", []).
execute(quit) :- format("Bye!~n", []), halt.
execute(Cmd) :- format("Sorry, I don't understand '~w'.~n", [Cmd]).

look_at(RoomID) :-
    room_name(RoomID, RoomName),
	describe_room(RoomID, RoomDescription),
    % room_description(RoomID, RoomDescription),
    format("\e[4m~s\e[0m~n~s~nExits: ", [RoomName, RoomDescription]),
    findall(Exit, (room_exit(RoomID, Exit0, _), atom_chars(Exit, Exit0)), Exits),
    atomic_list_concat(Exits, ', ', ExitsStr),
    format("~w.~n", [ExitsStr]).

move(Thing, Direction) :-
	location(Thing, RoomID),
	room_exit(RoomID, Direction, Destination),
	move_(Thing, Destination),
	format("You head ~s.~n", [Direction]),
	!.
move(Thing, Direction) :-
	location(Thing, RoomID),
	\+room_exit(RoomID, Direction, _),
	format("You can't go ~s.~n", [Direction]).

move_(Thing, RoomID) :-
	retract(location(Thing, _)),
	assertz(location(Thing, RoomID)).


% TODO: refactor this ↓
describe_room(RoomID, Result) :-
	room_words(RoomID, Words),
	maplist(emphasize(RoomID), Words, Emphasized),
	phrase(strings(Emphasized), Result).
room_words(RoomID, Words) :-
	room_description(RoomID, Desc),
	split_string(Desc, " ", "", Words).
emphasize(RoomID, Noun, Str) :-
	phrase(format_("\e[1m~s\e[0m", [Noun]), Str),
	room_detail(RoomID, Noun, _),
	!.
emphasize(_, Noun, Noun).
strings([Word|Words]) --> string(Word), " ", strings(Words).
strings([]) --> [].