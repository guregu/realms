:- module(player, []).
:- use_module(library(format)).


command(go(Direction)) --> "go", space, direction(Cs), { atom_chars(Direction, Cs) }.
command(look) --> "look" | "l".
command(look(Object)) --> ("look at" | "examine" | "x"), space, object(Object).
command(take(Object)) --> "take", space, object(Object).
command(use(Object)) --> "use", space, object(Object).
command(help) --> "help".
command(quit) --> "quit" | "q".
command(unknown(Cmd)) --> string(Cmd).
direction(Dir) --> string(Dir).
object(Thing) --> string(Thing).
string([C|Cs]) --> [C], string(Cs).
string([]) --> [].
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
execute(unknown(Cs)) :-
	% shorthand for directions: "west" instead of "go west"
	atom_chars(Direction, Cs),
	location(player, RoomID),
	room_exit_canonical(RoomID, Direction, Canon),
	execute(go(Canon)).
execute(unknown(Cmd)) :- format("Sorry, I don't understand ~q.~n", [Cmd]).
execute(Cmd) :- format("Unhandled command: ~q.~n", [Cmd]).

look_at(RoomID) :-
    room_name(RoomID, RoomName),
	describe_room(RoomID, RoomDescription),
    format("\e[7m~s\e[0m~n~s~n", [RoomName, RoomDescription]),
    findall(Exit, room_exit(RoomID, Exit, _), Exits),
	phrase(room_desc_exits(Exits), ExitCs),
	format("~s~n", [ExitCs]).

room_exit_canonical(RoomID, Direction, Direction) :-
	room_exit(RoomID, Direction, _).
room_exit_canonical(RoomID, Shorthand, Direction) :-
	\+room_exit(RoomID, Shorthand, _),
	% fuzzy match the first character of a direction
	atom_chars(Shorthand, [C|_]),
	room_exit(RoomID, Direction, _),
	atom_chars(Direction, [C|_]).


room_desc(RoomID) -->
	{ room_description(RoomID, Desc) },
	{ once(phrase(highlights(interesting_word(RoomID), Output), Desc)) },
	Output.

string_words(Cs, Ws) :-
	once(phrase(words(Ws), Cs)).

words([W|Ws]) -->
	{ dif(W, []) },
	word(W),
	words(Ws).
words([]) --> [].

word(Cs) --> word_letters(Cs).
word(Cs) --> word_punctuation(Cs).

word_letters([C|Cs]) -->
	[C],
	{ char_type(C, alpha) },
	word(Cs).
word_letters([]) --> [].

word_punctuation([C|Cs]) -->
	[C],
	{ \+char_type(C, alpha) },
	word_punctuation(Cs).
word_punctuation([]) --> [].

room_desc_exits([X|Xs]) -->
	"Exits: ",
	room_desc_exit(X),
	room_desc_more(Xs).
room_desc_more([X|Xs]) -->
	", ",
	room_desc_exit(X),
	room_desc_more(Xs).
room_desc_more([]) --> ".".
room_desc_exit(Direction) -->
	{ atom_chars(Direction, Ds), highlight(4, Ds, Cs) },
	Cs.

highlight_if(Code, Condition, Word, Highlight) :-
	(  call(Condition, Word)
	-> highlight(Code, Word, Highlight)
	;  Highlight = Word
	).
highlight(ANSICode, Word, Highlight) :-
	phrase(format_("\e[~wm~s\e[0m", [ANSICode, Word]), Highlight).

interesting_word(RoomID, Word) :-
	room_detail(RoomID, Word, _).

% TODO: refactor this ↓
describe_room(RoomID, Result) :-
	room_description(RoomID, Desc),
	once(phrase(words(Words), Desc)),
	maplist(highlight_if(1, interesting_word(RoomID)), Words, Highlighted),
	once(phrase(seqq(Highlighted), Result)).