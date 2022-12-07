:- module(player, []).
:- use_module(library(format)).

command(go(Direction)) --> "go", space, direction(Cs), { atom_chars(Direction, Cs) }.
command(look) --> "look" | "l".
command(look(Object)) --> ("look at" | "examine" | "x"), space, object(Object).
command(take(Object)) --> "take", space, object(Object).
command(use(Object)) --> "use", space, object(Object).
command(help) --> "help".
command(quit) --> "quit" | "q".
command(fallback(Cmd)) --> string(Cmd).
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

% movement
execute(go(Direction)) :-
	move(player, Direction),
	format("You head ~a.~n", [Direction]),
	execute(look).
execute(go(Direction)) :-
	format("You can't go ~a.~n", [Direction]).
execute(fallback(Cs)) :-
	% shorthand for directions: "west" instead of "go west"
	atom_chars(Direction, Cs),
	location(player, RoomID),
	room_exit_match(RoomID, Direction, Canon),
	execute(go(Canon)).
execute(fallback(Cmd)) :-
	atom_chars(Exit, Cmd),
	room_exit_match(_, Exit, Direction),
	% prefer error message for direction over "can't understand" if possible
	execute(go(Direction)).

% describe
execute(look) :-
	location(player, RoomID),
	phrase(look(RoomID), Cs),
	format("~s", [Cs]).
	% look_at(RoomID).
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
execute(fallback(Cmd)) :- format("Sorry, I don't understand ~q.~n", [Cmd]).
execute(Cmd) :- format("Unhandled command: ~q.~n", [Cmd]).

look(RoomID) -->
	{ room_name(RoomID, Title) },
	"\e[7m", % inverse colors
	Title,
	"\e[0m\n",
	look_describe(RoomID),
	"\n",
	look_exits(RoomID),
	"\n".

look_describe(RoomID) -->
	{ room_description(RoomID, Desc), string_words(Desc, Words) },
	look_describe_word(RoomID, Words).
look_describe_word(RoomID, [Word|Ws]) -->
	{ interesting_word(RoomID, Word) },
	format_("\e[~wm~s\e[0m", [1, Word]),
	look_describe_word(RoomID, Ws).
look_describe_word(RoomID, [Word|Ws]) -->
	{ \+interesting_word(RoomID, Word) },
	Word,
	look_describe_word(RoomID, Ws).
look_describe_word(_, []) --> [].

look_exits(RoomID) -->
	{ findall(Exit, room_exit(RoomID, Exit, _), Exits) },
	room_desc_exits(Exits).

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

highlight(ANSICode, Word, Highlight) :-
	phrase(format_("\e[~wm~s\e[0m", [ANSICode, Word]), Highlight).

interesting_word(RoomID, Word) :-
	room_detail(RoomID, Word, _).