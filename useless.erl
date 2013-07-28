-module(useless).
-export([add/2, hello/0, greet_and_add/2]).

add(X,Y) ->
	X + Y.

hello() -> io:format("Hello, World!~n").
%% This is a comment

greet_and_add(X, Y) ->
	hello(),
	add(X,Y).