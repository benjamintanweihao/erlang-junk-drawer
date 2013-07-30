-module(tree).
-export([empty/0, insert/3, lookup/2, has_value/2, has_value1/2]).

% A non-empty node: 
% {node, {Key, Value, Smaller, Larger}}

% An empty node:
% {node, nil}  

% We are using a function to encapsulate the representation of
% an empty node.
empty() -> {node, 'nil'}.

% This function returns a new tree each time.
insert(Key, Val, {node, 'nil'}) ->
	{node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
	{node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
	{node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
	{node, {Key, Val, Smaller, Larger}}.

% Lookup
lookup(_, {node, 'nil'}) -> undefined;
lookup(Key, {node, {Key, Val, _, _}}) -> {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey -> 
	lookup(Key, Smaller); 
lookup(Key, {node, {NodeKey, _, _, Larger}}) when Key > NodeKey -> 
	lookup(Key, Larger).


%% looks for a given value 'Val' in the tree
has_value(_, {node, 'nil'}) -> 
	false;
has_value(Val, {node, {_, Val, _, _}}) ->
	true;
has_value(Val, {node, {_, _, Left, Right}}) ->
	case has_value(Val, Left) of
		true -> true;
		false -> has_value(Val, Right)
	end.

%% this version uses 'throw' as control flow. instead of testing each an 
%% every branch, once a match has been found, 'true' is thrown, where this
%% will bubble up an eventually be caught. Once this is done, we have found
%% our value.
has_value1(_, {node, 'nil'}) ->
	false;
has_value1(Val, {node, {_, Val, _, _}}) ->
	throw(true);
has_value1(Val, {node, {_, _, Left, Right}}) ->
	has_value1(Val, Left), 
	has_value1(Val, Right). 
