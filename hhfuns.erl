-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X,Y) -> X() + Y().

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

% Let's implement map!
map(_, []) -> [];
map(Function, [H|T]) -> [Function(H)|map(Function,T)].

times_two(X) -> 2 * X.

% Called using:
%% hhfuns:map(fun hhfuns:times_two/1, [1,2,3,4]).


%% hhfuns.erl:28: Warning: variable 'A' shadowed in 'fun'
base() ->
	A = 1,
	(fun(A) -> A = 2 end)(2).

%% Filters
filter(_, [], Acc) -> list:reverse(Acc);
filter(Pred, [H|T], Acc) ->
	case Pred(H) of
		true  -> filter(pred, T, [H|Acc]);
		false -> filter(pred, T, [Acc])
	end.

% fold
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T). 

% hhfuns:fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T).

% using fold to build other HOF
% using fold you can build a list too.
reverse(L) ->
	fold(fun(X, Acc) -> [X|Acc] end, [], L).

map2(F,L) ->
	reverse(fold(fun(X,Acc) -> [F(X)|Acc] end, [], L)).

%% map(F, [H|T]) -> [F(H)|map(F,T)].

filter2(Pred, L) ->		
	F =	fun(X, Acc) -> 	
				case Pred(X) of
					true  -> [X|Acc];
					false -> [Acc]
				end
			end,
	list:reverse(fold(F, [], L)).

