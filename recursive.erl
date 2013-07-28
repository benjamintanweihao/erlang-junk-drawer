-module(recursive).
-export([fac/1, len/1, tail_fac/1, tail_len/1, duplicate/2, tail_duplicate/2,
         reverse/1, tail_reverse/1, sublist/2, tail_sublist/2, zip/2, tail_zip/2,
				 quicksort/1]).

fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).

% Tail recursive factorial
tail_fac(N) -> do_tail_fac(N, 1).

do_tail_fac(0, Acc) -> Acc;
do_tail_fac(N, Acc) -> do_tail_fac(N-1, N*Acc).

% Length of list
len([]) -> 0;
len([_|T]) -> 1+len(T).

% Tail recursive length
tail_len(List) -> do_tail_len(List, 0).

do_tail_len([], Len) -> Len;
do_tail_len([_|T], Len) -> do_tail_fac(T, 1+Len).

% Duplicate
duplicate(_,0) -> [];
duplicate(Term,N) -> [Term|duplicate(Term,N-1)].

% Tail recursive duplicate
tail_duplicate(Term, N) -> do_tail_duplicate(Term, N, []).
do_tail_duplicate(_,0,Acc) -> 
	Acc;
do_tail_duplicate(Term,N,Acc) when N > 0 -> 
	do_tail_duplicate(Term, N-1, [Term|Acc]). 

% Reverse
reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].

% Tail recursive reverse
tail_reverse(List) -> do_tail_reverse(List,[]).

do_tail_reverse([], Acc) -> Acc;
do_tail_reverse([H|T], Acc) -> do_tail_reverse(T, [H|Acc]).

%% sublist(List, N).
% Any empty list will always return an empty list
sublist([], _) -> [];
% When N == 0 the result is always the empty list 
sublist(_, 0) ->  [];
sublist([H|T],N) -> [H|sublist(T, N-1)].

tail_sublist(List,N) ->
	do_tail_sublist(List,N,[]).

do_tail_sublist(_, 0, Acc) -> reverse(Acc);
do_tail_sublist([], _, Acc) -> reverse(Acc);
do_tail_sublist([H|T], N, Acc) -> do_tail_sublist(T, N-1, [H|Acc]).

% Zip
zip([],_) -> [];
zip(_,[]) -> [];
zip([X|Xs],[Y|Ys]) -> [{ X,Y }|zip(Xs,Ys)].

% Tail recursive zip
tail_zip(List1, List2) -> do_tail_zip(List1, List2, []).

do_tail_zip([], _, Acc) -> reverse(Acc);
do_tail_zip(_, [], Acc) -> reverse(Acc);
do_tail_zip([X|Xs], [Y|Ys], Acc) -> do_tail_zip(Xs, Ys, [{X,Y}|Acc]).

tail_lenient_zip([],_,Acc) -> Acc;
tail_lenient_zip(_,[],Acc) -> Acc;
tail_lenient_zip([X|Xs],[Y|Ys], Acc) ->
    tail_lenient_zip(Xs,Ys,[{X,Y}|Acc]).

quicksort([]) -> [];
quicksort([Pivot|Rest) -> 
	{Smaller,Larger} = partition(Pivot,Rest,[],[]),
  quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_,[], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot,[H|T], Smaller, Larger) -> 
	if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
		 H <  Pivot -> partition(Pivot, T, [Smaller], [H|Larger])
	end.
