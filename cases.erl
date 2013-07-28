-module(cases).
-compile(export_all).

insert(X,[]) -> 
	[X];
insert(X,Set) ->
	case lists:member(X,Set) of
		true -> Set; 
		false -> [X|Set]
	end

beach(Temperature) ->
	case Temperature of
		{celcius, N} when N >= 20, N =< 45 ->
			'favourable';
		{kelvin, N} when N >= 293, N =< 318 ->
			'scientifically favourable';
		_ -> 
			'avoid beach'
	end.