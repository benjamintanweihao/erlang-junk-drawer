-module(calc).
-compile([rpn/1]).

rpn(L) when is_list(L) ->
	[Res] = lists:fold(fun rpn/2, [], string:tokens(L, " ")),
	Res.
