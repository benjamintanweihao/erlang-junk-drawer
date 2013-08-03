-module(multiproc).
-compile(export_all).

% matches nothing, so always goes into the after clause.
sleep(T) ->
	receive 
	after T -> ok
	end.


% when timeout is 0, then the after clause is executed immediately.
flush() ->
	receive
		_ -> flush()
	after 0 ->
		ok
	end.

% selective receives

important() ->
	receive 
		{Priority, Message} when Priority > 10 ->
			[Message|important()]
	after 0 ->
		normal()
	end.

normal() ->
	receive
		{_, Message} ->
			[Message|normal()]
		after 0 ->
			[]
		end.

% How does this work?

% First, we build a list of messages.
% self() ! {15,high}, self() ! {1,low}, self() ! {10, low}, self() !{29, high}.

% Note that these messages are all in self()'s mailbox.

% When we run multiproc:important(), the recceive block is hit, and
% we can match all the messages that was in the mailbox.

% Warning! Selective receives can cause performance problems.
