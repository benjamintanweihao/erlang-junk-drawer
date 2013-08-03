-module(kitchen).
-compile(export_all).

fridge(FoodList) ->
	receive
		{From, {store, Food}} ->
			From ! {self(), ok},
			fridge([Food|FoodList]);
		{From, {take, Food}} ->
			case lists:member(Food, FoodList) of
				true -> 
					From ! {self(), {ok, Food}},
						fridge(lists:delete(Food, FoodList));
				false ->
					From ! {self(), not_found},
					fridge(FoodList)
			end;
		terminate ->
			ok
	end.
						
%% extract the ugliness. so instead of:
%%
%% Pid ! {self(), {take, bacon}}.
%% Pid ! {self(), {store, bacon}}.
%% 
%% we can do:
%%
%% kitchen:store(Pid, bacon)
%% kitchen:take(Pid, bacon)

% Pid = "Who do I sent the reply to?"
store(Pid, Food) ->
	Pid ! {self(), {store, Food}},
	receive 
		{Pid, Msg} -> Msg
	after 3000 ->
		timeout
	end.

take(Pid, Food) -> 
	% once a message is sent ....
	Pid ! {self(), {take, Food}},
	% it switches to receive mode ...
	receive 
		{Pid, Msg} -> Msg
	% if it doesn't get what it is expecting ...
	after 3000 ->
		% timeout!
		byebye
	end.

%% unfortunately, starting the process is ugly:
%%
%% Pid = spawn(kitchen, fridge, [[baking_soda]]).
%% 
%% let's change that.

start(FoodList) ->	
	spawn(?MODULE, fridge, [FoodList]).

%% So now, we can call the module as such:
%% Pid = kitchen:start([bacon]).
