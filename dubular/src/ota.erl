-module(ota).
-compile([]).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,code_change/3,handle_info/2,terminate/2]).
-export([current_version/0,check_for_update/0,update_check/0]).

init([]) ->
	process_flag(trap_exit, true),
	{ok, []}.

start_link() -> gen_server:start_link({local, otad}, ota, [], []).

handle_call(version, _From, State) ->
	{reply, get_version(), State};
handle_call(check_for_update, _From, State) ->
	{reply, update_check(), State}.

handle_cast(_, _State) ->
	{noreply, _State}.

terminate(shutdown, _State) ->
	%% Code for cleaning up
	ok.

code_change(_OldVersion, State, _Extra) ->
	%% Update shit...?
    {ok, State}.

handle_info(_, State) ->
	{noreply, State}.

%% current_version/0
current_version() ->
	gen_server:call(otad, version).

%% check_for_update/0
check_for_update() ->
	gen_server:call(otad, check_for_update).

%% read_updates/1
%% Arg 1 is a list (string), which is expected to be a fully qualified
%% path to a local file on the filesystem
read_updates(PathToFile) when is_list(PathToFile) ->
	case file:read_file(PathToFile) of
		{ok, Data} ->
			SplitData = lists:map(fun(Item) -> binary:split(Item, [<<" ">>,<<"\t">>], [global]) end, binary:split(Data, [<<"\n">>], [global])),
			[PossiblyEmpty | Rest] = lists:reverse(SplitData),
			TrimmedData = case PossiblyEmpty of
				[<<>>] ->
					lists:reverse(Rest);
				_ ->
					SplitData
			end,
			lists:map(
				fun(Item) -> [Ver, Action, Url] = Item,
					{list_to_integer(binary_to_list(Ver)), list_to_atom(binary_to_list(Action)), binary_to_list(Url)}
				end, TrimmedData);
		{error, _} -> error
	end.

%% get_version/0
%% Return the currently running version of the system as an integer
get_version() ->
	MyVersion = os:getenv("DUBULAR_VER"),
	case MyVersion of
		false ->
			0;
		_ ->
			list_to_integer(MyVersion)
	end.

%% reduce_update_list/1
%% Takes list of update actions, and reduces the list to only the update steps
%% applicable to this instance.
reduce_update_list(UpdateList) when is_list(UpdateList) ->
	MyVersion = get_version(),
	[ {Version, Action, TempUrl} || {Version, Action, TempUrl}  <- UpdateList, Version > MyVersion].

%% parse_updates/1
%% Arg 1 is a list of lists (list of list of strings)
parse_updates([]) ->
	ok;
parse_updates(UpdateList) when is_list(UpdateList) ->
	MyVersion = os:getenv("DUBULAR_VER"),
	[FirstUpdate | _] = UpdateList,
	{CurVer, ActionAtom, Url} =  FirstUpdate,
	case MyVersion of
		false ->
			io:format("Doing initial update ~p~n", [CurVer]),
			perform_update(CurVer, {ActionAtom, Url});
		_ ->
			MyVerInt = list_to_integer(MyVersion),
			io:format("My Version is ~p~n", [MyVerInt]),
			TrimmedList = [ {Version, Action, TempUrl} || {Version, Action, TempUrl}  <- UpdateList, Version > MyVerInt],
			parse_updates(TrimmedList, MyVerInt)
	end.

%% parse_updates/2
%% Arg 1 is still the list
%% Arg 2 is the currently executing version
parse_updates([], ThisVersion) ->
	io:format("Up to date at version ~p~n", [ThisVersion]),
	ok;
parse_updates([FirstUpdate], ThisVersion) ->
	{CurVer, ActionAtom, Url} =  FirstUpdate,
	if
		ThisVersion < CurVer ->
			% We found an update step to do
			io:format("Doing update to version ~p~n", [CurVer]),
			% Update call would happen here
			perform_update(CurVer, {ActionAtom, Url})
	end;
parse_updates([FirstUpdate | Rest], ThisVersion) ->
	{CurVer, ActionAtom, Url} =  FirstUpdate,

	io:format("Checking against version ~p~n", [CurVer]),

	if
		ThisVersion >= CurVer ->
			parse_updates(Rest, ThisVersion);
		true -> % else
			% We found an update step to do
			io:format("Doing update to version ~p~n", [CurVer]),
			% Update call would happen here
			perform_update(CurVer, {ActionAtom, Url})
	end.

perform_update(Version, {Action, Url}) ->
	io:format("Update step is an \"~s\"~n", [Action]),
	io:format("Retrieve data from ~s~n", [Url]),
	case Action of
		update ->
			io:format("Doing update step...~n", []);
		install ->
			io:format("Doing install step...~n", [])
	end,
	os:putenv("DUBULAR_VER", integer_to_list(Version)),
	case Action of
		install ->
			io:format("Install step - rebooting...~n", []);
		_ -> false
	end,
	ok.

%% update_check/0
%% Check against the currently registered version for new update steps to do.
update_check() ->
	Updates = reduce_update_list(read_updates("/Users/rdub/Projects/erlang/dubular/tests/releases.txt")),
	parse_updates(Updates).
