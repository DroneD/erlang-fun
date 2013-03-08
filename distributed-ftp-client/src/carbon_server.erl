-module(carbon_server).
-behavior(gen_server).
-export([start_link/1, stop/1, reset/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	code_change/3, terminate/2]).
-export([find_files/2]).
-record(state, {port, site_name}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Name) ->
	gen_server:start_link({global, Name}, ?MODULE, [Name], []).

stop(Name) ->
	gen_server:call({global, Name}, stop).

reset(Name) ->
        gen_server:call({global, Name}, ident) ! {reset, Name},
	ok.

find_files(Name, Path) ->
        gen_server:call({global, Name}, {find_files, Path}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% flatten taken from:
%% http://stackoverflow.com/a/9346017 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flatten(X) -> flatten(X,[]).
flatten([], Acc) -> Acc;
flatten([[_|_]=H|T], Acc) -> flatten(T, flatten(H, Acc));
flatten([H|T], Acc) -> flatten(T,Acc++[H]).

list_dir({tokenized, Path}, State) ->
	case ftp:cd(State#state.port, Path) of
		ok -> list_dir(Path, State);
		_ -> string:concat(Path, "\r\n")
	end;
list_dir({error, Reason}, _State) -> 
        io:format("Error: ~p~n", [Reason]),
	[];
list_dir({ok, NList}, State) ->
	lists:map(
		fun(E) -> list_dir({tokenized,E}, State) end,
		string:tokens(NList, "\r\n")
	);
list_dir(Path, State) ->
%%        io:format("Scanning directory ~p~n", [Path]),
	list_dir(ftp:nlist(State#state.port, Path), State).

find_all_files(Path, State) ->
	string:tokens(flatten(list_dir(Path, State)), "\r\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Name]) ->
	{ok, Name, 0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(ident, _From, State) ->
	{reply, self(), State};
handle_call({find_files, Path}, _From, State) ->
	{reply, {ok, find_all_files(Path, State)}, State};
handle_call(_Call, _From, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(change_dir, State) ->
	{ok, LDir} = ftp:pwd(State#state.port),
	io:format("Current directory: ~p~n", [LDir]),
        NList = find_all_files("/", State),
        io:format("File Result: ~p~n", [NList]),
	{noreply, State, 600};
handle_cast(_Cast, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info(timeout, State) ->
        case is_atom(State) of
	true ->
	        {ok, {Site,User,Pass}} = application:get_env(State, cred),
		case inets:start(ftpc, [{host, Site}]) of
		{ok, Port} ->
	        	io:format("Starting connection to ~p~n", [State]),
		        NewState = #state{port = Port, site_name = State},
			case ftp:user(Port, User, Pass) of
		        ok ->
			        io:format("Logged on to ~p~n", [State]),
		        	gen_server:cast({global, State}, change_dir),
				{noreply, NewState};
			{error, Reason} ->
		        	io:format("Auth Error: ~p~n", [Reason]),
%%FIXME Terminate here
				{noreply, State, 500}
			end;
		{error, Reason} ->
		        io:format("Connection Error: ~p~n", [Reason]),
			{noreply, State, 500}
		end;
	false ->
		case ftp:pwd(State#state.port) of
		{ok, _} ->
			{noreply, State, 500};
		{error, Reason} ->
		        io:format("Healthcheck Error: ~p~n", [Reason]),
%%FIXME Terminate here
			{noreply, State#state.site_name, 500}
		end
	end;
handle_info({reset, Name}, State) ->
        ftp:close(State#state.port),
	self() ! timeout,
	{noreply, Name};
handle_info(_Info, State) ->
        io:format("Unhandled message: ~p ~p~n", [_Info, State]),
	{noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
terminate(_Reason, _State) ->
	ok.
