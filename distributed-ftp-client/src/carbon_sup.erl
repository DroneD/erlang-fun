-module(carbon_sup).
-behavior(supervisor).
-export([start_link/1, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Name) ->
        ProcName = list_to_atom("carbon_" ++ atom_to_list(Name)),
	{ok, Pid} = supervisor:start_link({global, ProcName}, ?MODULE, Name),
	link(Pid),
        {ok, Pid}.

init(Name) ->
	{ok, {{one_for_one, 1, 10},
		[{carbon,
			{carbon_server, start_link, [Name]},
			permanent,
			5000,
			worker,
			[carbon_server]
		}]}}.
