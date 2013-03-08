-module(carbon).
-behavior(application).
-export([start/2, stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(normal, Name) ->
	carbon_sup:start_link(Name);
start({takeover, _OtherNode}, Name) ->
	carbon_sup:start_link(Name).

stop(_State) ->
	ok.
