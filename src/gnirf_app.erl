-module(gnirf_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, gnirf, "index.html"}},
			{"/websocket", gnirf_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, gnirf, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	gnirf_sup:start_link().

stop(_State) ->
	ok.
