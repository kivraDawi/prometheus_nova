-module(prometheus_nova_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [       #{
      prefix => "",
      security => false,
      routes => [
          {"/metrics", {prometheus_nova_controller, metrics}, #{methods => [<<"GET">>]}},
          {"/metrics/:registry", {prometheus_nova_controller, metrics_registry}, #{
              methods => [<<"GET">>]
          }}
      ]
  }].
