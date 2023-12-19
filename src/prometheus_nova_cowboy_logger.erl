-module(prometheus_nova_cowboy_logger).

-include_lib("kernel/include/logger.hrl").

-export([
    log_metrics/1,
    add_metrics_data/2
]).

-define(SENSITIVE_HEADERS, [<<"authorization">>]).

%% TODO: This should be configurable
-define(NO_LOG_PATHS, [<<"/v1/heartbeat">>, <<"/heartbeat">>, <<"/metrics">>]).

%%------------------------------------------------------------------------------
%% @doc Generates a log based on the metrics gathered by 'cowboy_metrics_h'
%%      Used in cowboy 'metrics_callback' configuration
%%
%% NOTE: Some paths will not be logged due to them not being useful, @see NO_LOG_PATHS
-spec log_metrics(cowboy_metrics_h:metrics()) -> term().
log_metrics(#{req := Req} = Metrics) ->
    ok = prometheus_nova_metrics:http_requests_total(),
    case lists:member(cowboy_req:path(Req), ?NO_LOG_PATHS) of
        false -> ?LOG_INFO(metrics_log(Metrics));
        true -> ok
    end;
log_metrics(Metrics) ->
    ?LOG_INFO(metrics_log(Metrics)).

%%------------------------------------------------------------------------------
%% @doc Adds custom data to the metrics gathered.
%%      The data is presented under 'user_data' field in the provided metrics.
%%
add_metrics_data(Data, Req) ->
    cowboy_req:cast({set_options, #{metrics_user_data => Data}}, Req).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
metrics_log(
    #{
        req := Req,
        resp_status := RespStatus,
        user_data := UserData
    } = Metrics
) ->
    Duration = request_duration(Metrics),
    prometheus_nova_metrics:http_request_duration(request_resource(UserData), RespStatus, Duration),
    maybe_add_correlation_id(UserData, #{
        message => <<"request completed">>,
        request => req_log(Req),
        http_code => RespStatus,
        duration_us => duration_microseconds(Duration)
    });
metrics_log(#{
    partial_req := Req,
    reason := Reason,
    resp_status := RespStatus,
    user_data := UserData
}) ->
    ok = prometheus_nova_metrics:http_partial_requests_total(),
    maybe_add_correlation_id(UserData, #{
        message => <<"request partially completed">>,
        http_code => RespStatus,
        remote => remote(Req),
        error => Reason
    });
metrics_log(Metrics) ->
    ok = prometheus_nova_metrics:http_weird_requests_total(),
    Metrics#{message => <<"request failed">>}.

maybe_add_correlation_id(#{correlation_id := CorrId}, Map) ->
    Map#{correlation_id => CorrId};
maybe_add_correlation_id(_, Map) ->
    Map.

request_duration(#{req_start := Start, resp_end := Stop}) ->
    Stop - Start.

duration_microseconds(Duration) ->
    erlang:convert_time_unit(Duration, native, microsecond).

request_resource(#{resource := Resource}) ->
    Resource;
request_resource(_) ->
    unknown.

req_log(Req) ->
    #{
        host => cowboy_req:host(Req),
        method => cowboy_req:method(Req),
        path => cowboy_req:path(Req),
        port => cowboy_req:port(Req),
        remote => remote(Req),
        uri => binary:list_to_bin(cowboy_req:uri(Req)),
        headers => headers(Req)
    }.

remote(Req) ->
    {Ip, Port} = cowboy_req:peer(Req),
    #{
        ip => list_to_binary(inet:ntoa(Ip)),
        port => Port
    }.

headers(Req) ->
    Headers = cowboy_req:headers(Req),
    remove_sensitive_headers(Headers).

remove_sensitive_headers(Headers) ->
    remove_sensitive_headers(Headers, ?SENSITIVE_HEADERS).

remove_sensitive_headers(Headers, []) ->
    Headers;
remove_sensitive_headers(Headers, [H | Sensitive]) ->
    remove_sensitive_headers(maps:remove(H, Headers), Sensitive).
