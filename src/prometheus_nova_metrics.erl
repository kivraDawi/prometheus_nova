-module(prometheus_nova_metrics).

-export([setup/0]).

-export([
    http_request_duration/3,
    http_requests_total/0,
    http_partial_requests_total/0,
    http_weird_requests_total/0
]).

setup() ->
    prometheus_counter:new([
        {name, http_requests_total},
        {help, "Completed HTTP Request"}
    ]),
    prometheus_counter:new([
        {name, http_partial_requests_total},
        {help, "Partial HTTP Request"}
    ]),
    prometheus_counter:new([
        {name, http_weird_requests_total},
        {help, "Weird HTTP Request"}
    ]),
    prometheus_histogram:new([
        {name, http_request_microseconds},
        {labels, [resource, code]},
        {buckets, prometheus_http:microseconds_duration_buckets()},
        {help, "Duration of HTTP request per resource and response code"}
    ]),
    ok.

http_request_duration(Resource, ResponseCode, Duration) ->
    prometheus_histogram:observe(http_request_microseconds, [Resource, ResponseCode], Duration).

http_requests_total() ->
    prometheus_counter:inc(http_requests_total).

http_partial_requests_total() ->
    prometheus_counter:inc(http_partial_requests_total).

http_weird_requests_total() ->
    prometheus_counter:inc(http_weird_requests_total).
