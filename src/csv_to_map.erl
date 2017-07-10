-module(csv_to_map).

%% pass to parse a filename/streamed file and a callback:
%% csvkit:parse("apps/nod/account_exclusion.csv", Fu1).
%% The details of the callback will be shortly detailed, but the essence is that
%% any successful line parse will return the data in the order passed in on the
%% `Fields` parameter:
%%
%% fun(LineNo, {FullLineMapped}, State) ->
%%     io:format("~s:~s~n", [FullLineMapped]);
%%   (LineNo, {error, FullLine}, State) ->
%%     io:format("Error on ~p: ~p~n", [LineNo, FullLine]);
%%   (LineNo, done, State) ->
%%     io:format("File is done!~n")
%% end.
%%
-export([parse_string/3, parse_file/3, process_line/2]).
-export_type([call_arg/0, csv_callback/0]).

-type parse_response() :: {ok, term()} | {error, term()} | {throw, term()}.

% calls the provided callback with the line number, and either
% {LineMapped} | {error, Reason} | done
% Lines are given as a map of #{headerName => LineValue}.
-type call_arg() :: {[string()], [string()]} | {error, any()} | done.
-type user_state() :: any().
-type csv_callback() :: fun ((integer(), call_arg(), user_state()) -> user_state()).

-spec parse_string(string() | binary(),
                   csv_callback(),
                   user_state()) ->
  parse_response().
parse_string(String, Callback, State) when is_binary(String) ->
  parse_string(binary_to_list(String), Callback, State);
parse_string(String, Callback, State) when is_list(String) ->
  process_csv_string(String, Callback, State).

-spec parse_file(string(),
                 csv_callback(),
                 user_state()) ->
  parse_response().
parse_file(Filename, Callback, State) ->
  Response = csv_streamer:process_csv_file(Filename,
                                           fun process_line/2,
                                           {init, Callback, State}),

  case Response of
    {ok, {_, _, _, Acc}} -> {ok, Acc};
    X -> X
  end.

process_csv_string(Stream, Callback, State) ->
  Response = csv_streamer:process_csv_string(Stream,
                                             fun process_line/2,
                                             {init, Callback, State}),
  case Response of
    {ok, {_, _, _, Acc}} -> {ok, Acc};
    _ -> Response
  end.

process_line({newline, Line}, {init, Callback, State}) ->
  {1, Line, Callback, State};

process_line({newline, Line}, {LineNum, Headers, Callback, State}) ->
  Map = maps:from_list(lists:zip(Headers, Line)),
  State1 = Callback(LineNum, Map, State),
  {LineNum + 1, Headers, Callback, State1};

process_line({eof}, {LineNum, Headers, Callback, State}) ->
  State1 = Callback(LineNum, done, State),
  {LineNum, Headers, Callback, State1}.
