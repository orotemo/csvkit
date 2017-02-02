%%
%%
%%        .-.     .
%%       (   )   _|_
%%        `-. .  .|  .-.
%%       (   )|  || (.-'
%%        `-' `--|`-'`--'
%%               ;
%%            `-'
%%
-module(csvkit).
%% pass to parse a filename, column names, and a callback:
%% csvkit:parse("apps/nod/account_exclusion.csv", ["type", "url"], Fu1).
%% The details of the callback will be shortly detailed, but the essence is that
%% any successful line parse will return the data in the order passed in on the
%% `Fields` parameter:
%%
%% fun(LineNo, {[Typ, Url], _FullLine}, State) ->
%%     io:format("~s:~s~n", [Typ, Url]);
%%   (LineNo, {error, FullLine}, State) ->
%%     io:format("Error on ~p: ~p~n", [LineNo, FullLine]);
%%   (LineNo, done, State) ->
%%     io:format("File is done!~n")
%% end.
%%
-export([parse_string/4, parse_file/4, process_line/2]).
-export_type([call_arg/0, csv_callback/0]).

-type parse_response() :: {ok, term()} | {error, term()} | {throw, term()}.
% calls the provided callback with the line number, and either
% {RequestedLine, OriginalLine} | {error, Reason} | done
% Lines are given as a list.
-type call_arg() :: {[string()], [string()]} | {error, any()} | done.
-type user_state() :: any().
-type csv_callback() :: fun ((integer(), call_arg(), any()) -> user_state()).

-spec parse_string(string() | binary(),
                   [string()],
                   csv_callback(),
                   user_state()) ->
  parse_response().
parse_string(String, Fields, Callback, State) when is_binary(String) ->
  parse_string(binary_to_list(String), Fields, Callback, State);
parse_string(String, Fields, Callback, State) when is_list(String) ->
  parse_with_fun(process_csv_string_with, String, Fields, Callback, State).

-spec parse_file(string(),
                 [string()],
                 csv_callback(),
                 user_state()) ->
  parse_response().
parse_file(Filename, Fields, Callback, State) ->
  Response = csv_streamer:process_csv_file(Filename,
                                           fun process_line/2,
                                           {init, Fields, Callback, State}),

  case Response of
    {ok, {_,_,_,Acc}} -> {ok, Acc};
    _ -> Response
  end.
% case file:open(Filename, [read]) of
%   {ok, File} ->
%     try
%       parse_with_fun(process_csv_file_with, File, Fields, Callback, State)
%     catch
%       Error:Reason -> {Error, Reason}
%     after
%       file:close(File)
%     end;
%
%   Else -> Else
% end.

parse_with_fun(Fun, Stream, Fields, Callback, State) ->
  Response = ecsv:Fun(Stream,
                      fun process_line/2,
                      {init, Fields, Callback, State}),
  case Response of
    {ok, {_,_,_,Acc}} -> {ok, Acc};
    _ -> Response
  end.

filtering_list(DestinationIdx) ->
  fun(Name) ->
      Stripped = string:strip(Name),
      {Stripped, maps:get(Stripped, DestinationIdx, -1)}
  end.

process_line({newline, Line}, {init, Headers, Callback, State})
  when is_list(Headers) ->
  DestinationIdx = maps:from_list(
                     lists:zip(Headers, lists:seq(0, length(Headers) - 1))),

  {1, lists:map(filtering_list(DestinationIdx), Line), Callback, State};

process_line({newline, Line}, {LineNum, Filters, Callback, State}) ->
  LineFromHeader = line_by_header(Line, Filters, []),
  State1 = Callback(LineNum, {LineFromHeader, Line}, State),
  {LineNum + 1, Filters, Callback, State1};

process_line({eof}, {LineNum, Filters, Callback, State}) ->
  State1 = Callback(LineNum, done, State),
  {LineNum, Filters, Callback, State1}.

line_by_header([], [_Header | _Headers], _Acc) -> error;
line_by_header(_Line, [], Acc) ->
  lists:map(fun({_,V}) -> V end, lists:sort(Acc));

line_by_header([_E | RestLine],
               [{_Name, -1} | OtherHeaders],
               Acc) -> line_by_header(RestLine, OtherHeaders, Acc);

line_by_header([E | RestLine], [{_Name, Idx} | OtherHeaders], Acc) ->
  line_by_header(RestLine, OtherHeaders, [{Idx, E} | Acc]).
