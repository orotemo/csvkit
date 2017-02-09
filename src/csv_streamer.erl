-module(csv_streamer).
%%% This module helps streaming lines of csv files
%%% process_csv_file/3 - Receives a binary file name,(File can be large)
%%% Callback, and user's state.
%%% Process each line in the csv to a list using the csv module
%%% and for each line calls the callback with the processed line, and the user
%%% state. The returned data from the callback will be the next user state.
%%% When file ended, calls the callback with {eof}, UserState
%%%
%%% process_csv_string/3 - Receives csv data as a string or as binary,
%%% and like process_csv_file, calls the callback with the processed line.
%%%
%%% get_line/1 - receives data, returns the line, and rest if data received

-export([process_csv_file/3,
         get_line/1,
         process_csv_string/3]).

-type user_state() :: any().
-type csv_tuple() :: {newline, list()} | {eof}.
-type csv_callback() :: fun ((csv_tuple(), user_state()) -> user_state()).
-type process_csv_response() :: {ok, user_state()} | {error, any()}.

-spec process_csv_file(binary(),
                  csv_callback(),
                  user_state()) ->
 process_csv_response().

process_csv_file(FileName, Callback, UserState) ->
  case file_handler:open(FileName) of
    {ok, State} ->
    call_lines(State, Callback, UserState);
    Else -> Else
  end.

call_lines(State, Callback, UserState) ->
  case file_handler:read_line(State) of
    {ok, State1,Line} ->
      LineList = binary_to_list(Line),
      [ParsedLine] = csv:read(LineList),
      UserState1 = Callback({newline, ParsedLine}, UserState),
      call_lines(State1, Callback, UserState1);
    {eof, _State1, <<>>} ->  Callback({eof}, UserState)
  end.

%%% Handle a string and return to
%%% its callback the csv lines parsed.
-spec process_csv_string(string(),
                  csv_callback(),
                  user_state()) ->
 process_csv_response().

process_csv_string(String, Callback, UserState) when is_binary(String) ->
  process_csv_string(binary_to_list(String), Callback, UserState);
process_csv_string(String, Callback, UserState) ->
  case get_line(String) of
    {Line, Rest} ->
      [ParsedLine] = csv:read(Line),
      UserState1 = Callback({newline, ParsedLine}, UserState),
      process_csv_string(Rest, Callback, UserState1);
    {[]} -> Callback({eof}, UserState);
    {Line} ->
      [ParsedLine] = csv:read(Line),
      UserState1 = Callback({newline, ParsedLine}, UserState),
      Callback({eof}, UserState1)
  end.

%%% Finds the end of a line.
%%% like binary:split,it returnes {Line,Rest}
%%% If there is no end of line - returns the data it received
get_line(Data) ->
  get_line(Data,[]).

get_line([],ReversedStart) ->
  {lists:reverse(ReversedStart)};
get_line([10 | Rest], ReversedStart) ->
  Line = lists:reverse(ReversedStart),
  {Line, Rest};
get_line([Char|Rest],ReversedStart) ->
  get_line(Rest,[Char|ReversedStart]).
