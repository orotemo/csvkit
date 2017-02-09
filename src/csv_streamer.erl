-module(csv_streamer).
%%% This module helps streaming lines of csv files
%%% process_csv_file/3 - Receives files, can be large, streams
%%% its lines and returns to the callback, line by line a list of
%%% the line's columns
%%% process_csv_string/3 - Receives csv data, and streams it
%%% like process_csv_file/3
%%% get_line/1 - receives data, returns the line, and rest if data received

-export([process_csv_file/3,
         get_line/1,
         process_csv_string/3]).

process_csv_file(FileName, Callback, UserState) ->
  case file_handler:open(FileName) of
    {ok, State} -> call_lines(State, Callback, UserState);
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
