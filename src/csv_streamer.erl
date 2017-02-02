-module(csv_streamer).
%%% This module helps streaming lines of large files
%%% TODO: Streaming a string and returning the user lines of this string.

-export([process_csv_file/3,
         get_line/1,
         process_csv_string/3]).

process_csv_file(FileName, Callback, ReturnedData) ->
  case file_handler:open(FileName) of
    {ok, State} -> call_lines(State, Callback, ReturnedData);
    Else -> Else
  end.

call_lines(State, Callback, ReturnedData) ->
  case file_handler:read_line(State) of
    {ok, State1,Line} ->
      LineList = binary_to_list(Line),
      [ParsedLine] = csv:read(LineList),
      ReturnedData1 = Callback({newline, ParsedLine}, ReturnedData),
      call_lines(State1, Callback, ReturnedData1);
    {eof, _State1, <<>>} ->  Callback({eof}, ReturnedData)
  end.

%%% TODO: finish later on - handle a string and return to
%%% its callback the csv lines parsed.
process_csv_string(String, Callback, ReturnedData) ->
  case get_line(String) of
    [Line, Rest] ->
      [ParsedLine] = csv:read(Line),
      ReturnedData1 = Callback({newline, ParsedLine}, ReturnedData),
      process_csv_string(Rest, Callback, ReturnedData1);
    [Line] -> Callback({eof,Line}, ReturnedData)
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
