-module(csv_streamer).
-export([start/2]).

start(FileName, Callback) ->
  case file_handler:open(FileName) of
    {ok, State} -> call_lines(State, Callback);
    Else -> Else
  end.

call_lines(State, Callback) ->
  case file_handler:read_line(State) of
    {ok, State1,Line} -> Callback(Line), call_lines(State1, Callback);
    {eof, State1, <<>>} -> Else -> Callback(Else)
  end.
