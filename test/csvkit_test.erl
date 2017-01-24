-module(csvkit_test).

-include_lib("eunit/include/eunit.hrl").

-define(COLUMNS, ["line", "limit"]).
% testing csvkit according to file csv_example.csv located under the project

csvkit_file_test_() ->
  Filename = <<"test/csv_example.csv">>,
  csvkit:parse_file(Filename, ?COLUMNS, fun send_result/3, []),
  csvkit_all_tests().

csvkit_string_test_() ->
  Filename = <<"test/csv_example.csv">>,
  {ok, BinaryData} = file:read_file(Filename),
  CsvListData = binary_to_list(BinaryData),
  csvkit:parse_string(CsvListData, ?COLUMNS, fun send_result/3, []),
  csvkit_all_tests().

csvkit_all_tests() ->
  First = receive_once(),
  Second = receive_once(),
  Third = receive_once(),
  [?_assertMatch({1, {["Residential", "498960"], _}, _}, First),
   ?_assertMatch({2, {["Commercial","1322376.3"], _}, _}, Second),
   ?_assertMatch({3, done, _}, Third)
  ].

%%
receive_once() ->
  receive M -> M
  after 100 -> receive_error
  end.


%% Sending the returned from the parse_string
%%  to be evaluated in result_test_
send_result(P1, P2, P3) -> self() ! {P1, P2, P3}.
