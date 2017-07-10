-module(csv_to_map_tests).

-include_lib("eunit/include/eunit.hrl").

%%% testing according to file test/csv_example.csv

csv_to_map_file_test_() ->
  Filename = <<"test/csv_simple_example.csv">>,
  csv_to_map:parse_file(Filename, fun send_result/3, 10),
  csv_to_map_all_tests().

csv_to_map_string_test_() ->
  Filename = <<"test/csv_simple_example.csv">>,
  {ok, BinaryData} = file:read_file(Filename),
  CsvListData = binary_to_list(BinaryData),
  csv_to_map:parse_string(CsvListData, fun send_result/3, 10),
  csv_to_map_all_tests().

csv_to_map_all_tests() ->
  First = receive_once(),
  Second = receive_once(),
  Third = receive_once(),

  [?_assertMatch({1, #{"column1" := "aaa1",
                        "column2" := "bbb1",
                        "column3" := "ccc1"}, 10}, First),
   ?_assertMatch({2, #{"column1" := "aaa2",
                        "column2" := "bbb2",
                        "column3" := "ccc2"}, 11}, Second),
   ?_assertMatch({3, done, 12}, Third)
  ].

receive_once() ->
  receive M -> M
  after 100 -> receive_error
  end.

%% Sending the returned from the parse_string
%% to be evaluated in result_test_
%% Returned value: the parameter to be returned in the next call (State)
send_result(P1, P2, State) -> self() ! {P1, P2, State}, State + 1.
