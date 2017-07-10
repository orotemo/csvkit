-module(csvkit_test).

-include_lib("eunit/include/eunit.hrl").

-define(COLUMNS, ["column2", "column3"]).
%%% testing csvkit according to file test/csv_example.csv
-define(CRCOLUMNS, ["sku", "short_description", "category_list", "was_price"
                  "sell_price", "lalt1", "description", "currency", "gender"]).

csvkit_file_test_() ->
  Filename = <<"test/csv_simple_example.csv">>,
  csvkit:parse_file(Filename, ?COLUMNS, fun send_result/3, 10),
  csvkit_all_tests().

csvkit_file_carrige_return_test_() ->
  Filename = <<"test/csv_cr_example.csv">>,
  csvkit:parse_file(Filename, ?CRCOLUMNS, fun send_result/3, 10),
  First = receive_once(),
  Second = receive_once(),
  Third = receive_once(),
  Forth = receive_once(),

  [?_assertMatch({1, {["AZZ00043"|_], _}, 10}, First),
   ?_assertMatch({2, {["WZZ60324"|_], _}, 11}, Second),
   ?_assertMatch({3, {["WZZ60324-124-34"|_], _}, 12}, Third),
   ?_assertMatch({4, done, 13}, Forth)
  ].

csvkit_string_test_() ->
  Filename = <<"test/csv_simple_example.csv">>,
  {ok, BinaryData} = file:read_file(Filename),
  CsvListData = binary_to_list(BinaryData),
  csvkit:parse_string(CsvListData, ?COLUMNS, fun send_result/3, 10),
  csvkit_all_tests().

csvkit_all_tests() ->
  First = receive_once(),
  Second = receive_once(),
  Third = receive_once(),

  [?_assertMatch({1, {["bbb1", "ccc1"], _}, 10}, First),
   ?_assertMatch({2, {["bbb2", "ccc2"], _}, 11}, Second),
   ?_assertMatch({3, done, 12}, Third)
  ].

csvkit_file_acc_test_() ->
  Filename = <<"test/csv_simple_example.csv">>,
  {ok, Acc} = csvkit:parse_file(Filename, ?COLUMNS, fun send_result/3, 1),
  [?_assertMatch(4, Acc)].

csvkit_string_acc_test_() ->
  Filename = <<"test/csv_simple_example.csv">>,
  {ok, BinaryData} = file:read_file(Filename),
  CsvListData = binary_to_list(BinaryData),
  {ok, Acc} = csvkit:parse_string(CsvListData, ?COLUMNS, fun send_result/3, 1),
  [?_assertMatch(4, Acc)].

receive_once() ->
  receive M -> M
  after 100 -> receive_error
  end.

%% Sending the returned from the parse_string
%% to be evaluated in result_test_
%% Returned value: the parameter to be returned in the next call (State)
send_result(P1, P2, State) -> self() ! {P1, P2, State}, State + 1.
