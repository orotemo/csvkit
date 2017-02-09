-module(csv_streamer_tests).

-include_lib("eunit/include/eunit.hrl").
-define(FEED_EXAMPLE,"test/feed_affiliate_window_parser_test.csv").

csv_streamer_test_() ->
  Lines = csv_streamer:process_csv_file(?FEED_EXAMPLE,
                                        fun csv_callback/2,
                                        [1,[]]),
  [?_assertMatch([8,
                  ["custom_1",
                   "FEMALE > Sale > Dresses > Midi Dresses",
                   "FEMALE > Sale > Dresses > Evening Dresses",
                   "MALE > Sale > Shirts",
                   "MALE > Sale > Jumpers & Cardigans",
                   "MALE > Shop by Product > Shoes, Boots & Trainers > Trainers",
                   "MALE > Sale > Shoes & Trainers"]], Lines)].

csv_callback({newline, Line}, [Acc,Data]) ->
  Custom1 = lists:nth(51, Line),
  [Acc+1, [Custom1|Data]];
csv_callback({eof}, [Acc,Data]) ->
  ReverseData = lists:reverse(Data),
  [Acc,ReverseData].
