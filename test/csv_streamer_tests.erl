-module(csv_streamer_tests).

-include_lib("eunit/include/eunit.hrl").
-define(FEED_EXAMPLE,"test/feed_affiliate_window_parser_test.csv").

csv_streamer_test_() ->
  Lines = csv_streamer:process_csv_file(?FEED_EXAMPLE,
                                        fun csv_callback/2,
                                        [1,[]]),
  [?_assertMatch([8,
                  ["MALE > Sale > Shoes & Trainers",
                   "MALE > Shop by Product > Shoes, Boots & Trainers > Trainers",
                   "MALE > Sale > Jumpers & Cardigans",
                   "MALE > Sale > Shirts",
                   "FEMALE > Sale > Dresses > Evening Dresses",
                   "FEMALE > Sale > Dresses > Midi Dresses",
                   "custom_1"]], Lines)].

csv_callback({newline, Line}, [Acc,Data]) ->
  Custom1 = lists:nth(51, Line),
  [Acc+1, [Custom1|Data]];
csv_callback({eof}, Acc) -> Acc.
