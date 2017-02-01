-module(csv_streamer_tests).

-include_lib("eunit/include/eunit.hrl").
-define(FEED_EXAMPLE,"test/feed_affiliate_window_parser_test.csv").

csv_streamer_test_() ->
  Lines = csv_streamer:start(?FEED_EXAMPLE,fun csv_callback/2, []),
  [?_assertMatch(baba, Lines)].

csv_callback({newline, Line}, Acc) -> [Line | Acc];
csv_callback({eof}, Acc) -> Acc.
