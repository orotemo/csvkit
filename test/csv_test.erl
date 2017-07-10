-module(csv_test).
-include_lib("eunit/include/eunit.hrl").

csv_test_() ->
  Csv1 = "David Halberstam,\"\"\"War in a Time of Peace: Bush, Clinton, and the Generals\"\"\", B0000C37EA, Scribner",
  Csv2 = "David Halberstam,\"War in a Time of Peace: Bush, Clinton, and the Generals\", B0000C37EA, Scribner",

  [R1] = csv:read(Csv1),
  Length1 =  length(R1),
  [D1, D2, D3, D4] = R1,
  [R2] = csv:read(Csv2),
  Length2 =  length(R2),
  [C1, C2, C3, C4] = R2,

  [?_assertMatch(4, Length1), 
   ?_assertMatch(D1, "David Halberstam"),
   ?_assertMatch(D2,
                 "\"War in a Time of Peace: Bush, Clinton, and the Generals\""),
   ?_assertMatch(D3, "B0000C37EA"),
   ?_assertMatch(D4, "Scribner"),
   ?_assertMatch(4, Length2),
   ?_assertMatch(C1, "David Halberstam"),
   ?_assertMatch(C2, "War in a Time of Peace: Bush, Clinton, and the Generals"),
   ?_assertMatch(C3, "B0000C37EA"),
   ?_assertMatch(C4, "Scribner")
  ].

csvkit_file_test_() ->
  Filename = <<"test/feed_csv.csv">>,
  {ok, BinaryData} = file:read_file(Filename),
  CsvListData = binary_to_list(BinaryData),
  {_Headers, Rest} = csv_streamer:get_line(CsvListData),
  {Line1, Rest1} = csv_streamer:get_line(Rest),
  {Line2, _Rest2} = csv_streamer:get_line(Rest1),

  [R1] = csv:read(Line1),
  Length1 = length(R1),
  [D1, D2, D3, D4] = R1,
  [R2] = csv:read(Line2),
  Length2 =  length(R2),
  [C1, C2, C3, C4] = R2,
  [?_assertMatch(4, Length1),
   ?_assertMatch(D1, "David Halberstam"),
   ?_assertMatch(D2,"War in a Time of Peace: Bush, Clinton, and the Generals"),
   ?_assertMatch(D3, "B0000C37EA"),
   ?_assertMatch(D4, "Scribner"),
   ?_assertMatch(4, Length2),
   ?_assertMatch(C1, "David Halberstam"),
   ?_assertMatch(C2,
                "\"War in a Time of Peace: Bush, Clinton, and the Generals\""),
   ?_assertMatch(C3, "B0000C37EA"),
   ?_assertMatch(C4, "Scribner")
  ].
