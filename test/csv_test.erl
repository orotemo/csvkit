-module(csv_test).
-include_lib("eunit/include/eunit.hrl").

csv_test_() ->
  Csv1 = "David Halberstam,\"\"\"War in a Time of Peace: Bush, Clinton, and the Generals\"\"\", B0000C37EA, Scribner",
  Csv2 = "David Halberstam,\"War in a Time of Peace: Bush, Clinton, and the Generals\", B0000C37EA, Scribner",
  % CsvBin = <<"David Halberstam,\"\"\"War in a Time of Peace: Bush, Clinton, and the Generals\"\"\", B0000C37EA, Scribner">>,
  % CsvBin2 = <<"David Halberstam,\"War in a Time of Peace: Bush, Clinton, and the Generals\", B0000C37EA, Scribner">>


  [R1] = csv:read(Csv1),
  Length1 =  length(R1),
  [_,_,_,D1] = R1,
  [R2] = csv:read(Csv2),
  Length2 =  length(R2),
  [_,_,_,D2] = R2,

  [?_assertMatch(4, Length1),
   ?_assertMatch(D1, "Scribner"),
   ?_assertMatch(4, Length2),
   ?_assertMatch(D2, "Scribner")
  ].
