-module(csv_test).
-include_lib("eunit/include/eunit.hrl").

csv_test_() ->

  Csv1 = "David Halberstam, ""\"War in a Time of Peace: Bush, Clinton, and the Generals\""", B0000C37EA, Scribner",

  [R1|_B] = csv:read(Csv1),
  Length1 =  length(R1),
  [_,_,_,D] = R1,

  [?_assertMatch(4, Length1),
   ?_assertMatch(D, "Scribner")
  ].
