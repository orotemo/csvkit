## ugly line parser for csv files with headers. callback called for each line.

pass to parse a filename (or a file content binary), column names, and a callback:
csvkit:parse("apps/nod/account_exclusion.csv", ["type", "url"], Fu1).
The details of the callback will be shortly detailed, but the essence is that
any successful line parse will return the data in the order passed in on the
`Fields` parameter:

```
fun(LineNo, {[Typ, Url], _FullLine}, State) ->
    io:format("~s:~s~n", [Typ, Url]);
  (LineNo, {error, FullLine}, State) ->
    io:format("Error on ~p: ~p~n", [LineNo, FullLine]);
  (LineNo, done, State) ->
    io:format("File is done!~n")
end.
```
