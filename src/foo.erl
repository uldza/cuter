-module(foo).
-export([bar/2,
         baz/1]).

-spec bar([number()], [number()]) -> number().
bar([], Ys) -> lists:sum(Ys);
bar([X|Xs], [Y|Ys]) -> X * Y + bar(Xs, Ys).

-spec baz(In::string()) -> ok.
baz(Str) when is_list(Str) ->
  case Str of
    "Omg" -> ok;
    "" -> nok
  end.

