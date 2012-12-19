-module(foo).
-export([mymin/1, test/3, test/2]).

mymin([H|T]) -> mymin(T, H).

mymin([], CurrentMin) -> CurrentMin;
mymin([H|T], CurrentMin) ->
   case H < CurrentMin of
      true  -> mymin(T, H);
      false -> mymin(T, CurrentMin)
   end.
   
test(A, B) ->
%  X = fun(A) -> {A} end,
%  Y = fun(F, B) -> F(B) end,
%  Y(X, A).
%  test(1, 2, 3).
  X = lists:reverse(A),
  Y = lists:foldl(fun(X,L) -> X+L end, 0, B),
  {X, Y}.
  
test(A, B, C) ->
  [A, B, C].
