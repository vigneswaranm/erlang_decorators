# Erlang Decorators

An early implementation of python style decorators for Erlang.

# Usage

```erlang
-module(mymod).
-compile([{parse_transform, decorators}]).

-export([demo/0]).

my_decorator(OriginalFun, OriginalArgs) ->
    Start = erlang:now(),
    Result = apply(OriginalFun, OriginalArgs),
    io:format("call to ~p (args: ~p) took: ~f ms~n",
              [OriginalFun, OriginalArgs,
               timer:now_diff(now(), Start) / 1000]),
    Result.

%%-decorate({mymod, my_decorator}). % If decorator is an external call
-decorate(my_decorator). %% Since my_decorator is local, use shorthand notation
my_long_running_function(A, B) ->
    timer:sleep(100),
    A + B.

demo() ->
    3 = my_long_running_function(1, 2).
```

# TODO
  * Allow custom named decorators for nicer syntax
  * Allow passing arguments to decorators
