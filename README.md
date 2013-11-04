# Erlang Decorators

An early implementation of python style decorators for Erlang.

# Quick start

Add the following to your rebar.config:
```erlang
{deps,
 [
  {decorators, "", {git, "git://github.com/chrisavl/erlang_decorators.git", {branch, "master"}}}
 ]
}.
```

Add `-compile([{parse_transform, decorators}]).` to any source file using
decorators, or add `{erl_opts, [{parse_transform, decorators}]}.` to your
rebar.config.


# Usage

There are 4 ways to decorate a function:
```erlang
-decorate(my_decorator). %% local function, no decorator data
-decorate({my_decorator, [{option, value}]}). %% local function, with data
-decorate({my_module, my_decorator}). %% external function, no decorator data
-decorate({my_module, my_decorator, [{option, value}]}). %% external function, with data
```

If no data/options is specified it defaults to the empty list.


Example:

```erlang
-module(mymod).
-compile([{parse_transform, decorators}]).

-export([demo/0]).

my_decorator(OriginalFun, OriginalArgs, _UnusedData = []) ->
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
