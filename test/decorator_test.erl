-module(decorator_test).
-include_lib("eunit/include/eunit.hrl").
-export([replace_return_value_decorator/2]).

-compile([{parse_transform, decorators}]).


% example decorator that replaces the return value with the atom 'replaced'
% note that we always pass the arguments as a single list to the next fun
replace_return_value_decorator(F, Args)->
    _R = apply(F, Args),
    replaced.

-decorate({?MODULE, replace_return_value_decorator}).
replace_ret_val_decorated() -> ok.


replace_args_decorator(F, _Args)->
    apply(F, [replaced1, replaced2]).

-decorate(replace_args_decorator).
replace_args_decorated(replaced1, replaced2) -> ok.

-decorate(replace_return_value_decorator).
-decorate(replace_args_decorator).
multiple_decorators(replaced1, replaced2) ->
    ok.

replace_ret_value_test()->
    ?assertEqual(replaced, replace_ret_val_decorated()),
    ?assertEqual(ok, replace_args_decorated(arg1, arg2)),
    ?assertEqual(replaced, multiple_decorators(arg1, arg2)).

