%%% LibEMP Utility Functionality -
%%% 
-module(libemp_util).

-define(APPLICATION,libemp).

-export([wrap_extern/2,wrap_extern/3,wrap_extern/4]).
-export([escaping_foldl/3, do/1]).
-export([function_exists/1, function_exists/2]).
-export([exit_to_error/1]).

%% @doc Performs a left-fold respecting a fail-early approach requiring that
%%   the function indicate when an error occurs via an EXIT signal or error 
%%   tuple.
%% @end
-spec escaping_foldl( Fun, Acc, [ term() ] ) -> Res 
                      when Acc :: term(),
                           Fun :: fun( (term(), Acc) -> Res ),
                           Res :: ok | {ok, term()} | {error, any()}.
escaping_foldl( _Fun,  ok, [] )   -> ok;
escaping_foldl( _Fun, Acc, [] )   -> {ok, Acc};
escaping_foldl( Fun, Acc, [H|T] ) ->
    case catch Fun(H,Acc) of
        {'EXIT',Reason} -> {error,Reason};
        {error,_,_}=Err -> Err; % Extended error case.
        {error,_}=Error -> Error;
        {ok,NAcc}       -> escaping_foldl( Fun, NAcc, T );
        NAcc            -> escaping_foldl( Fun, NAcc, T )
    end.

%% @doc Provided a list of functions and arguments, do them in order until
%%   we receive a common error code. Returns the last function's value.
%% @end
-spec do( [ {fun(), list()} ] ) -> term().
do( Funs ) ->
  escaping_foldl(
    fun( {Fun, Args}, _ ) ->
      erlang:apply( Fun, Args )
    end,
    ok, Funs ).

%% @doc Wrap calls into behaviour implementations. This is were we can
%%   optionally turn on logging, verbosity, etc.
%% @end
wrap_extern( AnonFun, Args ) -> erlang:apply( AnonFun, Args ).
wrap_extern( Module, FunName, Args ) -> erlang:apply( Module, FunName, Args ).
wrap_extern( Module, FunName, Args, Default ) ->
  case erlang:function_exported( Module, FunName, length(Args) ) of
    true  -> wrap_extern( Module, FunName, Args );
    false -> Default
  end.

%% @doc Check if a function exists in the current runtime.
function_exists( Fun ) -> function_exists( Fun, false ).

%% @doc Check if a function exists. If it does not exist in the current runtime
%%    it will attempt to load the module to check if the function is exported.
%% @end
function_exists( Fun, LoadIfNot ) ->
  case erlang:fun_info( Fun, type ) of
    {type,local}    -> true;
    {type,internal} -> true; % Should be defined, the name is a reference.
    {type,external} ->
      {M,F,A} = erlang:fun_info_mfa( Fun ),
      load_module_maybe(LoadIfNot, M),
      erlang:function_exported(M,F,A)
  end.

%% @hidden
%% @doc Maybe load the module based on a predicate result.
load_module_maybe( false, _ ) -> ok;
load_module_maybe( true, Module ) -> code:ensure_loaded( Module ).

%% @doc Convert Exit codes to {error,Reason} objects for "safe" returns.
exit_to_error( {'EXIT',_From, Reason} ) -> {error, Reason};
exit_to_error( {'EXIT',Reason} ) -> {error, Reason};
exit_to_error( Reason ) -> Reason.