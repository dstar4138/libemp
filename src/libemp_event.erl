-module(libemp_event).

-include("internal.hrl").

-export( [exists/1, values/1] ).


%% @doc Check if the event name exists, if it does return a reference to the
%%   plugin it is registered to.
%% @end
-spec exists( libemp_event_name() ) -> false | {ok, plugin_ref()}.
exists( _Ref ) -> false.


%% @doc Given an Event reference, check what value names the event can
%%   have. Useful for displaying metadata or in subscription validation.
%% @end
-spec values( libemp_event_name() ) -> [ {atom(), non_neg_integer()} ].
values( _Ref ) -> []. %TODO: Get from DB


