%%% 
%%% An Optimized FIFO Message Buffer using ETS.
%%% Based on Jay Nelson's <jay@dumark.com> ets_buffer module in the epocxy 
%%% application. This module strips away all the extra optional operational 
%%% details and expands on it's functionality using several suggestions made
%%% to handle large and continuous buffers. Namely,
%%%
%%%     - Limited to dedicated ets buffer api.
%%%
%%%     - Automatic failover to new tables once integers turn to bignums on
%%%     the local machine. 
%%%
%%%     - Ets table duplication for logging/migrating events between machines.
%%%
-module( libemp_ets_fifo_buffer ).

-compile({inline, [
                   get_buffer/2, get_buffer_readw/2, get_buffer_write/2,
                   get_buffer_type_and_pos/3,
                   insert_ets_internal/4,
                   set_high_water/3
                  ]}).

%% External interface
-export([
         create_dedicated/1,
         write_dedicated/2,
         read_dedicated/1, read_dedicated/2, read_all_dedicated/1,
         read_timestamped_dedicated/1, read_timestamped_dedicated/2,
         read_all_timestamped_dedicated/1,
         history_dedicated/1, history_dedicated/2,
         history_timestamped_dedicated/1, history_timestamped_dedicated/2,
         clear_dedicated/1, delete_dedicated/1, list_dedicated/1,
         num_entries_dedicated/1, 
         clear_high_water_dedicated/1
        ]).

-type buffer_name() :: atom().
-type buffer_size() :: non_neg_integer().
-type buffer_loc()  :: pos_integer().
-type buffer_data() :: any().
-type buffer_data_timestamped() :: {erlang:timestamp(), buffer_data()}.

-type buffer_error() :: not_supported
                      | {missing_ets_buffer, buffer_name()}
                      | {missing_ets_data,   buffer_name(), buffer_loc()}.

-export_type([buffer_error/0]).


%%%------------------------------------------------------------------------------
%%% Support functions for consistent access to data structures
%%%------------------------------------------------------------------------------

%% Record stored in ets table (also used for matchspecs).
-record(ets_buffer, {
          name                     :: {meta, buffer_name()} | {meta, '_'},
          size        = 0          :: buffer_size()         | '_',
          high_water  = 0          :: buffer_size()         | '_',
          reserve_loc = 0          :: non_neg_integer()     | '_',
          write_loc   = 0          :: non_neg_integer()     | '_',
          read_loc    = 0          :: non_neg_integer()     | '_'
         }).

-define(meta_key(Buffer_Name), {meta, Buffer_Name}).
    
%% Convert record to proplist.
make_buffer_proplist(#ets_buffer{name={meta, Name}, size=Size, high_water=High_Water,
                                 reserve_loc=Reserve_Loc, write_loc=Write_Loc, read_loc=Read_Loc}) ->
    [{name, Name}, {size, Size}, {high_water, High_Water}, 
     {reserve_loc, Reserve_Loc}, {write_loc, Write_Loc},   {read_loc, Read_Loc}].

%% Match specs for buffers.
all_buffers(Table_Name) ->
    try ets:match_object(Table_Name, #ets_buffer{name=?meta_key('_'), _='_'})
    catch error:badarg -> []
    end.


%%%------------------------------------------------------------------------------
%%% New entries are written to buffers in three atomic steps to ensure that
%%% readers can concurrently access a buffer and not read the wrong information,
%%% whilst also ensuring the writers don't overwrite each other's values
%%% (n.b., other actions can interleave between the 3 atomic steps).
%%%
%%%   1) Reserve a write slot for the new buffer entry (atomic inc)
%%%   2) Write the new buffer entry to the reserved slot (insert_new key)
%%%   3) Move the write pointer to indicate entry available to read (atomic inc)
%%%      a) move the read pointer if buffer semantics require
%%%
%%% The pointers are maintained in a meta-data record of the format:
%%%
%%%   #ets_buffer{name={meta, Buffer_Name}, ...}
%%%------------------------------------------------------------------------------

%% FIFO bumps only write location to publish, read is trailing behind.
-define(fifo_reserve_write_cmd(),  {#ets_buffer.reserve_loc, 1}).
-define(fifo_publish_write_cmd(), [{#ets_buffer.write_loc,   1}, 
                                   {#ets_buffer.read_loc, 0}]).

%% FIFO read reserve atomically gets latest read location and increments it.
-define(fifo_reserve_read_cmd(Num_Items, Write_Loc),
                                  [{#ets_buffer.read_loc, 0}, 
                                   {#ets_buffer.read_loc, Num_Items, 
                                                Write_Loc-1, Write_Loc}]).

-define(fifo_reserve_read_all_cmd(Write_Loc, Read_Loc),
                                  [{#ets_buffer.read_loc, 0}, 
                                   {#ets_buffer.read_loc, 
                                                Write_Loc-Read_Loc}]).

-define(set_high_water_cmd(Count), {#ets_buffer.high_water, Count}).
    

%%%------------------------------------------------------------------------------
%%% Buffer entries use a different record structure and have the buffer name as
%%% part of the key to take advantage of the ordered set characteristics of the
%%% ets table used. The following key sequences are used:
%%%
%%%   1) Ring: {Buffer_Name,  1 ...  Bignum}  => ever increasing integer
%%%   1) FIFO: {Buffer_Name,  1 ...  Bignum}  => ever increasing integer
%%%   2) LIFO: {Buffer_Name, -1 ... -Bignum}  => ever decreasing integer
%%%
%%% Note that the integers may turn into bignums, causing a slight slowdown
%%% when indexing and accessing the keys. To avoid this, periodically delete
%%% the buffer and then re-create it to start the sequence over again. Be
%%% sure to do this only after you have read all the data and are certain
%%% that no writers are accessing the buffer.
%%%------------------------------------------------------------------------------

%% Record format of all ets_buffer data (also used for matchspecs).
-record(buffer_data, {
          key          :: {buffer_name(), buffer_loc()  | '_' | '$1'},
          created      :: erlang:timestamp()            | '_' | '$1' | '$2' | '$3',
          data         :: buffer_data()                 | '_' | '$1' | '$2'
         }).

-define(buffer_key(Name, Loc), {Name, Loc}).
-define(buffer_data(Name, Loc, Data),
    #buffer_data{key=?buffer_key(Name, Loc), created=os:timestamp(), data=Data}).
-define(buffer_data(Name, Loc, Time, Data),
    #buffer_data{key=?buffer_key(Name, Loc), created=Time,           data=Data}).
    
%%%------------------------------------------------------------------------------
%%% External API for separately named ets_buffers (one per ets table)
%%%------------------------------------------------------------------------------

-spec list_dedicated(buffer_name()) -> proplists:proplist().
-spec create_dedicated(buffer_name()) -> buffer_name().
-spec clear_dedicated(buffer_name()) -> boolean().
-spec delete_dedicated(buffer_name()) -> boolean().

-spec write_dedicated(buffer_name(), any()) -> non_neg_integer() | true | buffer_error().
-spec read_dedicated(buffer_name()) -> [buffer_data()] | buffer_error().
-spec read_dedicated(buffer_name(), pos_integer()) -> [buffer_data()] | buffer_error().
-spec read_all_dedicated(buffer_name()) -> [buffer_data()] | buffer_error().
-spec read_timestamped_dedicated(buffer_name()) -> [buffer_data_timestamped()] | buffer_error().
-spec read_timestamped_dedicated(buffer_name(), pos_integer()) -> [buffer_data_timestamped()] | buffer_error().
-spec read_all_timestamped_dedicated(buffer_name()) -> [buffer_data_timestamped()] | buffer_error().
-spec history_dedicated(buffer_name()) -> [buffer_data()].
-spec history_dedicated(buffer_name(), pos_integer()) -> [buffer_data()].
-spec history_timestamped_dedicated(buffer_name()) -> [buffer_data()].
-spec history_timestamped_dedicated(buffer_name(), pos_integer()) -> [buffer_data()].
-spec num_entries_dedicated(buffer_name()) -> non_neg_integer() | buffer_error().
-spec clear_high_water_dedicated(buffer_name()) -> true | buffer_error().


%% @doc Get a single proplist for the buffer in a named ets table.
list_dedicated(Buffer_Name) when is_atom(Buffer_Name) ->
    case all_buffers(Buffer_Name) of
        []       -> [];
        [Buffer] -> make_buffer_proplist(Buffer)
    end.

%% @doc
%%   Initialize a new empty named ETS table to hold a FIFO or LIFO buffer.
%%   This function is used when the number of accesses to a shared
%%   ets table would be too high, or when independent ets life cycle
%%   provides a quicker way to eliminate buffer memory.
%% @end
create_dedicated(Buffer_Name) when is_atom(Buffer_Name) ->
    Tid = ets:new(Buffer_Name, [named_table, ordered_set, public, {keypos, 2}, {write_concurrency, true}]),
    Buffer_Meta = #ets_buffer{name=?meta_key(Buffer_Name), size=0},
    ets:insert_new(Buffer_Name, Buffer_Meta),
    Tid.

%% @doc Remove all entries from a dedicated buffer, but keep the empty buffer.
clear_dedicated(Buffer_Name) when is_atom(Buffer_Name) ->
    clear_internal(Buffer_Name, Buffer_Name).

%% @doc Delete the entire dedicate ets table.
delete_dedicated(Buffer_Name) when is_atom(Buffer_Name) ->
    try   ets:delete(Buffer_Name)
    catch error:badarg -> {missing_ets_buffer, Buffer_Name}
    end.

%% @doc Write data to the named ets buffer table following the semantics of the buffer type.
write_dedicated(Buffer_Name, Data) when is_atom(Buffer_Name) ->
    write_internal(Buffer_Name, Buffer_Name, Data).

%% @doc Read all currently queued data items from a dedicated buffer (ring and FIFO only).
read_all_dedicated(Buffer_Name)
  when is_atom(Buffer_Name) ->
    read_all_internal(Buffer_Name, Buffer_Name, false).

%% @doc Read multiple data items from a dedicated buffer following the semantics of the buffer type.
read_dedicated(Buffer_Name, Num_Items)
  when is_atom(Buffer_Name), is_integer(Num_Items), Num_Items > 0 ->
    read_internal(Buffer_Name, Buffer_Name, Num_Items, false).
read_dedicated(Buffer_Name) when is_atom(Buffer_Name) -> read_dedicated(Buffer_Name, 1).

%% @doc Read all currently queued data items from a dedicated buffer (ring and FIFO only).
read_all_timestamped_dedicated(Buffer_Name)
  when is_atom(Buffer_Name) ->
    read_all_internal(Buffer_Name, Buffer_Name, true).

%% @doc Read one data item from a dedicated buffer following the semantics of the buffer type.
read_timestamped_dedicated(Buffer_Name) when is_atom(Buffer_Name) ->
    read_timestamped_dedicated(Buffer_Name, 1).

%% @doc Read multiple data items from a dedicated buffer following the semantics of the buffer type.
read_timestamped_dedicated(Buffer_Name, Num_Items)
  when is_atom(Buffer_Name), is_integer(Num_Items), Num_Items > 0 ->
    read_internal(Buffer_Name, Buffer_Name, Num_Items, true).

%% @doc
%%   Return all buffered data which is still present in a named ets buffer table,
%%   even if previously read. The order of the list is from oldest item to newest item.
%% @end
history_dedicated(Buffer_Name) when is_atom(Buffer_Name) ->
    history_internal(Buffer_Name, Buffer_Name, false).

%% @doc
%%   Return the last N buffered items still present in a named ets buffer table,
%%   even if previously read. The order of the list is from oldest item to newest item.
%% @end
history_dedicated(Buffer_Name, Num_Items)
  when is_atom(Buffer_Name), is_integer(Num_Items), Num_Items > 0 ->
    history_internal(Buffer_Name, Buffer_Name, Num_Items, false).

%% @doc
%%   Return all buffered data which is still present in a named ets buffer table,
%%   even if previously read. The order of the list is from oldest item to newest item.
%% @end
history_timestamped_dedicated(Buffer_Name) when is_atom(Buffer_Name) ->
    history_internal(Buffer_Name, Buffer_Name, true).

%% @doc
%%   Return the last N buffered items still present in a named ets buffer table,
%%   even if previously read. The order of the list is from oldest item to newest item.
%% @end
history_timestamped_dedicated(Buffer_Name, Num_Items)
  when is_atom(Buffer_Name), is_integer(Num_Items), Num_Items > 0 ->
    history_internal(Buffer_Name, Buffer_Name, Num_Items, true).

%% @doc Return the number of unread entries present in a buffer
num_entries_dedicated(Buffer_Name) when is_atom(Buffer_Name) ->
    num_entries_internal(Buffer_Name, Buffer_Name).

%% @doc Return the potential capacity of the buffer
clear_high_water_dedicated(Buffer_Name) when is_atom(Buffer_Name) ->
    set_high_water(Buffer_Name, ?meta_key(Buffer_Name), 0).


%%%------------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------------

-define(READ_RETRIES, 3).

-define(READ_LOC,    {#ets_buffer.read_loc,    0}).
-define(WRITE_LOC,   {#ets_buffer.write_loc,   0}).
-define(RESERVE_LOC, {#ets_buffer.reserve_loc, 0}).

-define(TYPE_AND_SIZE,  {#ets_buffer.size, 0}, {#ets_buffer.high_water, 0}).
-define(TYPE_AND_WRITE, ?TYPE_AND_SIZE,  ?WRITE_LOC).
-define(TYPE_AND_READW, ?TYPE_AND_WRITE, ?READ_LOC).

%% Use only writers to get the values so that a read lock isn't used.
get_buffer(Table_Name, Buffer_Name) -> get_buffer_type_and_pos(Table_Name, Buffer_Name, [?TYPE_AND_SIZE]).
get_buffer_write(Table_Name, Buffer_Name) -> get_buffer_type_and_pos(Table_Name, Buffer_Name, [?TYPE_AND_WRITE]).
get_buffer_readw(Table_Name, Buffer_Name) -> get_buffer_type_and_pos(Table_Name, Buffer_Name, [?TYPE_AND_READW]).

get_buffer_type_and_pos(Table_Name, Buffer_Name, Update_Cmd) ->
    try   ets:update_counter(Table_Name, ?meta_key(Buffer_Name), Update_Cmd)
    catch error:badarg -> false
    end.

clear_internal(Table_Name, Buffer_Name) ->
    try 
        ets:match_delete(Table_Name, #buffer_data{key=?buffer_key(Buffer_Name, '_'), _='_'}),
        ets:update_element(Table_Name, ?meta_key(Buffer_Name), [?RESERVE_LOC, ?WRITE_LOC, ?READ_LOC])
    catch error:badarg -> {missing_ets_buffer, Buffer_Name}
    end.

%% All writes use 1) reserve, 2) write, 3) publish semantics.
write_internal(Table_Name, Buffer_Name, Data) ->
    Meta_Key = ?meta_key(Buffer_Name),
    case get_buffer(Table_Name, Buffer_Name) of
        false -> {missing_ets_buffer, Buffer_Name};
        [_Max_Loc, High_Water_Count] ->
                 Reserved_Loc = ets:update_counter(Table_Name, Meta_Key, ?fifo_reserve_write_cmd()),
                true = insert_ets_internal(Table_Name, Buffer_Name, Reserved_Loc, Data),
                [New_Write_Loc, Read_Loc] = ets:update_counter(Table_Name, Meta_Key, ?fifo_publish_write_cmd()),
                Num_Entries = compute_num_entries(New_Write_Loc, Read_Loc),
                Num_Entries > High_Water_Count andalso set_high_water(Table_Name, Meta_Key, Num_Entries),
                Num_Entries
    end.

insert_ets_internal(Table_Name, Buffer_Name, Write_Loc, Data) ->
    ets:insert(Table_Name, ?buffer_data(Buffer_Name, Write_Loc, Data)).

set_high_water(Table_Name, Meta_Key, New_High_Water) ->
    ets:update_element(Table_Name, Meta_Key, ?set_high_water_cmd(New_High_Water)).

%% Use read pointer to reserve entries, obtain and then delete them.
read_all_internal(Table_Name, Buffer_Name, With_Timestamps) ->
    case get_buffer_readw(Table_Name, Buffer_Name) of
        false                                            -> {missing_ets_buffer, Buffer_Name};
        [_Max_Loc, _High_Water,  Write_Loc,  Write_Loc]    -> [];
        [_Max_Loc, _High_Water,  Write_Loc,  Old_Read_Loc] ->
            read_internal_finish( ?fifo_reserve_read_all_cmd(Write_Loc, Old_Read_Loc), 
                                 Table_Name, Buffer_Name, With_Timestamps)
    end.

read_internal_finish(New_Read_Loc_Cmd, Table_Name, Buffer_Name, With_Timestamps) ->
            [Start_Read_Loc, End_Read_Loc] = ets:update_counter(Table_Name, ?meta_key(Buffer_Name), New_Read_Loc_Cmd),
            read_ets(Table_Name, Buffer_Name,  Start_Read_Loc, End_Read_Loc, ?READ_RETRIES, With_Timestamps).

%% Use read pointer to reserve entries, obtain and then delete them.
read_internal(Table_Name, Buffer_Name, Num_Items, With_Timestamps) ->
    case get_buffer_readw(Table_Name, Buffer_Name) of
        false -> {missing_ets_buffer, Buffer_Name};
        [_Max_Loc, _High_Water, Write_Loc, _Old_Read_Loc] ->
            read_internal_finish(
                ?fifo_reserve_read_cmd(Num_Items, Write_Loc),
                Table_Name, Buffer_Name, With_Timestamps)
    end.

%%%------------------------------------------------------------------------------
%%% Read logic has several possibilities:
%%%   If Start/End are the same, return an empty set of data...
%%%   If Retry attempts yield nothing, return an empty set without deleting any data...
%%%   If an attempt yields any data, delete (except for ring) and return the results
%%%
%%% Retries are necessary when read slot(s) is reserved, but not yet written,
%%% because other processes jumped in with reservations and even writes before
%%% the first reservation had a chance to publish its write. Presumably the
%%% data will show up before we give up, but it is possible that the return
%%% value is {missing_ets_data, Buffer_Name, Read_Loc}.
%%%------------------------------------------------------------------------------

read_ets(_Table_Name, _Buffer_Name,  Read_Loc, Read_Loc, _Retries, _With_Timestamps) -> [];
read_ets( Table_Name,  Buffer_Name,  Read_Loc,  End_Loc,  Retries,  With_Timestamps) ->
    {Buffer_Entry_Match, Buffer_Deletes} = 
            disjoint_match_specs(Buffer_Name, Read_Loc, End_Loc, 'andalso', With_Timestamps),
    read_ets_retry(Table_Name, Buffer_Name,  Read_Loc, End_Loc, Retries, Buffer_Entry_Match, Buffer_Deletes).

read_ets_retry(_Table_Name, Buffer_Name, Read_Loc, _End_Loc, 0, _, _) -> {missing_ets_data, Buffer_Name, Read_Loc};
read_ets_retry( Table_Name, Buffer_Name, Read_Loc,  End_Loc, Retries, Buffer_Entry_Match, Buffer_Deletes) ->
    case ets:select(Table_Name, Buffer_Entry_Match) of
        []   -> erlang:yield(),
                read_ets_retry(Table_Name, Buffer_Name, Read_Loc, End_Loc, Retries-1, Buffer_Entry_Match, Buffer_Deletes);
        Data -> ets:select_delete(Table_Name, Buffer_Deletes),
                Data
    end.

%% Select all buffer_data Read_Loc =< Key =< End_Loc.
disjoint_match_specs(Buffer_Name, Read_Loc, End_Loc, Operator, With_Timestamps) ->
    Key = ?buffer_key(Buffer_Name, '$1'),
    Guard = [{Operator, {'<', Read_Loc, '$1'}, {'>=', End_Loc, '$1'}}],
    case With_Timestamps of
        true  -> Match = #buffer_data{key=Key, data='$2', created='$3'},
                 {[{Match, Guard, [{{'$3', '$2'}}]}],  %% Get the timestamp + data item
                  [{Match, Guard, [true]}]};           %% Delete the data item
        false -> Match = #buffer_data{key=Key, data='$2', created='_' },
                 {[{Match, Guard, ['$2']}],            %% Get only the data item
                  [{Match, Guard, [true]}]}            %% Delete the data item
    end.
    
%% Currently this function assumes the number of items is not excessive and fetches all in one try.
history_internal(Table_Name, Buffer_Name, With_Timestamps) ->
    case get_buffer_write(Table_Name, Buffer_Name) of
        false -> {missing_ets_buffer, Buffer_Name};
        _ ->
            case With_Timestamps of
                false -> [Elem || [Elem] <- ets:match(Table_Name, ?buffer_data(Buffer_Name, '_', '_', '$1'))];
                true -> [{Time, Elem} || [Time, Elem] <- ets:match(Table_Name, ?buffer_data(Buffer_Name, '_', '$1', '$2'))]
            end
    end.

history_internal(Table_Name, Buffer_Name, Num_Items, With_Timestamps) ->
    case get_buffer_write(Table_Name, Buffer_Name) of
        false -> {missing_ets_buffer, Buffer_Name};
        [Max_Loc, _High_Water, _Write_Pos] ->
            True_Num_Items = case Max_Loc of 0 -> Num_Items; _ -> min(Num_Items, Max_Loc) end,
            history_internal_limited(Table_Name, Buffer_Name, True_Num_Items, With_Timestamps)
    end.

history_internal_limited(Table_Name, Buffer_Name, Num_Items, _With_Timestamps=false) ->
    Pattern = {?buffer_data(Buffer_Name, '_',  '_', '$1'),[],['$1']},
    select_history_matches(Table_Name, Pattern, Num_Items);
history_internal_limited(Table_Name, Buffer_Name, Num_Items, _With_Timestamps=true) ->
    Pattern = {?buffer_data(Buffer_Name, '_', '$2', '$1'),[],[{{'$2', '$1'}}]},
    select_history_matches(Table_Name, Pattern, Num_Items).

select_history_matches(Table_Name, Pattern, Num_Items) ->
    case ets:select(Table_Name, [Pattern], Num_Items) of
        '$end_of_table'          -> [];
        {Matches, _Continuation} -> Matches
    end.

%% Compute the number of entries using read and write pointers
%% This is not supported for LIFO buffers because they may have
%% holes in the buffer and an accurate size cannot simply be
%% computed. Scanning the table also fails because it is non-atomic
%% and the result might not reflect the number of LIFO entries.
num_entries_internal(Table_Name, Buffer_Name) ->
    case get_buffer_readw(Table_Name, Buffer_Name) of
        false -> {missing_ets_buffer, Buffer_Name};
        [_Max_Loc, _High_Water, Write_Loc, Raw_Read_Loc] ->
            compute_num_entries(Write_Loc, Raw_Read_Loc)
    end.

compute_num_entries( 0, _ ) -> 0;     %% Nothing ever written
compute_num_entries( Write_Loc, 0 ) -> 
    Write_Loc;                        %% Written, but nothing read
compute_num_entries( Write_Loc, Read_Loc ) -> 
    Write_Loc - Read_Loc.             %% Both written and read before
