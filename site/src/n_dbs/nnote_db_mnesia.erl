-module(nnote_db_mnesia).
-record(nnote, {
    id = n_utils:create_id(),
    user_id,
    type,
    date,
    event,
    source,
    topic,
    question,
    tags,
    note
}).

-export([ init_table/0,
          put_record/1,
          get_all_values/1,
          get_all/0,
          get_record/1,
          delete/1,
          populate_record/1,
          get_records_by_type/2,
          get_records_by_date/3,
          search/3,
          id/1,
          user_id/1,
          date/1,
          type/1,
          event/1,
          source/1,
          topic/1,
          question/1,
          tags/1,
          note/1,
          id/2,
          user_id/2,
          date/2,
          type/2,
          event/2,
          source/2,
          topic/2,
          question/2,
          tags/2,
          note/2
        ]).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE, nnote).

init_table() ->
    mnesia:create_table(?TABLE,
    [ {disc_copies, [node()] },
      {attributes,
       record_info(fields, ?TABLE)}
    ]).

%% Copy and paste the following functions
put_record(Record) ->
    Insert =
        fun() ->
            mnesia:write(Record)
        end,
    {atomic, Results} = mnesia:transaction(Insert),
    Results.

get_all_values(Record) ->
    [_|Tail] = tuple_to_list(Record),
    Tail.

get_all() ->
    Query =
        fun() ->
            qlc:eval( qlc:q(
                [ Record || Record <- mnesia:table(?TABLE) ]
           ))
        end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

get_record(Key) ->
    Query =
        fun() ->
            mnesia:read({?TABLE, Key})
        end,
    {atomic, Results} = mnesia:transaction(Query),
    case length(Results) < 1 of
        true -> [];
        false -> hd(Results)
    end.

delete(Key) ->
    Insert =
        fun() ->
            mnesia:delete({?TABLE, Key})
        end,
    {atomic, Results} = mnesia:transaction(Insert),
    Results.


populate_record([Id, User_id, Type, Date, Event, Source,
                 Topic, Question, Tags, Note]) ->
    #nnote{id = Id,
           user_id = User_id,
           type = Type,
           date = Date,
           event = Event,
           source = Source,
           topic = Topic,
           question = Question,
           tags = Tags,
           note = Note
          }.

get_records_by_type(UserID, Type) ->
    Query =
        fun() ->
            qlc:eval( qlc:q(
                        [Record || Record <- mnesia:table(?TABLE),
                                   Record#nnote.id == UserID,
                                   Record#nnote.type == Type]
                       ))
        end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

get_records_by_date(UserID, Type, Date) ->
    DateTime = qdate:to_date(Date),
    {FirstDate, LastDate} = n_dates:date_span(DateTime, 7),
    Query =
        fun() ->
            qlc:eval( qlc:q(
                        [Record || Record <- mnesia:table(?TABLE),
                                   qdate:between(FirstDate, Record#nnote.date, LastDate),
                                   Record#nnote.user_id == UserID,
                                   Record#nnote.type >= Type
                        ]))
        end,

    {atomic, Results} = mnesia:transaction(Query),
    Results.

search(UserID, NoteType, SearchList) ->
    Query =
        fun() ->
            qlc:eval( qlc:q(
                        [Record || Record <- mnesia:table(?TABLE),
                                   Record#nnote.user_id == UserID,
                                   Record#nnote.type == NoteType,
                                   n_search:filter(SearchList, Record)]
                       ))
        end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.
