%% -*- mode: nitrogen -*-
-module (nnote_add_edit).
-compile(export_all).
-behavior(n_apps).
-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros
%% ***************************************************
-define(MMSELECTED, "nnote").
-define(TITLE, "Add/edit note").
-define(TOP, "nnote").
url_vars() -> [id, {note_type, atom}, {task, atom}].

%% ***************************************************
%% Template and Title
%% ***************************************************

main() ->
    n_common:template().

title() ->
    ?TITLE.

%% ***************************************************
%% Panel definitions
%% ***************************************************

top() ->
    #h1 {text=?TOP}.

main_menu() ->
    n_menus:show_main_menu(?MMSELECTED).

content(#{id:=undefined, note_type:=undefined}) ->
    #h2{class=content, text="My Notes"};
content(#{id:=ID, note_type:=NoteType}) ->
    [
     content_headline(ID, NoteType),
     add_edit_form(ID, NoteType)
    ].

content_headline(ID, NoteType) ->
    Action = case ID of
        "new" -> "Enter";
        _ -> "Edit"
    end,
    NoteType2 = wf:to_list(NoteType),
    #h2{class=content, text=[Action, " ",string:titlecase(NoteType2)," Note"]}.


%% This bit below where we're using a Map to pass arguments to form/1 works,
%% but even it has some fragility that might be worth exploring eliminating.
%% The simplest solution would be to make a new nnote_db_mnesia:new() function
%% which simply returns an initialized #nnote{} record. Then you could just use
%% the setters and getters to interact with the record.  This is the safest
%% approach, and would save a few lines of code, but this is manageable.

add_edit_form("new", NoteType) ->
    UserID = n_utils:get_user_id(),
    Date = qdate:to_string("m/d/Y"),
    Map = #{id=>"new",
            user_id=>UserID,
            type=>NoteType,
            date=>Date},
    Record = nnote_api:map_to_record(Map),
    form(Record);
add_edit_form(ID, _NoteType) ->
    Record = nnote_api:get_record(ID),
    form(Record). 

form(Record) ->
    ID = nnote_api:id(Record),
    UserID = nnote_api:user_id(Record),
    NoteType = nnote_api:type(Record),
    Date = nnote_api:date(Record),
    Event = nnote_api:event(Record),
    Source = nnote_api:source(Record),
    Tags = nnote_api:tags(Record),
    Topic = nnote_api:topic(Record),
    Question = nnote_api:question(Record),
    Note = nnote_api:note(Record),

    ShowEvent = show_event(NoteType),
    ShowSource = show_source(NoteType),
    ShowQuestion = show_question(NoteType),
    wf:defer(save_note, topic, #validate{validators=[
        #is_required{text="Topic required"}]}),
    wf:defer(save_note, note, #validate{validators=[
        #is_required{text="Note required"}]}),
    ?WF_IF(ShowEvent, wf:defer(save_note, event, #validate{validators=[
        #is_required{text="Event required"}]})),
    ?WF_IF(ShowSource, wf:defer(save_note, source, #validate{validators=[
        #is_required{text="Source required"}]})),

    [ #label{text="Date"},
      n_dates:datepicker(date, Date),
      #label{text=event_label(NoteType), show_if=ShowEvent},
      #textbox{id=event, text=Event, show_if=ShowEvent},
      #label{text=source_label(NoteType), show_if=ShowSource},
      #textbox{id=source, text=Source, show_if=ShowSource},
      #label{text="Topic"},
      #textbox{id=topic, text=Topic},
      #label{text=question_label(NoteType), show_if=ShowQuestion},
      #textbox{id=question, text=Question, show_if=ShowQuestion},
      #label{text="Search Words"},
      #textbox{id=tags, text=Tags},
      #label{text="Note"},
      #textarea{id=note, text=Note},
      #br{},
      #button{id=save_note, text=button_text(ID), postback={save_note, ID, UserID, NoteType}},
      #button{text="Cancel", postback=cancel}
    ].

%% ***************************************************
%% Content helpers
%% ***************************************************
button_text("new") -> "Enter new note";
button_text(_ID) -> "Submit changes".

event_label(conference) -> "conference";
event_label(lecture) -> "event";
event_label(_) -> "".

source_label(conference) -> "speaker";
source_label(idea) -> "";
source_label(lab) -> "";
source_label(lecture) -> "speaker";
source_label(web) -> "URL";
source_label(_) -> "source".

question_label(conference) -> "";
question_label(idea) -> "";
question_label(web) -> "";
question_label(_) -> "question".

show_event(conference) -> true;
show_event(lecture) -> true;
show_event(_) -> false.

show_source(idea) -> false;
show_source(lab) -> false;
show_source(_) -> true.

show_question(interview) -> true;
show_question(lab) -> true;
show_question(lecture) -> true;
show_question(research) -> true;
show_question(_) -> false.

%% ***************************************************
%% Tips
%% ***************************************************
tips() ->
    [ #h2{text="Tips & Info"},
      #p{body="The applications in this framework
         were developed by Jesse Gumm and
         Lloyd R. Prentice for their book
         <em>Build it with Nitrogen</em>. These
         applications are available for use and
         modification under the MIT License."}
    ].


%% ***************************************************
%% Saving Things
%% ***************************************************

%% Even if you end up doing the recommendations above add_edit_form(), I would
%% still leave this functionality as is, as this is a very easy method for
%% saving.
save(ID, UserID, NoteType) ->
    Map = wf:q_map([date, event, source, topic, tags, note]),
    Map2 = Map#{user_id=>UserID,
                type=>NoteType},
    Record = nnote_api:map_to_record(Map2),
    Record2 = case ID of
        "new" -> Record;
        _ -> nnote_api:id(Record, ID)
    end,
    nnote_api:put_record(Record2),
    Redirect = ["/nnote", "?",
                wf:to_qs([{note_type, NoteType} ]) ],
    wf:redirect(Redirect).



%% ***************************************************
%% Sidebar events
%% ***************************************************
event({select, NoteType}) ->
    Redirect = [wf:path(), "?",
                wf:to_qs([{id, "new"}, {note_type, NoteType} ]) ],
    wf:redirect(Redirect);
%% ***************************************************
%% Save Event
%% ***************************************************
event({save_note, ID, UserID, NoteType}) ->
    io:format("Save~n"),
    wf:wire(#confirm{text="Save?",
                     postback={confirm_save, ID, UserID, NoteType}});
event({confirm_save, ID, UserID, NoteType}) ->
    save(ID, UserID, NoteType);
event(cancel) ->
    wf:redirect("/nnote").

%% ***************************************************
%% Sidebar executives
%% ***************************************************
sidebar(#{note_type:=NoteType}) ->
    [ #h3 {text="SELECT"},
      show_side_menu("NOTE TYPE", NoteType)
    ].

%% ***************************************************
%% Sidebar functions
%% ***************************************************
show_side_menu(Menu, Selected) ->
    [ #h4 {class=select, text=Menu},
      [n_menus:show_menu_item(MenuItem, Selected) ||
       MenuItem <- n_menus:note_type_side_menu()]
    ].


