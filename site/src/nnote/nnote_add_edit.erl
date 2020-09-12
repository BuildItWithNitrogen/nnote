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
url_vars() -> [id, note_type, task].

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
    #h2{class=content, text=[Action, " ",string:titlecase(NoteType)," Note"]}.

add_edit_form("new", NoteType) ->
    UserID = n_utils:get_user_id(),
    Date = qdate:to_string("Y-m-d"),
    form("new", UserID, NoteType, Date, "", "", "", "", "", "");
add_edit_form(ID, NoteType) ->
    Record = nnote_api:get_record(ID),
    [ID, UserID, NoteType, Date, Event, Source, Topic,
     Question, Tags, Note] = nnote_api:get_all_values(Record),
    form(ID, UserID, NoteType, Date, Event, Source, Topic,
         Question, Tags, Note).

form(ID, UserID, NoteType, Date, Event, Source, Topic,
     Question, Tags, Note) ->
    ShowEvent = show_event(NoteType),
    ShowSource = show_source(NoteType),
    ShowQuestion = show_question(NoteType),
    wf:defer(save_note, topic, #validate{validators=[
        #is_required{text="Topic required"}]}),
    wf:defer(save_note, note, #validate{validators=[
        #is_required{text="Note required"}]}),
    wf:defer(save_note, event, #validate{validators=[
        #is_required{text="Event required"}]}),
    wf:defer(save_note, source, #validate{validators=[
        #is_required{text="Source required"}]}),

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

event_label("conference") -> "conference";
event_label("lecture") -> "event";
event_label(_) -> "".

source_label("conference") -> "speaker";
source_label("idea") -> "";
source_label("lab") -> "";
source_label("lecture") -> "speaker";
source_label("web") -> "URL";
source_label(_) -> "source".

question_label("conference") -> "";
question_label("idea") -> "";
question_label("web") -> "";
question_label(_) -> "question".

show_event("conference") -> true;
show_event("lecture") -> true;
show_event(_) -> false.

show_source("idea") -> false;
show_source("lab") -> false;
show_source(_) -> true.

show_question("interview") -> true;
show_question("lab") -> true;
show_question("lecture") -> true;
show_question("research") -> true;
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
%% Sidebar menus
%% ***************************************************
side_menu("NOTE TYPE") ->
    [{"conference", {select,"conference"}},
     {"idea",       {select,"idea"}},
     {"interview",  {select,"interview"}},
     {"lab",        {select,"lab"}},
     {"lecture",    {select,"lecture"}},
     {"research",   {select,"research"}},
     {"web",        {select,"web"}}
    ].

%% ***************************************************
%% Saving Things
%% ***************************************************
save(ID, UserID, NoteType) ->
    Params = wf:mq([date, event, source, topic, question, tags, note]),
    Params2 = [ID, UserID, NoteType | Params],
    Record = nnote_api:populate_record(Params2),
    nnote_api:put_record(Record),
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
       MenuItem <- side_menu(Menu)]
    ].


