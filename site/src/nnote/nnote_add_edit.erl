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
    %% We’ll do more here when we set up editing
    [].

form(ID, UserID, NoteType, Date, Event, Source, Topic,
     Question, Tags, Note) ->
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
      #label{text="Event"},
      #textbox{id=event, text=Event},
      #label{text="Source"},
      #textbox{id=source, text=Source},
      #label{text="Topic"},
      #textbox{id=topic, text=Topic},
      #label{text="Question"},
      #textbox{id=question, text=Question},
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
%% Sidebar events
%% ***************************************************
event({select, NoteType}) ->
    Redirect = [wf:path(), "?",
                wf:to_qs([{id, "new"}, {note_type, NoteType} ]) ],
    wf:redirect(Redirect).

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


