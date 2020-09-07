%% -*- mode: nitrogen -*-
-module (nnote).
-compile(export_all).
-behavior(n_apps).
-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros
%% ***************************************************
-define(MMSELECTED, "nnote").
-define(TITLE, "Welcome to nnote!").
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

content(#{note_type:=undefined, task:=undefined}) ->
    [content_headline(),
     #p{text="Select note type."}
    ];
content(#{note_type:=NoteType, task:=Task}) ->
    Records = case Task of
                  undefined -> undefined;
                  search_by_tag -> tag_search(NoteType);
                  search_by_date -> date_search(NoteType)
              end,
    display_forms(NoteType, Records).

tag_search(NoteType) ->
    [].
date_search(NoteType) ->
    [].

search_results(undefined) ->
    [];
search_results([]) ->
    [#hr{},
     #h2{text="Search Results"},
     #p{text="No notes found"}
    ];
search_results(Records) ->
    [#hr{},
     #h2{text="Search Results"},
     [n_utils:draw_link(Record) || Record <- Records]
    ].


%% ***************************************************
%% Content
%% ***************************************************
display_forms(NoteType, Records) ->
    [content_headline(),
     add_note_button(NoteType),
     search_by_tag(),
     search_by_date(),
     search_results(Records)
    ].



content_headline() ->
    [#h2{class=content, text="My Notes"}].

add_note_button(NoteType) ->
    ButtonText = ["Enter new ",NoteType," note"],
    #button{text=ButtonText, postback={add_note, NoteType}}.

search_by_tag() ->
    [#label{text="enter search words"},
     #textbox{id=search_words},
     #button{text="Search", postback=search_by_tag},
     #button{text="Info", postback={info, search_by_tag}}
    ].

 search_by_date() ->
     io:format("Search by date~n"),
     [ #label{text="enter date"},
       n_dates:datepicker(search_date, ""),
       #button{text="Search", postback=search_by_date},
       #button{text="Info", postback={info, search_by_date}}
     ].

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
%% Info
%% ***************************************************
info(search_by_tag) ->
    [ #h2{body=["<i>Search Words</i>"]},
      #p{text=["Search word documentation goes here"]}
    ];
info(search_by_date) ->
    [ #h2{body="<i>Search Date</i>"},
      #p{text="Search date documentation goes here"}
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


event(search_by_tag) ->
    NoteType = wf:q(note_type),
    Content = content(#{note_type=>NoteType, task=>search_by_tag}),
    wf:update(content, Content);
event(search_by_date) ->
    NoteType = wf:q(note_type),
    Content = content(#{note_type=>NoteType, task=>search_by_date}),
    wf:update(content, Content);
%% ***************************************************
%% Info events
%% ***************************************************
event({info, Function}) ->
    wf:flash(info(Function));
%% ***************************************************
%% Sidebar events
%% ***************************************************
event({select, NoteType}) ->
    Redirect = [wf:path(), "?",
                wf:to_qs([ {note_type, NoteType} ]) ],
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


