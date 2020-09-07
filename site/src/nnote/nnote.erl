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

content(#{}) ->
    content_headline().

content_headline() ->
    [#h2 {class=content, text="My Notes"}].

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
side_menu("WEB SITE") ->
    [{"nitrogen", {goto,
                   "http://nitrogenproject.com/"}},
     {"erlang", {goto,
                 "http://erlang.org/doc/apps/stdlib/"}},
     {"hacker news", {goto,
                      "https://news.ycombinator.com/"}}
    ].

%% ***************************************************
%% Sidebar events
%% ***************************************************
event({goto, Link}) ->
    wf:redirect(Link).

%% ***************************************************
%% Sidebar executives
%% ***************************************************
sidebar(#{}) ->
    [ #h3 {text="SELECT"},
      show_side_menu("WEB SITE", unselected)
    ].

%% ***************************************************
%% Sidebar functions
%% ***************************************************
show_side_menu(Menu, Selected) ->
    [ #h4 {class=select, text=Menu},
      [n_menus:show_menu_item(MenuItem, Selected) ||
       MenuItem <- side_menu(Menu)]
    ].


