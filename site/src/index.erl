%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros
%% ***************************************************
-define(TEMPLATE,"./site/templates/n_apps.html").

-define(TITLE, "Welcome!").
-define(TOP, "Build it with Nitrogen").

%% ***************************************************
%% Template and Title
%% ***************************************************

main() -> #template { file="./site/templates/n_apps.html" }.

title() ->
    ?TITLE.

%% ***************************************************
%% Panel definitions
%% ***************************************************

top() ->
    #h1 {text=?TOP}.

main_menu() ->
    #h2 {text="Main Menu"}.

sidebar() ->
    #panel {text="Sidebar"}.

content() ->
    #panel {text="Content"}.
