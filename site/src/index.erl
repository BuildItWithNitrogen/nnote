%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/n_apps.html" }.

title() ->
    "Welcome to nnote".

top() ->
    #h1 {text="Build it with Nitrogen"}.

main_menu() ->
    #h2 {text="Main Menu"}.

sidebar() ->
    #panel {text="Sidebar"}.

content() ->
    #panel {text="Content"}.
