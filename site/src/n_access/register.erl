%% -*- mode: nitrogen -*-
-module (register).
-compile(export_all).
-behavior(n_apps).
-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros
%% ***************************************************
-define(MMSELECTED, "home").
-define(TITLE, "Registration Page").
-define(TOP, "Build it with Nitrogen").
url_vars() -> [].

access() -> public.

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
    new_account_form().

new_account_form() ->
    wf:defer(save, username, #validate{validators=[
        #is_required{text="Username Required"}]}),
    wf:defer(save, email, #validate{validators=[
        #is_required{text="Email Required"}]}),
    wf:defer(save, password, #validate{validators=[
        #is_required{text="Password Required"}]}),
    wf:defer(save, password2, #validate{validators=[
        #confirm_same{text="Passwords do not match",
                      confirm_id=password}]}),
    [#h1{text="Create Account"},
     #label{text="Username"},
     #textbox{id=username, placeholder="Your Username"},
     #label{text="Email"},
     #textbox{id=email, placeholder="your@email.com"},
     #label{text="Password"},
     #password{id=password},
     #label{text="Confirm Password"},
     #password{id=password2},
     #br{},
     #button{id=save, text="Save Account", postback=save}
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
%% Sidebar events
%% ***************************************************
event({goto, Link}) ->
    wf:redirect(Link).

%% ***************************************************
%% Sidebar executives
%% ***************************************************
sidebar(#{}) ->
    [].



