:- module(discord_client, [
        discord_start_gateway/0,
        discord_say/2,
        discord_send/1,
        ping_discord/0,
        discord_get_websocket/1,
        is_thread_running/1,
        discord_ensure_im/2,
        any_to_id/2]).

/** <module> discord_client - Provides a websocket API to write discord clients

*/

disable_gateway:- false.  % true until we fix our websocket code

discord_restore_0:- 
  stream_property(X,file_no(2)),asserta(tmp:discord_debug(X)),
  mutex_create(last_done_mutex),
  mutex_create(connection_mutex).

:- initialization(discord_restore_0, now).
:- initialization(discord_restore_0, restore).


:- multifile(tmp:discord_info/3).
:- volatile(tmp:discord_info/3).
:- dynamic(tmp:discord_info/3).
reload_discord_info:- reconsult(guild_info_file).
%load_discord_info:- !.
load_discord_info:- exists_file(guild_info_file)->consult(guild_info_file);true.
clear_discord_info:- retractall(tmp:discord_info/3).
save_discord_info:- 
 setup_call_cleanup(open(guild_info_file,write,O),
  save_discord_info(O),
  close(O)).

tmp_r(R,TmpR,TmpRG):- functor(R,discord_info,3), TmpR=tmp:R, TmpRG = (TmpR, \+ discord_is_secret(R)).

save_discord_info(O):-
  tmp_r(R,TmpR,TmpRG),
  functor(R,F,A), Dyn = tmp:F/A, 
  format(O,'~n~q.~n~q.~n~q.~n',
   [(:- multifile(Dyn)),
    (:- volatile(Dyn)),
    (:- dynamic(Dyn))]),      
  forall(TmpRG,format(O,'~q.~n',[TmpR])).

show_discord_info:-
  tmp_r(R,_TmpR,TmpRG),
  forall(TmpRG,ddbg_always(R)).
show_discord_info_raw:-
  tmp_r(R,_TmpR,TmpRG),
  forall(TmpRG,ddbg_raw(R)).
show_discord_info(Str):-
 tmp_r(R,_TmpR,TmpRG),
 forall(TmpRG,
 ignore((
   discord_debug_cvt(R,RD),sformat(S,'~w ~q',[RD,R]),
   matches_info(S,Str),
   ddbg_always(RD)))).

show_discord_info_raw(Str):-
 tmp_r(R,_TmpR,TmpRG),
 forall(TmpRG,
 ignore((
   discord_debug_cvt(R,RD),sformat(S,'~w ~q',[RD,R]),
   matches_info(S,Str),
   ddbg_raw(R)))).


:- if( \+ prolog_load_context(reloading, true)).
:- at_halt(save_discord_info).
:- load_discord_info.
:- endif.

discord_grouping(messages).
discord_grouping(channels).
discord_grouping(roles).
discord_grouping(members).
discord_grouping(presences).
discord_grouping(guilds).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(url)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/websocket)).

:- if(exists_source(library(dicts))).
	:- use_module(library(dicts)).
:- endif.

:- use_module(library(logicmoo_utils)).

:- meta_predicate(notrace_catch(:)).
notrace_catch(G):- notrace(catch(G,_,fail)).
debug_console(G):- (tmp:discord_debug(X);X=user_error), with_output_to(X,G).

if_debug(_).


:- thread_local(t_l:hide_ddbg/0).

:- meta_predicate(without_ddbg(:)).
without_ddbg(G):- locally(t_l:hide_ddbg,call(G)).


%ddbg(O):- sub_var("PRESENCE_UPDATE",O),!.
%ddbg(_):- \+ thread_self(main),!.
ddbg(_):- t_l:hide_ddbg,!.
ddbg(discord_addd(s,hasValue,_)).
ddbg(O):- ddbg_always(O).
%ddbg_always(O):- discord_debug_cvt(O,OO), !, debug_console(in_cmt(fmt(OO))).
ddbg_always(O):- discord_debug_cvt(O,OO),!, ddbg_raw(OO).
ddbg_raw(OO):-
 locally(current_prolog_flag(debugger_write_options,[quoted(true), portray(true), 
   max_depth(500), attributes(portray), spacing(next_argument)]),
   debug_console(in_cmt(print_tree(OO)))).
%ddbg_always(F,Args):- if_debug((fresh_line, format(user_error,F,Args))).

is_thread_running(ID):-
  is_thread(ID), thread_property(ID,status(What)),!,
   (What==running->true;(thread_join(ID,_ ),!,fail)).


:- dynamic(tmp:discord_token/1).

% ===============================================
% How this module might find your token:
% ===============================================

% 1st - Checks for a local declaration 
%  (if the next line is uncommented and replaced by a real token )
% tmp:discord_token('xoxb-01234567890-xxxxxxxxxxxxxxxx').
find_token:- tmp:discord_token(_),!.
% 2nd - Checks for a local file called ".discord_auth.pl" for tmp:discord_token/1 as above
find_token:- \+ tmp:discord_token(_), exists_file('.discord_auth.pl'), consult('.discord_auth.pl'), !.
% 3rd - Checks env for DISCORD_API_TOKEN
%  ( defined by# export DISCORD_API_TOKEN=xoxb-01234567890-xxxxxxxxxxxxxxxx )
find_token:-  \+ tmp:discord_token(_), getenv('DISCORD_API_TOKEN',Was), asserta(tmp:discord_token(Was)), !.
% 4th - Checks users config directory for file called ".discord_auth.pl"  tmp:discord_token/1 as above
find_token:- \+ tmp:discord_token(_), expand_file_name('~/.discord_auth.pl',[X]), exists_file(X), consult(X), !.
find_token:- throw(missing(tmp:discord_token(_))).

:- find_token.

:- dynamic(tmp:discord_websocket_event/2).
:- dynamic(tmp:discord_chat_event/2).
:- dynamic(tmp:last_disconnect/1).
% ===============================================
% Utility functions
% ===============================================

discord_token_string(S):-tmp:discord_token(T),atom_string(T,S).

%curl -H "Authorization: Bot $AUTH_TOK" -H "User-Agent: DiscordBot" -H "Content-Type: application/json" 
%  https://discord.com/api/v9/

discord_update(me):-  discord_http(users/'@me').
discord_update(guilds):- discord_update(me), discord_http(users/'@me'/guilds).
discord_update(channels):- discord_update(guilds),
   %discord_http(guilds/{guilds-id}),
   discord_http(guilds/{guilds-id}/channels),
   %discord_http(guilds/{guilds-id}/members),
   %discord_http(guilds/{guilds-id}/roles),
   !.

/*
request_members:- 
discord_send({
  "op": 8,
  d: {msg

    "guild_id": $guild_id,
    "query": "",
    "limit": 0
  }
}).
*/

% ===============================================
% Thread Pool Models
% ===============================================

ensure_thread_model2(M,Already):-
 \+ thread_self(M),!, (is_thread_running(M) -> call(Already) ; show_call(thread_create(M,_,[alias(M),detached(true)]))).
ensure_thread_model(M,Pause,G):- ensure_thread_model2(M,G) -> true ; (call(Pause),call(G),call(M)),!.
%ensure_thread_model(M,Pause,G):- 

ping_discord:- disable_gateway,!.
ping_discord:- ensure_thread_model(ping_discord,ping_sleep,do_task(send_ping)).

send_ping:- remember_task(send_ping),discord_send({"op": 1, "d": $s}).
ping_sleep:- discord_ddd(heartbeat_interval,hasValue,Time)->sleep(Time);sleep(30).


%deque_discord_events:- disable_gateway,!.
deque_discord_events:- ensure_thread_model(deque_discord_events,sleep(1),do_task(handle_discord_websocket_events)).


discord_proc_tasks:- ensure_thread_model(discord_proc_tasks, sleep(1), check_4_tasks).

discord_message_checking_01:- disable_gateway,!.
discord_message_checking_01:-  
  ensure_thread_model(discord_message_checking_01,
  sleep(1), check_4_tasks).

discord_message_checking_02:-  
  ensure_thread_model(discord_message_checking_02,
  sleep(1), check_4_tasks).


check_seen:- forall(discord_ddd(MID,content,_),check_seen(MID)).

check_seen(MID):- discord_ddd(MID, seen, true),!.
check_seen(MID):- discord_ddd(MID, timestamp, When), get_time(Now), When + ( 86400 * 3) < Now, !. % over 3 days old
check_seen(MID):- \+ discord_ddd(MID,content,_), rtrv_message(MID),fail.
check_seen(MID):- add_seen(MID).

rtrv_message(MID):- discord_ddd(MID, seen, true),!.
rtrv_message(MID):- discord_ddd(MID,content,_),!.
rtrv_message(MID):- m_to_c(MID,CID),!,ignore(discord_http(channels/CID/messages/MID)),!.
rtrv_message(_):- !.

m_to_c(MID,CID):- discord_ddd(MID,channel_id,CID).
m_to_c(MID,CID):- discord_ddd(CID,last_message_id,MID).

add_seen(ID):- discord_ddd(ID, seen, true),!.
add_seen(ID):- discord_addd(ID, seen, true), fail.
add_seen(ID):- m_to_c(ID,CID),discord_http(channels/CID/messages/ID/reactions/'%F0%9F%91%80'/'@me',[method(put)]).
add_unseen(ID):- m_to_c(ID,CID),discord_http(channels/CID/messages/ID/reactions/'%F0%9F%91%80'/'@me',[method(delete)]).

rtrv_new_messages :- 
 forall(
 (discord_ddd(ChannelID,last_message_id,MessageID),
  \+ discord_ddd(MessageID,content,_)),
  (sleep(0.2),ignore(discord_http(channels/ChannelID/messages/MessageID)))).

check_4_tasks:- forall(how_often(Often,Task),ignore(check_since(Often,Task))),fail.
check_4_tasks.

:- volatile(tmp:doing_task/1).
:- dynamic(tmp:doing_task/1).
:- volatile(tmp:last_done/2).
:- dynamic(tmp:last_done/2).

check_since(never,_Task):-!.
check_since(_Often,Task):- with_mutex(last_done_mutex, (\+ tmp:last_done(Task,_),remember_task(Task))),!,do_task(Task).
check_since(once,_Task):-!.
check_since(Often,Task):- with_mutex(last_done_mutex, ((tmp:last_done(Task,When), get_time(Now), When+Often < Now, remember_task(Task)))),do_task(Task).

do_task(Task):- tmp:doing_task(Task),!.
do_task(Task):-
  setup_call_cleanup(asserta(tmp:doing_task(Task),R),
     setup_call_cleanup(
         remember_task(Task), ignore(call(Task)), remember_task(Task)),erase(R)).

remember_task(Task):- with_mutex(last_done_mutex, 
  ignore((retractall(tmp:last_done(Task,_)),get_time(Time),asserta(tmp:last_done(Task,Time))))).

%how_often(200, guilds).
%how_often(30, channels).

how_often(5, handle_discord_websocket_events).
%how_often(41, send_ping).
%how_often(2, handle_chat_events).
/*
how_often(once, discord_update(channels)).
how_often(300, rtrv_new_messages).
how_often(500, rtrv_dm_handles).
*/
%how_often(600, show_discord_tasks).

handle_discord_websocket_events:- remember_task(handle_discord_websocket_events),  
  handle_discord_websocket_event,fail.
handle_discord_websocket_events:- 
  handle_chat_events,fail.
handle_discord_websocket_events.

handle_discord_websocket_event:-
  retract(tmp:discord_websocket_event(Type,Message))*->ignore(discord_websocket_hook_1(Type,Message));true.

handle_chat_events:- 
 handle_chat_event,fail.
handle_chat_events:- remember_task(handle_chat_events).

handle_chat_event:- 
  (retract(tmp:discord_chat_event(Type,Message))*->ignore(handle_discord_chat_event(Type,Message));true).


add_discord_chat_event(ID,Message):- assertz(tmp:discord_chat_event(ID,Message)).

handle_discord_chat_event(ID,_Message):- discord_ddd(_,referenced_message,ID),!.
handle_discord_chat_event(ID,_Message):- discord_ddd(ID,referenced_message,_),!.
handle_discord_chat_event(ID,content(Message)):-  
  discord_ddd(ID,author_username,User), !, 
  handle_discord_chat_event(ID,say(User,Message)).

handle_discord_chat_event(ID,Message):- remember_task(handle_chat_events), dmsg(chat_event(ID,Message)),fail.
handle_discord_chat_event(_ID,say(User,_Message)):- User=="PrologMUD",!.
handle_discord_chat_event(ID,say(User,Message)):- !,
  term_to_atom(ID,DEST), atom_string(DEST,DESTS), trim_message(Message,Str), 
  without_ddbg(external_chat_event(discord_client:discord_chat_override,DESTS,User,say(Str))),!.
handle_discord_chat_event(ID,Message):- dmsg(failed_chat_event(ID,Message)).

trim_message(A,C):-  split_string(A, "", "`\s\t\n", [B]), A\==B,!,trim_message(B,C).
trim_message(A,A).

  %discord_ddd(ID,channel_id,Channel),!,


:- use_module(library(eggdrop)).


discord_write_done(det('Yes',_)):-!. 
discord_write_done(Data):- write(' '),write(Data).

discord_show_each_result(_CMD,Done,_,[]):- !, format(' '), Done==true, write("% Yes.").

discord_show_each_result(_CMD,Done,N,Vs):- 
  once(once((Done==true -> (once(\+ \+ write_varvalues2(Vs))) ; (once(\+ \+ write_varvalues2(Vs)),N>200)))).

discord_chat_say(DEST,Msg):-
 text_to_string(DEST,Channel),
 text_to_string(Msg,Str),
  discord_say(Channel,Str),!.

%discord_chat_override(Goal):- dmsg(discord_chat_override(Goal)),fail.
discord_chat_override(put_msg(DEST,Msg)):- !, discord_chat_say(DEST,Msg).
discord_chat_override(put_notice(DEST,Msg)):- !, discord_chat_say(DEST,Msg).
discord_chat_override(Goal):- functor(Goal,F,A),atom_concat('discord_',F,DF), discord_client:current_predicate(DF/A), !,
  Goal=..[F|Args],DGoal=..[DF|Args],!,call(DGoal).
discord_chat_override(Goal):- dmsg(skipped_chat_override(Goal)).
%discord_chat_override(Goal):- discord_client:call(Goal). % in-case there are new ones

%how_often(300, rtrv_messages_chan).
%how_often(10, discord_message_checking_01).
%how_often(10, discord_message_checking_02).
:- meta_predicate(discord_show_list(?)).
discord_show_list(TmpR):- forall(tmp:TmpR,format(' ~q.~n',[TmpR])).

show_discord_tasks:- 
 remember_task(show_discord_tasks),
 discord_show_list(discord_websocket_event(_,_)),
 discord_show_list(discord_chat_event(_,_)),
 findall(Task,tmp:doing_task(Task),TaskList),ttyflush,format('~N~n%~~ (secs)\tdoing: ~q~n',[TaskList]),
 findall(Task,tmp:last_done(Task,_);tmp:doing_task(Task);how_often(_,Task),TaskL),list_to_set(TaskL,TaskS),
 forall(member(Task,TaskS),ignore(show_discord_task(Task))).

show_discord_task(Task):- tmp:last_done(Task,When),!,show_discord_task(Task,When).
show_discord_task(Task):- show_discord_task(Task,never).
show_discord_task(Task,When):-  ignore(how_often(Often,Task)),ignore(Often=whenever), 
 ignore((number(When), get_time(Now), Ago is integer(Now-When))), ignore(Ago=When),
 format('~N%~~ ~w  \t~q.',[Ago/Often,Task]).

% https://discord.com/oauth2/authorize?response_type=code&client_id=157730590492196864&scope=identify+guilds.join&state=15773059ghq9183habn&redirect_uri=https%3A%2F%2Fgoogle.com&prompt=consent
rtrv_messages_chan:- \+ thread_self(discord_message_checking_01), forall(any_to_chan_id(_Channel,ID),  rtrv_messages_chan(ID)).
rtrv_messages_chan(ID):- discord_ddd(ID,last_message_id,MessageID),discord_ddd(MessageID,content,_),!.
rtrv_messages_chan(ID):- discord_http(channels/ID/'messages?limit=20'),sleep(1).

% should return wss://gateway.discord.gg
%discord_get_websocket_url('wss://127.0.0.1:50051/'):-!.
discord_get_websocket_url('wss://gateway.discord.gg/?v=9&encoding=json'):-!.
discord_get_websocket_url(URL):- discord_http(gateway), get_discord(url,URL).

into_discord_url_object(UCmd,Prop):- atomic_list_concat([Prop2,_|_],'?',UCmd),!,into_discord_url_object(Prop2,Prop).
into_discord_url_object(UCmd,Prop):- atomic_list_concat([_,I|ST],'/',UCmd),member_rev(Prop2,[I|ST]), \+ atom_number(Prop2,_),!,into_discord_url_object(Prop2,Prop).
into_discord_url_object(Prop,Prop).

member_rev(X,[_|L]):-  member_rev(X,L).
member_rev(X,[X|_]).

check_pausing:- ignore((retract(tmp : discord_info(_,retry_after,Sleep)),sleep(Sleep),sleep(0.3))).

discord_http(Cmd):- discord_http(Cmd,[]),!.

discord_http(Cmd,Opts):- notrace(discord_http_0(Cmd,Opts)).
discord_http_0(Cmd,Opts):-
 must_det_l((
  expand_discoure_host_post(Cmd,UCmd),
  into_discord_url_object(UCmd,Prop),
  discord_http(Prop,Cmd,Opts))).
discord_http(Prop,Cmd,Opts):- select(json(JSON),Opts,OptsWo), \+ is_dict(JSON), \+ atomic(JSON),
  any_to_json_dict(JSON,Dict),!,discord_http(Prop,Cmd,[json(Dict)|OptsWo]). 
discord_http(Prop,Cmd,Opts):- select(post(json(JSON)),Opts,OptsWo), \+ is_dict(JSON), \+ atomic(JSON),
  any_to_json_dict(JSON,Dict),!,discord_http(Prop,Cmd,[post(json(Dict))|OptsWo]). 

discord_http(Prop,Cmd,Opts):- member(method(_),Opts), \+ member(post(_),Opts),!,discord_http(Prop,Cmd,[post([])|Opts]).
discord_http(Prop,Cmd,Opts):-
 must_det_l((  
  expand_discoure_host_post(Cmd,UCmd),
  bot_discord_token(TokenHeader),
  sformat(URL,'https://discord.com/api/v9/~w',[UCmd]))),
  %wdmsg(URL=Opts),
  check_pausing,
  http_open(URL, In, [status_code(Status), 
          request_header('Authorization'=TokenHeader), 
          request_header('User-Agent'='DiscordBot'),
          request_header('Content-Type'='application/json')|Opts]),
  ignore((\+ ok_satus_code(Status),dmsg((URL:Opts=Status)))),
  must_det_l((
    read_stream_to_codes(In,Codes),notrace_catch(close(In)),text_to_string(Codes,Str),
    read_codes_response(Status,Str,Term),discord_receive(Prop,Term))),
  !, nop(ok_satus_code(Status)).

ok_satus_code(Code):- var(Code),!,fail.
ok_satus_code(200).
ok_satus_code(204).

read_codes_response(_Status,String,Term):- String\=="",string_to_dict(String,Term),!.
read_codes_response(Status,Term,reply(Status,Term)).



expand_discoure_host_post(A,O):- \+ compound(A),!,A=O.
expand_discoure_host_post('$'(A),O):- !, get_discord(A,M),expand_discoure_host_post(M,O),!.
expand_discoure_host_post(A / B,O):- !, expand_discoure_host_post(A,AA),expand_discoure_host_post(B,BB),!,sformat(O,"~w/~w",[AA,BB]).
expand_discoure_host_post({A - B},O):- !, get_discord(A,B,M),expand_discoure_host_post(M,O).
expand_discoure_host_post(A,O):- A=O.

:- dynamic(tmp:discord_websocket/3).

% use dmiles proxy impl first?
discord_get_websocket(WS):- tmp:jpl_websocket(WS),!.
discord_get_websocket(WS):- tmp:discord_websocket(WS,_,_),!.
discord_get_websocket(WS):-
   discord_get_websocket_url(URL),!,
   discord_open_websocket(URL,WS),!.

:- dynamic(tmp:jpl_websocket/1).

discord_open_websocket(URL,WS):-
   ignore(tmp:discord_websocket(OLD_WS,_,_)),
   %atom_concat(URL,'?v=9&encoding=json',UrlV9),
   http_open_websocket(URL, WS, []),
   stream_pair(WS,I,O),
   show_call(asserta(tmp:discord_websocket(WS,I,O))),
   (nonvar(OLD_WS)->discord_remove_websocket(OLD_WS);true).
   

discord_remove_websocket(OLD_WS):-
   ignore(retract(tmp:discord_websocket(OLD_WS,_,_))),
   ignore(catch(my_ws_close(OLD_WS,1000,''),_,true)).

% ===============================================
% Property Names
% ===============================================

discord_start_gateway:- disable_gateway,!.
discord_start_gateway:- tmp:jpl_websocket(_),!.
discord_start_gateway:- is_thread_running(discord_start_gateway),!.
%discord_start_gateway:- discord_gateway_proc,!.
discord_start_gateway:- \+ thread_self(discord_start_gateway), 
  !,thread_create(discord_gateway_proc,_,[alias(discord_start_gateway)]),!.

string_to_dict:-
 string_to_dict("{\"type\":\"dnd_updated_user\",\"user\":\"U3T3R279S\",\"dnd_status\":{\"dnd_enabled\":false,\"next_dnd_start_ts\":1,\"next_dnd_end_ts\":1},\"event_ts\":\"1485012634.280271\"}",Dict),
  ddbg_always(Dict).


string_to_dict(Text,Dict):-
   text_to_string(Text,String),
   setup_call_cleanup(open_string(String,Stream),
    notrace_catch(json_read_dict(Stream,Dict)),
    close(Stream)),!.
string_to_dict(Text,Dict):- atom_json_dict(Text,Dict,[value_string_as(string),null(null),true(true),false(false)]).

discord_join_subchannel(ID):- discord_http(channels/ID/'thread-members'/'@me',[method(put)]).

system:discord_websocket_hook(Type,Message):- discord_client:discord_websocket_client_hook(Type,Message).

discord_websocket_client_hook(onOpen,_):- ping_discord.
discord_websocket_client_hook(Event,Message):- discord_reconn_after(Event),!, 
  writeln(user_error,discord_event(Event,Message)), discord_reconnect.
discord_websocket_client_hook(Event,Message):- assertz(tmp:discord_websocket_event(Event,Message)).

discord_gateway_proc:- tmp:jpl_websocket(_),!.
discord_gateway_proc:- discord_connect,!.
discord_gateway_proc:-
 call_cleanup((  
  repeat,
  once(discord_get_websocket(WS)),
  once(my_ws_receive(WS,Data,[format(json)])),
  (Data==
    end_of_file->!;
  (once(discord_receive(gateway,Data)),flush_output,fail))),
  discord_remove_websocket(WS)).

my_ws_close(Ws,Data,Options):- ws_close(Ws,Data,Options).
my_ws_receive(Ws,Data,Options):- ws_receive(Ws,Data,Options).

undict(ID,IDO):- is_dict(ID),ID.IDK=IDV,IDK=id,IDO=IDV.
undict(ID,ID).

discord_websocket_hook_1(_,_):- remember_task(handle_discord_websocket_events),fail.
discord_websocket_hook_1(Type,Message):- \+ atomic(Message),!,writeln(user_error,discord_event(Type,Message)),discord_event(Type,Message).
discord_websocket_hook_1(onMessage,Message):- atom_contains(Message,'"op":11'),!.
%discord_websocket_hook_1(onMessage,Message):- atom_json_dict(Message,Dict,[as(string)]),!,discord_event(gateway,Dict).
discord_websocket_hook_1(onMessage,Message):- string_to_dict(Message,Dict),!,discord_event(gateway,Dict).
%discord_websocket_hook_1(Type,Message):- notrace_catch(atom_json_term(Message,Dict,[as(string)])),!,discord_event(Type,Dict).
%discord_websocket_hook_1(Type,Message):-notrace_catch(atom_to_term(Message,Dict,_)),!,discord_event(Type,Dict).
discord_websocket_hook_1(Type,Message):- writeln(user_error,discord_event(Type,Message)),discord_event(Type,Message),!.

discord_reconn_after(onCreateError).
discord_reconn_after(sendTextError).
discord_reconn_after(onClose).
discord_reconn_after(sendTextError).



discord_event(onMessage,Data):- !,discord_event(gateway,Data).
discord_event(Type,{O}):- !, any_to_json_dict({O},Dict),!,discord_event(Type,Dict).

% OP==0 an event was dispatched.
discord_event(_,O):- get_prop(O,op,0),get_prop(O,t,Type),get_prop(O,s,S),get_prop(O,d,Data),!,
  discord_add(s,S),!, discord_event(Type,Data).  
discord_event("PRESENCE_UPDATE",Data):- !, discord_addd(presence,hasMember,Data).
discord_event(gateway,O):- get_prop(O,op,Type),!,discord_receive(Type,O),!.
%discord_event(gateway,O):- is_dict(O),O.Key=Type,Key=id,!,discord_receive(Type,O),!.
% simplify the data objects
%discord_event(Type,O):- is_dict(O),O.Key=Data,Key=data,!,discord_receive(Type,Data),!.
discord_event(Type,O):- get_prop(O,data,Data),!,discord_receive(Type,Data),!.
% typify the data objects
%discord_event(gateway,O):- is_dict(O),O.Key=Type,Key=type,!,discord_receive(Type,O),!.
discord_event(_,O):- get_prop(O,type,Type),!,discord_receive(Type,O),!.
discord_event(Evt,end_of_file):- !, throw(discord_event(Evt,end_of_file)).
discord_event(Type,Dict):- discord_receive(Type,Dict),!.


discord_unused(user_typing).
discord_unused(reconnect_url).

discord_receive(heartbeat_ack(11),_Data):- !.
discord_receive(Type,Data):- number(Type), gw_op(Type,Dispatch,_,_), Type\==Dispatch,!,discord_receive(Dispatch,Data).
discord_receive(Type,Data):- string(Data),string_to_dict(Data,Dict),!,discord_receive(Type,Dict).
%discord_receive(Type,Data):- ddbg_always((discord_receive(Type,Data))),fail.
discord_receive(Type,Data):- once(discord_add(Type,Data)),fail.
discord_receive(_Type,_Data).


discord_connect:- with_mutex(connection_mutex,discord_connect_0).
discord_disconnect:- with_mutex(connection_mutex,discord_disconnect_0).
discord_reconnect:- with_mutex(connection_mutex,discord_reconnect_0).


%discord_disconnect:- \+ mutex_trylock(connection_mutex), mutex_unlock(connection_mutex).
discord_disconnect_0:- retractall(tmp:last_disconnect(_)), get_time(Time), asserta(tmp:last_disconnect(Time)),fail.
discord_disconnect_0:- tmp:jpl_websocket(O), catch(jpl_call(O,close,[],_),_,true), fail.
discord_disconnect_0:- retractall(tmp:jpl_websocket(_)).

% This may be triggered by several events at once (so it has to hapen at least 10 seconds appart
discord_reconnect_0:- tmp:last_disconnect(Before), get_time(Time), Before+10 <  Time,!.
discord_reconnect_0:- discord_disconnect_0, fail.
discord_reconnect_0:- discord_connect_0, fail.
discord_reconnect_0.

discord_connect_0:- tmp:jpl_websocket(_),!.
discord_connect_0:- %setenv('CLASSPATH','/opt/logicmoo_workspace/packs_sys/swicli/build/application/classes:/opt/logicmoo_workspace/packs_sys/swicli/lib/javax.websocket-api-1.0.jar'),
  discord_get_websocket_url(URL),
  jpl_new('PrologWebSocket',[URL,'discord_websocket_hook'],O),
  assert(tmp:jpl_websocket(O)),!.

discord_identify:- 
 discord_send(
  {"op": 2,
  d: {
    "token": $token,
    "properties": {
      "$os": "linux",
      "$browser": "disco",
      "$device": "disco"
    }, 
  "intents": 65535
  }
}),
 nop(discord_add_slash).

discord_add_slash:- 
 discord_http(applications/772113231757574185/command,
 [post(
 json({
    "name": "blep",
    "type": 1,
    "description": "Send a random adorable animal photo",
    "options": [
        {
            "name": "animal",
            "description": "The type of animal",
            "type": 3,
            "required": true,
            "choices": [
                {
                    "name": "Dog",
                    "value": "animal_dog"
                },
                {
                    "name": "Cat",
                    "value": "animal_cat"
                },
                {
                    "name": "Penguin",
                    "value": "animal_penguin"
                }
            ]
        },
        {
            "name": "only_smol",
            "description": "Whether to show only baby animals",
            "type": 5,
            "required": false
        }
    ]
}))]).
discord_resume:- 
 discord_send( {
  "op": 6,
  d: {
    "token": $token,
    "session_id": $session_id,
    "seq": $s
  }
}).

rtrv_dm_handles:-  forall(discord_ddd(UserID,instanceOf,members), rtrv_dm_handle(UserID)).

rtrv_dm_handle(UserID):-
 %discord_ddd(UserID,instanceOf,members),
 notrace(( 
   ignore((
    \+ discord_ddd(UserID,bot,true),
    \+ (discord_ddd(ID,recipient,UserID),
     discord_ddd(ID,recipient_name,_Name),
     discord_ddd(ID,type,1),
     discord_ddd(ID,instanceOf,channels)),
 rtrv_dm_handle_now(UserID))))).

rtrv_dm_handle_now(UserID):- integer(UserID), !, 
  (discord_http(users/'@me'/channels,[post(json(_{recipient_id:UserID}))]) -> true ;
   show_discord_info_raw(UserID)).
rtrv_dm_handle_now(Name):- maybe_into_string(Name,SName), !, rtrv_dm_handle_now(SName).
rtrv_dm_handle_now(Name):- discord_name_id_type(Name,ID,members),integer(ID),!,rtrv_dm_handle_now(ID).
rtrv_dm_handle_now(Name):- from_string(Name,ID),integer(ID),!,rtrv_dm_handle_now(ID).

get_prop(O,_,_):- \+ compound(O),!,fail.
% _{k:v,...}
get_prop(O,K,V):- is_dict(O),!,O.Key=V,atom_string(K,Key).
% [k:v,...]
get_prop(O,K,V):- is_list(O),!,member(KV,O),get_kvd(KV,K,V).
% json([k-v,...])
get_prop(json(O),K,V):- !, member(KV,O),get_kvd(KV,K,V).
% {k:v,...}
get_prop({(KV,Rest)},K,V):- !, (get_kvd(KV,K,V); get_prop({Rest},K,V)).
% {k:v}
get_prop({KV},K,V):- !, get_kvd(KV,K,V).
get_prop(KV,K,V):-  get_kvd(KV,K,V).

get_kvd(KV,K,V):- \+ compound(KV),!,fail,throw(var_get_kvd(KV,K,V)).
get_kvd(K:V,N,V):- !, atom_string(N,K).
get_kvd(K-V,N,V):- !, atom_string(N,K).
get_kvd(K=V,N,V):- !, atom_string(N,K).
%get_kvd(KV,N,V):- compound_name_arguments(KV,_,[K,V]),atom_string(N,K).

same_strings(K,N):- K=N,!.
same_strings(K,N):- atom_string(K,KS),atom_string(N,NS),KS=NS.

toplevel_prop :- \+ t_l:prop_prepend(_).

% curl -H "Authorization: Bot $AUTH_TOKEN" -H "User-Agent: DiscordBot" https://discord.com/api/channels/892809238710222930/messages

from_dict(Dict,Var):- var(Var),!,Dict=Var.
from_dict(Dict,List):- is_list(List),!,maplist(from_dict(Dict),List).
from_dict(Dict,json(KV)):- !,from_dict(Dict,KV).
from_dict(Dict,{KV,List}):- !,from_dict(Dict,KV),from_dict(Dict,{List}).
from_dict(Dict,{KV}):- !,from_dict(Dict,KV).
from_dict(Dict,KV):- get_kvd(KV,K,V),get_prop(Dict,K,DV),from_dict(DV,V).

add_id_type(_,Type):- number(Type),!.
add_id_type(ID,Type):- 
  discord_set_info(ID,instanceOf,Type),
  discord_set_info(ID,id,ID).

discord_add("TYPING_START",Dict):- from_dict(Dict,[channel_id:ID,user_id:User,timestamp:Time]),!, 
  nop(discord_addd(ID,event,at(typing_start(User),Time))).

discord_add(guild_member,Data):- toplevel_prop, 
  Data.user.id = ID, 
  add_id_type(ID,members),
  discord_add(ID,Data),!.
discord_add(member,Data):- toplevel_prop,
  Data.user.id = ID, 
  add_id_type(ID,memberz),
  discord_add(ID,Data),!.

%discord_add(Prop,List):- discord_grouping(Prop), is_list(List),!, maplist(discord_add(Prop),List).
discord_add(Type,Pairs):- 
  toplevel_prop,
  is_list(Pairs),select(KV,Pairs,Rest),
  once(get_kvd(KV,id,ID);get_kvd(KV,"id",ID)),
  once((add_id_type(ID,Type),discord_add(ID,Rest))),!.  

discord_add(Type,Pairs):- toplevel_prop, is_list(Pairs), Pairs\==[], !, maplist(discord_add(Type),Pairs),!.

discord_add(Type,KV):- get_kvd(KV,K,V), !,discord_addd(Type,K,V),!.
discord_add(Type,Data):- toplevel_prop, is_dict(Data),dict_pairs(Data,_Tag,Pairs),!,discord_add(Type,Pairs),!.
discord_add(CREATE,Data):- toplevel_prop, string_appended('_CREATE',CREATE,TypeS),default_guild(ID),discord_addd(ID,TypeS,Data),!.
discord_add(UPDATE,Data):- toplevel_prop, string_appended('_UPDATE',UPDATE,TypeS),default_guild(ID),discord_addd(ID,TypeS,Data),!.

%discord_add("GUILD_CREATE",{Data,Rest}):- !, discord_add("GUILD_CREATE",Data),discord_add("GUILD_CREATE",{Rest}),!.
%discord_add("GUILD_CREATE",{Data}):- !, discord_add("GUILD_CREATE",Data).


%discord_add(Type,{Data,Rest}):- !, discord_add(Type,Data),discord_add(Type,{Rest}).
discord_add(Type,Data):- %retractall(tmp:discord_info(Type,_,_)),
  discord_addd(Type,hasValue,Data),!.

string_appended(Appended,STRING_CREATE,STRINGS):- 
  string(STRING_CREATE),atom_concat(STRING,Appended,STRING_CREATE),
  downcase_atom(STRING,STRINGD),atom_concat(STRINGD,'s',STRINGS),!.

%int_to_date(S,V):- stamp_date_time(S,Date,local),!, format_time(string(V),'%a, %d %b %Y %T PST',Date,posix).

int_to_date(S,V):- stamp_date_time(S,Date,local),!, format_time(string(V),':%Y:%b:%d:%T',Date,posix).

name_type_string(Name,Type,S):- atom_concat(Type2,'s',Type),!,name_type_string(Name,Type2,S).
name_type_string(Name,Type,S):- atom_concat('@',Name2,Name),!,name_type_string(Name2,Type,S).
name_type_string(Name,Type,S):- atom_concat('#',Name2,Name),!,name_type_string(Name2,Type,S).
name_type_string(Name,member,S):- format(atom(S),'@~w',[Name]),!.
name_type_string(Name,role,S):- format(atom(S),'@~w',[Name]),!.
name_type_string(Name,channel,S):- format(atom(S),'#~w',[Name]),!.
name_type_string(Name,Type,S):- format(atom(S),'~w:~w',[Type,Name]).
%753344235805343785
int_to_name(S,V):- S>1420070400,get_time(T),TT is T + 6000,TT>S,int_to_date(S,VV),format(atom(V),'~w',[VV]).
%int_to_name(S,V):- discord_ddd(S,name,VV),!,sformat(V,'~w{~w}',[VV,S]).
int_to_name(S,V):- discord_name_id_type(Name,S,Type),!,name_type_string(Name,Type,VVV),
   %format(atom(V),'<~w/~w>',[VVV,S]).
   format(atom(V),'~w',[VVV]).
int_to_name(S,V):- discord_ddd(S,name,VV),!,format(atom(V),'name(~q)',[VV]).
int_to_name(S,V):- discord_ddd(S,username,VV),!,format(atom(V),'username(~q)',[VV]).
int_to_name(S,V):- discord_ddd(S,user_username,VV),!,format(atom(V),'user_username(~q)',[VV]).
%int_to_name(S,V):- discord_ddd(Vc,ontent,S),!.
%int_to_name(S,V):- % S> 4194304,  S > 1178066625, id_to_time(S,T),int_to_date(T,TT),sformat(V,'{~w?~w}',[S,TT]).

id_to_name(S,V):- discord_ddd(S,name,V).
id_to_name(S,V):- discord_ddd(S,username,V).
id_to_name(S,V):- discord_name_id_type(V,S,_Type).

discord_debug_cvt(T,T):- var(T),!.
discord_debug_cvt(tmp:T,S):- nonvar(T), !,discord_debug_cvt(T,S).
discord_debug_cvt(discord_info(A,B,C),discord_info(S,id,C)):- B==id, !, discord_debug_cvt(A,S).
discord_debug_cvt(T,S):- compound(T), compound_name_arguments(T,F,As),maplist(discord_debug_cvt,As,AAs),!,compound_name_arguments(S,F,AAs).
discord_debug_cvt(T,S):- number(T), !, int_to_name(T,S)-> true ; T=S.
discord_debug_cvt(T,S):- (atom(T);string(T)),discord_is_secret(T),!,(discord_ddd(V,hasValue,T)-> S= '$'(V) ; S="nZC-SECRET").
%discord_debug_cvt(T,S):- string(T), notrace_catch(atom_number(T,N)), !, discord_debug_cvt(N,S).
discord_debug_cvt(S,S).

discord_is_secret(R):- compound(R),!, functor(R,_,A),arg(A,R,E),discord_is_secret(E).
discord_is_secret(T):- (atom(T);string(T)),!,atom_contains(T,"Nzc").

from_string(S,V):- compound(S), compound_name_arguments(S,F,As),maplist(from_string,As,AAs),!,compound_name_arguments(V,F,AAs).
from_string(S,V):- \+ string(S), \+ atom(S), !, V=S.
from_string(S,V):- atom_length(S,L),L>40,!,S=V.
from_string(S,V):- \+ atom(S), text_to_string(S,SS),string_to_atom(SS,A),notrace_catch(atom_number(A,V)),!.
from_string(S,V):- parse_time(S,_,V),!.
from_string(V,V).

id_to_time(ID,UTC):- integer(ID), UTC is (( ID >> 22) / 1000) + 1420070400.

has_id(H):- get_prop(H,id,V),from_string(V,N),!,integer(N).

:- thread_local( t_l:prop_prepend/1).

discord_addd(reconnect,op,7):- discord_reconnect.

discord_addd(ID,Prop,Data):- from_string(ID,ID2),ID\==ID2,!,discord_addd(ID2,Prop,Data).
discord_addd(ID,Prop,Data):- from_string(Data,Data2),Data\==Data2,!,discord_addd(ID,Prop,Data2).
discord_addd(Guild,presences,[H|List]):- default_guild(Guild), maplist(discord_addd(presence,hasMember),[H|List]),!.

discord_addd(ID,mentions,List):- !, discord_addd(ID,recipients,List).

discord_addd(ID,recipients,List):- is_list(List),maplist(discord_addd(ID,recipient),List),!.
discord_addd(ID,recipient,Dict):- is_dict(Dict),del_dict(id,Dict,UserID,Rest),!,discord_add(UserID,Rest),
   discord_addd(ID,recipient,UserID).

discord_addd(ID,recipient,UserID):- integer(UserID),
  forall(id_to_name(UserID,Name),discord_addd(ID,recipient_name,Name)),
  fail.

discord_addd(ID,attachments,List):- is_list(List),maplist(discord_addd(ID,attachment),List).
discord_addd(ID,attachment_url,URL):- notrace_catch(http_open:http_get(URL,Data,[])), 
  once(discord_addd(ID,attached_content,Data)),fail.

discord_addd(ID,content,Data):- Data \== "",
  add_discord_chat_event(ID,content(Data)),fail.

discord_addd(ID,embeds,Data):- Data \== [],
  add_discord_chat_event(ID,embeds(Data)),fail.

discord_addd(ID,attached_content,Data):- Data \== "",
  add_discord_chat_event(ID,content(Data)),fail.


discord_addd(ID,user,Data):- 
  is_dict(Data),
  once(discord_add(ID,Data)),fail.

discord_addd(MID,channel_id,CID):-
  add_id_type(CID,channels),
  add_id_type(MID,messages),
  discord_ddd(MID,author_username,Who),
  without_ddbg(discord_addd(CID,user_seen,Who)),
  fail.

discord_addd(_CID,last_message_id,MID):- MID\==null, add_id_type(MID,messages),fail.


discord_addd(Guild,hasValue,List):- default_guild(Guild),is_list(List),!, maplist(discord_add(Guild),List).


discord_addd(presence,hasMember,Dict):- Dict.user.id = ID,!,
  without_ddbg(discord_add(ID,Dict)).

discord_addd(ID,referenced_message,Dict):- is_dict(Dict), Dict.id = MID, !,
  discord_addd(ID,referenced_message,MID),
  discord_add(MID, Dict).



discord_addd(Guild,Prop,[H|List]):- default_guild(Guild), % discord_grouping(Prop), 
  is_list(List),
  \+ \+ has_id(H),!,
  maplist(discord_add(Prop),[H|List]),!.

discord_addd(Guild,members,[H|List]):- default_guild(Guild), % discord_grouping(Prop), 
  is_list(List), maplist(discord_add(guild_member),[H|List]),!.

  

discord_addd(_,op,10):- !, discord_identify.

 
 

discord_addd(ID,Prop,Dict):- atom(Prop), once(t_l:prop_prepend(Pre)), Pre\=='',
 atomic_list_concat([Pre,Prop],'_',PreProp),!,
 locally(t_l:prop_prepend(''), discord_addd(ID,PreProp,Dict)).

discord_addd(ID,Author,Dict):- maybe_prepend(Author), integer(ID), toplevel_prop, is_dict(Dict),!,
 dict_pairs(Dict,_,Pairs),
 locally(t_l:prop_prepend(Author),maplist(discord_add(ID),Pairs)).

%discord_addd(ID,user_username,Dict):- !, discord_set_info(ID,name,Dict).
%discord_addd(ID,username,Dict):- !, discord_set_info(ID,name,Dict).
discord_addd(ID,Prop,Dict):- discord_set_info(ID,Prop,Dict),!.

discord_set_info(ID,Prop,Data):- 
 once(Prop==hasValue ; (number(Data),get_time(Time),Data<Time)),
 \+ tmp:discord_info(ID,Prop,Data),
 retractall(tmp:discord_info(ID,Prop,_)),fail.
discord_set_info(_,Prop,Data):- default_info(Prop,Data),!.
discord_set_info(ID,Prop,Data):-
  TmpR=tmp:R,
  R= discord_info(ID,Prop,Data),
  (\+ \+ call(TmpR) -> (retract(TmpR),asserta(TmpR),nop(ddbg(discord_keep_info(ID,Prop,Data)))) 
   ; (asserta(TmpR),on_new_ddd(ID,Prop,Data))).

on_new_ddd(ID,Prop,Data):-
  ddbg(discord_addd(ID,Prop,Data)),
  ignore((Data==threads->discord_join_subchannel(ID))).

maybe_prepend(author).
maybe_prepend(user).
maybe_prepend(recipients).
maybe_prepend(referenced_message):-!,fail.
maybe_prepend(message_reference).
%maybe_prepend(Atom):- atom(Atom).

get_discord(ID,Data):- discord_ddd(ID,hasValue,Data).
get_discord(ID,Prop,Value):- discord_ddd(ID,Prop,Value)*->true;get_discord2(ID,Prop,Value).

get_discord2(Type,Prop,Value):- discord_ddd(Type,hasValue,ID),discord_ddd(ID,Prop,Value).

get_discord_info(ID,Prop,Data):- discord_ddd(ID,Prop,Data)*-> true;
  (\+ integer(ID), \+ var(ID), any_to_id(ID,ID2),!, discord_ddd(ID2,Prop,Data)).

%check_unifier(U1,U2):- nonvar(U1),var(U2),!,freeze(U2,check_unifier_final(U1,U2)).
%check_unifier(U2,U1):- nonvar(U1),var(U2),!,freeze(U2,check_unifier_final(U1,U2)).
%check_unifier(U2,U1):- var(U1),var(U2),!,freeze(U2,check_unifier_final(U1,U2)). 
check_unifier(U1,U2):- var(U1),!,check_unifier_var_1(U1,U2).
check_unifier(U2,U1):- var(U1),!,check_unifier_var_1(U1,U2).
check_unifier(U1,U2):- check_unifier_final(U1,U2).

check_unifier_var_1(U1,U2):- nonvar(U2),!,U1=U2.
check_unifier_var_1(U1,U2):- \+ frozen(U2,_), !, freeze(U2,check_unifier(U1,U2)).
check_unifier_var_1(U1,U2):- U1=U2.


check_unifier_final(U1,U2):- any_to_each(U1,ID1),any_to_each(U2,ID2),ID1==ID2,!.

any_to_each(U1,U1).
any_to_each(U1,Each):- nonvar(U1), any_to_each_1(U1,Each), U1\==Each.

any_to_each_1(U1,Each):- maybe_into_string(U1,Each).
any_to_each_1(U1,Each):- term_to_atom(U1,Each). 
any_to_each_1(U1,Each):- from_string(U1,Each), \+ integer(Each).
any_to_each_1(U1,Each):- any_to_id(U1,Each).

maybe_into_string(Name,NameS):- nonvar(Name), \+ string(Name), notrace_catch(text_to_string(Name,NameS)), !.

discord_name_id_type(Name,ID,Type):- maybe_into_string(Name,NameS), !, discord_name_id_type(NameS,ID,Type).
discord_name_id_type(Name,ID,Type):- var(Name), nonvar(Type),  !,  discord_ddd(ID,instanceOf,Type), discord_ddd(ID,name,Name).
discord_name_id_type(Name,ID,Type):- discord_ddd(ID,name,Name), integer(ID), once(discord_id_type(ID,Type)).
discord_name_id_type(Name,ID,Type):- discord_ddd(ID,username,Name), integer(ID), once(discord_id_type(ID,Type)).

discord_id_type(ID,Type):- discord_ddd(ID,instanceOf,Type).

discord_ddd(guilds, id, X):- default_guild(X).
discord_ddd(A,B,C):- tmp:discord_info(A,B,C).
discord_ddd(A,B,C):- integer(A), B == timestamp, !, id_to_time(A,C).
  %get_discord(Type,hasValue,ID),
  %get_discord(ID,id,ID),
  %true.872902388623757364


any_to_id(Name,ID):-var(Name),!, fail,ID=Name.
any_to_id(Name,ID):-integer(Name),!,ID=Name.
any_to_id(Name,ID):- maybe_into_string(Name,SName), !, any_to_id(SName,ID).
any_to_id(Name,ID):- discord_name_id_type(Name,ID,_),integer(ID),!.
any_to_id(Name,ID):- from_string(Name,ID),integer(ID),!.
%any_to_id(Name,ID):-text_to_string(Name,NameS),get_discord(_,hasValue,ID), get_discord(ID,_,NameS),!.

same_ids(ID,IDS):-any_to_id(ID,IDA),any_to_id(IDS,IDB),IDA==IDB.

discord_ensure_im2(ID,IM):- get_discord(IM,user,ID),!.

discord_ensure_im(To,IM):- get_discord(IM, name, To), get_discord(IM, is_channel, true),!.
discord_ensure_im(To,IM):- any_to_id(To,ID),!, discord_ensure_im(ID,IM).
discord_ensure_im(To,IM):- discord_ensure_im2(To,IM),!.
% OLD discord_ensure_im(To,IM):- any_to_id(To,ID), discord_send({type:'im_open',user:ID}),!,must(discord_ensure_im2(To,IM)),!.
discord_ensure_im(To,IM):- discord_send({type:'conversations_open',users:To}),!,must(discord_ensure_im2(To,IM)),!.


discord_id_time(ID,TS):-flag(discord_id,OID,OID+1),ID is OID+1,get_time(Time),number_string(Time,TS).


discord_me(Self):-get_discord('@me', id, Self).

matches_info(S,(A;B)):-!, matches_info(S,A);matches_info(S,B).
matches_info(S,(A,B)):-!, matches_info(S,A),matches_info(S,B).
matches_info(S,Str):- sub_string(S, _Offset0, _Length, _After, Str).


as_msg_string(call(Msg),Str):- wots(SF,call(Msg)),!,as_msg_string(SF,Str).
as_msg_string(Msg,Str):- compound(Msg),wots(SF,print_tree(Msg)),!,as_msg_string(SF,Str).
as_msg_string(Msg,Str):- string(Msg),!,Msg=Str.
as_msg_string(Msg,Str):- any_to_string(Msg,SF),as_msg_string(SF,Str).


backquote_multiline_string(Msg,Str):- \+ atom_contains(Msg,'```'), \+ atom_contains(Msg,'||'),  atom_contains(Msg,'\n'), sformat(Str,'```~w```',[Msg]),!.
backquote_multiline_string(Msg,Msg).

%discord_say:- discord_say(_,'From Prolog').
% {"id":3,"type":"message","channel":"D3U47CE4W","text":"hi there"}

/*
discord_say(ID,Str):- atom_length(Str,L),L>20,!,
  sub_string(Str,0,100,_,Sub),
  replace_in_string(['"'='\\"'],Sub,SubR),
  sformat(S,'--boundary
Content-Disposition: form-data; name="payload_json"
Content-Type: application/json
{ "content": "~w" }
--boundary
Content-Disposition: form-data; name="file"; filename="discord_say.txt"
Content-Type:application/octet-stream
~w
--boundary--
',[SubR,Str]),
   replace_in_string(['\n'='\r\n"'],Sub,SubR),
    discord_http(channels/ID/messages,[post(S)]).
*/

any_to_chan_id(Channel,ID):- atom(Channel),!,atom_string(Channel,Name),!,any_to_chan_id(Name,ID).
any_to_chan_id(Channel,ID):- (var(Channel)->!;true), discord_name_id_type(Channel,ID,channels).
any_to_chan_id(Channel,ID):- string(Channel),atom_number(Channel,Name),!,any_to_chan_id(Name,ID).
any_to_chan_id(Channel,ID):- string(Channel),string_concat('#',Name,Channel),!,any_to_chan_id(Name,ID).
any_to_chan_id(Channel,ID):- string(Channel),string_concat('@',Name,Channel),!,any_to_chan_id(Name,ID).
any_to_chan_id(Channel,ID):- integer(Channel),discord_name_id_type(Name,Channel, members),!,any_to_chan_id(Name,ID).
any_to_chan_id(Channel,ID):- integer(Channel),discord_id_type(Channel,messages),discord_ddd(Channel,channel_id,ID).
any_to_chan_id(Channel,ID):- integer(Channel),discord_id_type(Channel,threads),!,Channel=ID.
any_to_chan_id(Channel,ID):- integer(Channel),discord_id_type(Channel,channels),!,Channel=ID.
any_to_chan_id(Channel,ID):- integer(Channel),!,Channel=ID. % Cache miss
% @TODO locate DMs
any_to_chan_id(Channel,ID):- discord_ddd(ID,recipient_name,Channel),!.
any_to_chan_id(Channel, _):- rtrv_dm_handle(Channel),fail.
any_to_chan_id(Channel,ID):- discord_ddd(ID,recipient_name,Channel),!.
%any_to_chan_id("prologmud_bot_testing",892806433970716692).
any_to_chan_id(Channel,ID):- any_to_id(Channel,ID),!.

use_file_upload(Str):- atom_length(Str,L),L>1500,!.
use_file_upload(Str):- atomic_list_concat(Lines,'\n',Str),!,length(Lines,L), L>7. 


is_guild_chan(ID):- (discord_ddd(ID,parent_id,PID),PID\==null,PID>0).

discord_say(X):- discord_say('dmiles',X),discord_say('prologmud_bot_testing',X).
% https://discord.com/oauth2/authorize?client_id=772113231757574185&scope=bot&permissions=268823638


discord_say(Channel,Msg):- atom_or_string(Channel),atom_number(Channel,ID),!,discord_say(ID,Msg).
discord_say(Channel,Msg):- \+ integer(Channel),any_to_chan_id(Channel,ID),!,discord_say(ID,Msg).
discord_say(MID,Msg):- discord_name_id_type(Name,MID,members),any_to_chan_id(Name,ID),!,discord_say_text(ID,Msg,[]).
discord_say(MID,Msg):- \+ discord_id_type(MID,channels), discord_ddd(MID,channel_id,ID), 
   %(is_guild_chan(ID) -> default_guild(GID); GID=0),!,
    discord_say_text(ID,Msg,_{message_reference:_{message_id: MID}}),!.
discord_say(ID,Msg):- discord_say_text(ID,Msg,[]),!.

add_to_dict(Dict,[],Dict):-!.
add_to_dict(Dict,[Add|Mentions],NewDict):-
  add_to_dict(Dict,Add,MDict),
  add_to_dict(MDict,Mentions,NewDict),!.
add_to_dict(Dict,ADict,NewDict):- is_dict(ADict),dict_pairs(ADict,_,List),!,add_to_dict(Dict,List,NewDict).
add_to_dict(Dict,KV,NewDict):- get_kvd(KV,K,V), get_dict(K,Dict,Old),is_dict(Old),add_to_dict(Old,V,New),
  put_dict(K,Dict,New,NewDict).
add_to_dict(Dict,KV,NewDict):- get_kvd(KV,K,V),put_dict(K,Dict,V,NewDict).
 


discord_say_text(Channel,Msg, AddMentions):- use_file_upload(Msg),
 add_to_dict(_{content: ""},AddMentions,NewDict),
 tmp_file('discord_say',Tmp), atom_concat(Tmp,'.txt',File),
 setup_call_cleanup(
   setup_call_cleanup(open(File,write,O),write(O,Msg),close(O)),
   discord_say_file(Channel,File,NewDict),
   delete_file(File)),!.

discord_say_text(Channel,Msg, AddMentions):-  backquote_multiline_string(Msg,Str), !,
   add_to_dict(_{content: Str},AddMentions,NewDict),
   discord_http(channels/Channel/messages,[post(json(NewDict))]),!.

json_to_string(JSON,Str):- wots(Str,json_write(current_output,JSON,[])).

atom_or_string(S):- atom(S);string(S).

% todoo fix this!
discord_say_file(ID,File,JSON):- fail,
  discord_http(channels/ID/messages,[
       post([ payload_json =  json(JSON),
              filename     =  file(File)])]),!.
discord_say_file(Channel,File,JSON):-  
 bot_discord_token(Token),
 json_to_string(JSON,Str),
 sformat(S,
   "curl -H 'Authorization: ~w' -H 'User-Agent: DiscordBot' -F 'payload_json=~w' -F 'filename=@~w' ~w/channels/~w/messages",
   [Token,Str,File,"https://discord.com/api/v9", Channel]), catch(shell(S),E,(wdmsg(E=S),fail)),!.
/*
   @TODO remove the shell/1 above and do something like..  

    discord_http(channels/ID/messages,[
       post([ payload_json =  json(_{content: ""}),
              filename     =  file(File)])]).

  (crazy we have no examples anywhere of file upload?!)

*/




/*
  Dict=
    _{content: StrO,
      
    %embeds: [_{ title: "Hello, Embed!", description: "This is an embedded message."},
    tts: false},
 %sformat(S,'~q',[Dict]),
 %ddbg_always(post=S),
 discord_http(channels/ID/messages,[post(json(Dict))]),!.
*/

dict_append_curls(Dict,Params,NewDict):-any_to_curls(Params,Curly), dict_append_curls3(Dict,Curly,NewDict).

dict_append_curls3(Dict,{},Dict):-!.
dict_append_curls3(Dict,{Curly},NewDict):-!,dict_append_curls3(Dict,Curly,NewDict).
dict_append_curls3(Dict,(A,B),NewDict):-!,dict_append_curls3(Dict,A,NewDictM),dict_append_curls3(NewDictM,B,NewDict).
dict_append_curls3(Dict,KS:V,NewDict):- string_to_atom(KS,K), put_dict(K,Dict,V,NewDict).


bot_discord_token(TokenHeader):- tmp:discord_token(Token),sformat(TokenHeader,"Bot ~w",[Token]).


type_to_url("message",'chat.postMessage').
type_to_url("im_open",'im.open').
type_to_url("conversations_open",'conversations.open').
type_to_url(X,X):-!.

make_url_params({In},Out):-!,make_url_params(In,Out).
make_url_params((A,B),Out):-!,make_url_params(A,AA),make_url_params(B,BB),format(atom(Out),'~w&~w',[AA,BB]).
make_url_params([A|B],Out):-!,make_url_params(A,AA),make_url_params(B,BB),format(atom(Out),'~w&~w',[AA,BB]).
make_url_params([A],Out):-!,make_url_params(A,Out).
make_url_params(KV,Out):-get_kvd(KV,K,A),www_form_encode(A,AA),format(atom(Out),'~w=~w',[K,AA]).


discord_send(DataI):- any_to_curls(DataI,Data),discord_send000(Data).
%discord_send_receive(DataI,Recv):- any_to_curls(DataI,Data),discord_send_receive000(Data,Recv).

% @TODO comment the above and fix this next block
discord_send000(Data):-  discord_send_ws(_WebSocket,Data),!.


discord_send_ws(WebSocket,Data):- 
  with_output_to(atom(S),writeq(Data)), tmp:jpl_websocket(WebSocket),!,jpl_call(WebSocket,send_message,[S],_),!. 
discord_send_ws(WebSocket,Data):- 
  tmp:discord_websocket(WebSocket, _WsInput, WsOutput), !,  
 must_det_l((flush_output(WsOutput),
  wdmsg(ws_send(WsOutput,text(Data))),
  ws_send(WsOutput,text(Data)),!,flush_output(WsOutput))),!.

discord_send_ws(WebSocket,Data):- 
  tmp:discord_websocket(WebSocket, _WsInput, WsOutput), !,  
 must_det_l((flush_output(WsOutput), any_to_json_dict(Data,Dict),
  wdmsg(ws_send(WsOutput,json(Dict))),
  ws_send(WsOutput,json(Dict)),!,flush_output(WsOutput))),!.
%discord_send_ws(WsOutput,Data):- is_stream(WsOutput), format(WsOutput,'~q',[Data]),flush_output(WsOutput),ddbg_always(discord_sent(Data)),flush_output.

any_to_json_dict(D,D):- is_dict(D,_),!. 
any_to_json_dict(List,Dict):- is_list(List),dict_create(Dict,_,List),!.

any_to_json_dict({C},D):-  notrace_catch((sformat(S,'~q',[{C}]),atom_json_dict(S,D,[as(string)]))),!.
any_to_json_dict({C},D):- !, conjuncts_to_list(C,L),maplist(any_to_json_dict_arg,L,L2),dict_create(D,_,L2),!.

any_to_json_dict(S,V):- compound(S), compound_name_arguments(S,F,As),maplist(any_to_json_dict,As,AAs),!,compound_name_arguments(V,F,AAs).
any_to_json_dict(String,D):- notrace_catch(atom_json_dict(String,D,[as(string)])),!.
%any_to_json_dict(S,V):- number(S), int_to_name(S,V),!.
any_to_json_dict(V,V).

any_to_json_dict_arg(S:V,SS-VV):- any_to_atom(S,SS),any_to_json_dict(V,VV).
%dict_to_curly(Dict,{type:Type,Data}):- del_dict(type,Dict,Type,DictOut),dict_pairs(DictOut,_,Pairs),any_to_curls(Pairs,Data).
%dict_to_curly(Dict,{type:Type,Data}):- dict_pairs(Dict,Type,Pairs),nonvar(Type),any_to_curls(Pairs,Data).
dict_to_curly(Dict,{Data}):- dict_pairs(Dict,_,Pairs),list_to_curls(Pairs,Data).

list_to_curls([A],AA):-!,any_to_curls(A,AA).
list_to_curls([A|B],(AA,BB)):-!,any_to_curls(A,AA),list_to_curls(B,BB).

any_to_curls(Dict,Data):- is_dict(Dict),!,dict_to_curly(Dict,Data).
any_to_curls(Var,"var"):- \+ must(\+ var(Var)),!.
any_to_curls(null,null).
any_to_curls(true,true).
any_to_curls(false,false).
any_to_curls(KV,AA:BB):-get_kvd(KV,A,B),!,any_to_curls(A,AA),any_to_curls(B,BB).
any_to_curls('$'(Var),ValO):- get_discord(Var,Val),!,any_to_curls(Val,ValO).
any_to_curls({DataI},{Data}):-!,any_to_curls(DataI,Data).
any_to_curls((A,B),(AA,BB)):-!,any_to_curls(A,AA),any_to_curls(B,BB).
any_to_curls(A,O):- string(A),!,from_string(A,O).
any_to_curls(A,O):- maybe_into_string(A,AA),!,any_to_curls(AA,O).
any_to_curls(A,O):- from_string(A,O).

%gw_op(0,'dispatch','receive','an event was dispatched.').
%gw_op(1,'heartbeat',_,'fired periodically by the client to keep the connection alive.').
gw_op(2,'identify','send','starts a new session during the initial handshake.').
gw_op(3,'presence update','send','update the client''s presence.').
gw_op(4,'voice state update','send','used to join/leave or move between voice channels.').
gw_op(6,'resume','send','resume a previous session that was disconnected.').
gw_op(7,'reconnect','receive','you should attempt to reconnect and resume immediately.').
gw_op(8,'request guild members','send','request information about offline guild members in a large guild.').
gw_op(9,'invalid session','receive','the session has been invalidated. you should reconnect and identify/resume accordingly.').
gw_op(10,'hello','receive','sent immediately after connecting and contains the heartbeat_interval to use.').
gw_op(11,heartbeat_ack(11),'receive','sent in response to receiving a heartbeat to acknowledge that it has been received.').

get_emojis:- discord_http(guilds/{guilds-id}/emojis).

default_guild(748871194572226661).
default_guild("GUILD_CREATE").

no_default_info(topic). no_default_info(position). no_default_info(parent_id). % no_default_info(s).
no_default_info(id). no_default_info(instanceOf). no_default_info(type). no_default_info(hasValue).

default_info(hasValue,[]).
default_info(X,_):- nonvar(X), no_default_info(X),!,fail.
default_info(flags,0). % probably this doesnt belong
default_info(accent_color,null). default_info(attachments,[]). default_info(avatar,null).
default_info(banner,null). default_info(banner_color,null). default_info(components,[]).
default_info(edited_timestamp,null). default_info(embeds,[]). 
default_info(guild_id,GuildID):- default_guild(GuildID).
default_info(last_message_id,null). default_info(mention_everyone,false). default_info(mention_roles,[]).
default_info(mentions,[]). default_info(nsfw,false). default_info(permission_overwrites,[]).
default_info(pinned,false). default_info(rate_limit_per_user,0). default_info(rtc_region,null).
default_info(tts,false). default_info(type,0). default_info(user_limit,0). default_info(public_flags,0).
default_info(email,null). default_info(features,[]). default_info(messages,[]). default_info(owner,false).
default_info(X,Y):- default_info_value(Y),!, nop(dmsg(default_info(X,Y))).

default_info_value(null).
default_info_value(0).
default_info_value([]).
default_info_value(false).

%:- autoload_all.
discord_restore_1:- 
  discord_add(s,null),
  bot_discord_token(TokenHeaderB), discord_add(bot_token,TokenHeaderB),
  discord_token_string(TokenHeader), discord_add(token,TokenHeader),
  get_time(Time), ITime is integer(Time), discord_add(time,ITime).

:- if( \+ prolog_load_context(reloading, true)).
:- discord_restore_1.
:- endif.
:- initialization(discord_restore_1,program).

discord_say :- discord_say('prologmud_bot_testing',"test message to prologmud_bot_testing").
discord_say0:- 
 discord_say( asserted( exists( Y2,
                                   ( info( 'XVAR_NP_X_1_1', [
                                       loc(1), pos('NP'),equals('XVAR_NP_X_1_1'),
                                       words([w(x,[alt(pos(nn)),root(x),pos(nnp),loc(1),lnks(2),txt("X"),truecase('LOWER'),link(1,'NP',r('NP',seg(1,1))),link(2,'S',r('S',seg(1,3))),lex_winfo])]), seg(1,1),phrase('NP'),size(1),lnks(1),
                                       #(r('NP',seg(1,1))),txt(["X"]),childs(0),
                                       link(1,'S',r('S',seg(1,3)))])  ,
                                     span( [ seg(1,3), phrase('S'),size(3),lnks(0),#(r('S',seg(1,3))),
                                             txt(["X","is","Y"]),childs(2),
                                             child(1,'NP',r('NP',seg(1,1))),
                                             child(2,'VP',r('VP',seg(2,3)))]) ,
                                     span( [ seg(2,3), phrase('VP'),size(2),lnks(1),
                                             #(r('VP',seg(2,3))),txt(["is","Y"]),
                                             childs(1),child(1,'VP',r('VP',seg(3,3))),
                                             link(1,'S',r('S',seg(1,3)))]) ,
                                     span( [ seg(3,3), phrase('VP'),size(1),lnks(2),
                                             #(r('VP',seg(3,3))),txt(["Y"]),childs(0),
                                             link(1,'VP',r('VP',seg(2,3))),link(2,'S',r('S',seg(1,3)))]) ,
                                     iza(Y2,'Y') ,
                                     equalsVar(Y2,'XVAR_NP_X_1_1'))))).



discord_say1:- discord_say(call(ls)).
discord_say2:- forall(discord_say2(X),discord_say2a(X)).
discord_say3:- discord_say("|| || \n spoiler\n||line 1||\n||line 2||\nexcera").


discord_say2(232781767809957888). discord_say2('#prologmud_bot_testing'). discord_say2('dmiles'). discord_say2('@dmiles').
discord_say2("dmiles"). discord_say2("@dmiles"). discord_say2('prologmud_bot_testing').
discord_say2a(X):- sformat(S,"test message to ~q.",[X]), discord_say(X,S).


discord_restore_2:- discord_connect.
:- initialization(discord_restore_2).

:- if( \+ prolog_load_context(reloading, true)).
:- prolog_load_context(file,File), forall((source_file(PP,File),strip_module(PP,M,P),functor(P,F,0)),add_history(M:F)).
:- endif.


% start discord gateway in a thread
:- initialization(discord_start_gateway).
% start discord pinger in a thread
% :- deque_discord_events.
% start discord pinger in a thread
%:- ping_discord.
% start discord message checker in a thread 
:- initialization(discord_proc_tasks).
/*
:- discord_message_checking_01.
:- discord_message_checking_02.
*/

:- fixup_exports.

:- eggdrop:import(discord_client:discord_chat_override/1).



