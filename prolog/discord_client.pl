:- module(discord_client, [
        discord_start_gateway/0,
        discord_say/2,
        discord_send/1,
        ping_discord/0,
        discord_get_websocket/1,
        is_thread_running/1,        
        discord_restore_0/0,
        any_to_id/2]).

/** <module> discord_client - Provides a websocket API to write discord clients

*/
:- meta_predicate(notrace_catch(:)).
notrace_catch(G):- notrace(catch(G,_,fail)).
debug_console(G):- (tmp:discord_debug(X);X=user_error),!, with_output_to(X,G).

:- volatile(tmp:discord_debug/1).
:-  dynamic(tmp:discord_debug/1).
if_debug(_).

:- meta_predicate(disco_call(:)).
disco_call(G):- wots(S,G),discord_say(S).

disable_gateway:- false.  % true until we fix our websocket code

discord_restore_0:- tmp:discord_debug(_),!.
discord_restore_0:- 
  stream_property(X,file_no(2)),asserta(tmp:discord_debug(X)),
  notrace_catch(mutex_create(last_done_mutex)),
  notrace_catch(mutex_create(connection_mutex)).

:- initialization(discord_restore_0).

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

% ===============================================
% Utility functions
% ===============================================

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

rtrv_message(MID):- discord_ddd(MID,content,_),!.
% rtrv_message(MID):- discord_ddd(MID,seen,true),!.
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

how_long_ago(Task,Ago):- nonvar(Ago),!,how_long_ago(Task,TryAgo),!,Ago=TryAgo.
how_long_ago(Task,Ago):- 
  with_mutex(last_done_mutex,tmp:last_done(Task,Time);Ago=never),!,
  ignore((number(Time),get_time(Now),Ago is Now - Time)),!.

remember_task(Task):- with_mutex(last_done_mutex, 
  ignore((retractall(tmp:last_done(Task,_)),get_time(Time),asserta(tmp:last_done(Task,Time))))).

%how_often(200, guilds).
%how_often(30, channels).

how_often(5, handle_discord_websocket_events).
%how_often(41, send_ping).
%how_often(2, handle_chat_events).
how_often(once, discord_update(channels)).
%how_often(300, rtrv_new_messages).
/*
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


dont_reply_user("PrologMUD").
dont_reply_user("irc0").
dont_reply_user("LM489").
dont_reply_user("jllykifsh").

add_discord_chat_event(ID,Message):- assertz(tmp:discord_chat_event(ID,Message)).

handle_discord_chat_event(ID,_Message):- discord_ddd(_,referenced_message,ID),!.
handle_discord_chat_event(ID,_Message):- discord_ddd(ID,referenced_message,_),!.
handle_discord_chat_event(ID,content(Message)):-  
  discord_ddd(ID,author_username,User), !, 
  handle_discord_chat_event(ID,say(User,Message)).

handle_discord_chat_event(ID,Message):- remember_task(handle_chat_events), dmsg(chat_event(ID,Message)),fail.
handle_discord_chat_event(ID, _):- \+ number(ID),!.
handle_discord_chat_event(_ID,say(User,_Message)):- dont_reply_user(User),!.
handle_discord_chat_event(ID,say(User,Message)):- !,
 trim_message(Message,Str), 
 discord_set_typing(ID),
 locally(t_l:discord_channel(ID),
  without_ddbg(external_chat_event(discord_client:discord_chat_override,ID,User,say(Str)))),!,
  flush_channel_output_buffer(ID).
handle_discord_chat_event(ID,Message):- dmsg(failed_chat_event(ID,Message)).

trim_message(A,C):- \+ string(A),!,any_to_string(A,S),trim_message(S,C).
trim_message(A,C):- split_string(A, "", "`\s\t\n", [B]), A\==B,!,trim_message(B,C).
trim_message(A,A).

discord_set_typing(ID):- any_to_chan_id(ID,CID),ID\==CID,!,discord_set_typing(CID).
discord_set_typing(ID):- how_long_ago(discord_set_typing(ID),Ago),number(Ago), Ago < 5,!. % only sending typing idicator every 5 seconds
discord_set_typing(ID):- 
  remember_task(discord_set_typing(ID)),
  discord_http(channels/ID/typing,[method(post)]).


% ====================================
% Discord Task Calls
% ====================================

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



discord_id_time(ID,TS):-flag(discord_id,OID,OID+1),ID is OID+1,get_time(Time),number_string(Time,TS).


discord_me(Self):-get_discord('@me', id, Self).

matches_info(S,(A;B)):-!, matches_info(S,A);matches_info(S,B).
matches_info(S,(A,B)):-!, matches_info(S,A),matches_info(S,B).
matches_info(S,Str):- sub_string(S, _Offset0, _Length, _After, Str).


as_msg_string(call(Msg),Str):- wots(SF,call(Msg)),!,as_msg_string(SF,Str).
as_msg_string(Msg,Str):- compound(Msg),wots(SF,print_tree(Msg)),!,as_msg_string(SF,Str).
as_msg_string(Msg,Str):- string(Msg),!,Msg=Str.
as_msg_string(Msg,Str):- notrace_catch(text_to_string(Msg,SF)),!,as_msg_string(SF,Str).
as_msg_string(Msg,Str):- any_to_string(Msg,SF),!,as_msg_string(SF,Str).


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


dict_append_curls(Dict,Params,NewDict):-any_to_curls(Params,Curly), dict_append_curls3(Dict,Curly,NewDict).

dict_append_curls3(Dict,{},Dict):-!.
dict_append_curls3(Dict,{Curly},NewDict):-!,dict_append_curls3(Dict,Curly,NewDict).
dict_append_curls3(Dict,(A,B),NewDict):-!,dict_append_curls3(Dict,A,NewDictM),dict_append_curls3(NewDictM,B,NewDict).
dict_append_curls3(Dict,KS:V,NewDict):- string_to_atom(KS,K), put_dict(K,Dict,V,NewDict).


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

get_emojis:- discord_http(guilds/{guilds-id}/emojis).

default_guild(748871194572226661).
default_guild("GUILD_CREATE").

no_default_info(bot). 
no_default_info(topic). no_default_info(position). no_default_info(parent_id). % no_default_info(s).
no_default_info(id). no_default_info(instanceOf). no_default_info(type). no_default_info(hasValue).

%:- autoload_all.
discord_restore_1:-
 without_ddbg((
  bot_token_string(TokenHeaderB), discord_add(bot_token,TokenHeaderB),
  discord_token_string(TokenHeader), discord_add(token,TokenHeader),
  get_time(Time), ITime is integer(Time), discord_add(time,ITime))).

:- if( \+ prolog_load_context(reloading, true)).
:- discord_restore_1.
:- endif.
:- initialization(discord_restore_1).

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


end_of_file.


https://discord.com/api/oauth2/authorize?client_id=772113231757574185&scope=bot&permissions=1

Name=logicmoo-app
USERNAME=PrologMUD#4124
PERMISSION=536536415425
Public=63920eac424255a5a57fc6b48a7c958c46621ab18505506b1d3fa32f3708e638
Client=772113231757574185
Channel=892809238710222930
Token=


curl -v -H "Authorization: Bot $AUTH_TOK" -H "User-Agent: curl" -H "Content-Type: application/json" -XGET https://discord.com/api/v9/channels/892809238710222930/messages
curl -v -H "Authorization: $AUTH_TOKEN" -H "User-Agent: DiscordBot" -H "Content-Type: application/json" -H "Content-Length: 0" -X GET https://discord.com/api/channels/892809238710222930/messages

{
  "token_type": "Bearer",
  "access_token": "GNaVzEtATqdh173tNHEXY9ZYAuhiYxvy",
  "scope": "webhook.incoming",
  "expires_in": 604800,
  "refresh_token": "PvPL7ELyMDc1836457XCDh1Y8jPbRm",
  "webhook": {
    "application_id": "310954232226357250",
    "name": "testwebhook",
    "url": "https://discord.com/api/webhooks/347114750880120863/kKDdjXa1g9tKNs0-_yOwLyALC9gydEWP6gr9sHabuK1vuofjhQDDnlOclJeRIvYK-pj_",
    "channel_id": "345626669224982402",
    "token": "kKDdjXa1g9tKNs0-_yOwLyALC9gydEWP6gr9sHabuK1vuofjhQDDnlOclJeRIvYK-pj_",
    "type": 1,
    "avatar": null,
    "guild_id": "290926792226357250",
    "id": "347114750880120863"
  }
}

curl -v -H "Authorization: XXX" -H "User-Agent: curl" -H "Content-Type: application/json" -H "Content-Length: 0" -X GET https://discord.com/api/v9/channels/892809238710222930/messages


From this object, you


https://discord.com/api/oauth2/authorize?response_type=code&client_id=772113231757574185&scope=webhook.incoming&state=15773059ghq9183habn&redirect_uri=https%3A%2F%2Fnicememe.website



 {
    "application": {
        "id": "772113231757574185",
        "name": "AIRHORN SOLUTIONS",
        "icon": "f03590d3eb764081d154a66340ea7d6d",
        "description": "",
        "summary": "",
        "hook": true,
        "bot_public": true,
        "bot_require_code_grant": false,
        "verify_key": "c8cde6a3c8c6e49d86af3191287b3ce255872be1fff6dc285bdb420c06a2c3c8"
    },
    "scopes": [
        "guilds.join",
        "identify"
    ],
    "expires": "2021-01-23T02:33:17.017000+00:00",
    "user": {
        "id": "268473310986240001",
        "username": "Discord",
        "avatar": "f749bb0cbeeb26ef21eca719337d20f1",
        "discriminator": "0001",
        "public_flags": 131072
    }
}



// This is a message
{
    "content": "Mason is looking for new arena partners. What classes do you play?",
    "components": [
        {
            "type": 1,
            "components": [
                {
                    "type": 3,
                    "custom_id": "class_select_1",
                    "options":[
                        {
                            "label": "Rogue",
                            "value": "rogue",
                            "description": "Sneak n stab",
                            "emoji": {
                                "name": "rogue",
                                "id": "625891304148303894"
                            }
                        },
                        {
                            "label": "Mage",
                            "value": "mage",
                            "description": "Turn 'em into a sheep",
                            "emoji": {
                                "name": "mage",
                                "id": "625891304081063986"
                            }
                        },
                        {
                            "label": "Priest",
                            "value": "priest",
                            "description": "You get heals when I'm done doing damage",
                            "emoji": {
                                "name": "priest",
                                "id": "625891303795982337"
                            }
                        }
                    ],
                    "placeholder": "Choose a class",
                    "min_values": 1,
                    "max_values": 3
                }
            ]
        }
    ]
}
