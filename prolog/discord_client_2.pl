:- module(discord_client, [
        discord_start_gateway/0,
        discord_say/2,
        discord_send/1,
        ping_discord/0,
        discord_get_websocket/1,
        is_thread_running/1,
        discord_ensure_im/2,
        any_to_id/2
        ]).

/** <module> discord_client - Provides a websocket API to write discord clients

*/

disable_gateway:- false.  % true until we fix our websocket code

discord_restore_0:- 
  mutex_create(last_done_mutex),
  stream_property(X,file_no(2)),asserta(tmp:discord_debug(X)).

:- if( \+ prolog_load_context(reloading, true)).
:- discord_restore_0.
:- endif.


:- multifile(tmp:discord_info/3).
:- volatile(tmp:discord_info/3).
:- dynamic(tmp:discord_info/3).
reload_discord_info:- reconsult(guild_info_file).
load_discord_info:- exists_file(guild_info_file)->consult(guild_info_file);true.
clear_discord_info:- retractall(tmp:discord_info/3).
save_discord_info:- 
 setup_call_cleanup(open(guild_info_file,write,O),
  save_discord_info(O),
  close(O)).
save_discord_info(O):-
  functor(R,discord_info,3), TmpR=tmp:R, 
  functor(R,F,A), Dyn = tmp:F/A, 
  format(O,'~n~q.~n~q.~n~q.~n',
   [(:- multifile(Dyn)),
    (:- volatile(Dyn)),
    (:- dynamic(Dyn))]),      
  forall(TmpR,format(O,'~q.~n',[TmpR])).

:- if( \+ prolog_load_context(reloading, true)).
:- at_halt(save_discord_info).
:- load_discord_info.
:- endif.

discord_grouping(messages).
discord_grouping(channels).
discord_grouping(roles).
discord_grouping(members).
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

:- if(exists_source(library(logicmoo_utils))).
	:- use_module(library(logicmoo_utils)).
:- endif.

:- use_module(library(jpl)).

jpl_test_working :-
    jpl_call('org.jpl7.JPL', version_string, [], Vj),
    jpl_c_lib_version(Vc),
    jpl_pl_lib_version(Vp),
    nl,
    write('prolog library version: '), write( Vp), nl,
    write('  java library version: '), write( Vj), nl,
    write('     c library version: '), write( Vc), nl,
    (       Vp == Vj,
            Vj == Vc
    ->      write('BINGO! you appear to have the same version of each library installed'), nl
    ;       write('WHOOPS! you appear not to have the same version of each library installed'), nl
    ),
    nl,
    findall(
            Ar,
            (       current_prolog_flag(N, V),                                              % assume atom(N)
                    term_to_atom(V, Va),
                    jpl_list_to_array([N,Va], Ar)                                   % or jpl_new('[Ljava.lang.String;', [N,Va], Ar)
            ),
            Ars
    ),
    jpl_list_to_array(Ars, Ac),                                                     % or jpl_new('[[Ljava.lang.String;', Ars, Ac)
    jpl_list_to_array([name,value], Ah),
    jpl_new('javax.swing.JFrame', ['current_prolog_flag'], F),
    jpl_call(F, getContentPane, [], CP),
    jpl_new('javax.swing.JTable', [Ac,Ah], T),
    jpl_new('javax.swing.JScrollPane', [T], SP),
    jpl_call(CP, add, [SP,'Center'], _),
    jpl_call(F, setSize, [600,400], _),
    jpl_call(F, setVisible, [@(true)], _).
% this directive runs the above
% :- jpl_test_working.

:- if(exists_source(library(udt))).
    :- use_module(library(udt)).

:- oo_class_begin(discord_client).

% url	A WebSocket Message Server URL.
:- oo_class_field(url).

% '@me'	The authenticated bot user.
:- oo_inner_class_begin(clients).

discord_client:clients:new(Ref):- throw(clients:new(Ref)).

:- oo_inner_class_end(clients).



% '@me'	The authenticated bot user.
:- oo_inner_class_begin('@me').
:- oo_inner_class_end('@me').

% guild	Details on the authenticated user's guild.
:- oo_inner_class_begin(guild).
:- oo_inner_class_end(guild).

% users	A hash of user objects by user ID.
:- oo_inner_class_begin(users).
:- oo_inner_class_end(users).


% channels	A hash of channel objects, one for every channel visible to the authenticated user.
:- oo_inner_class_begin(channels).
:- oo_inner_class_end(channels).

% roles	A hash of role objects, one for every role the authenticated user is in.
:- oo_inner_class_begin('@me').
:- oo_inner_class_end('@me').

% ims	A hash of IM objects, one for every direct message channel visible to the authenticated user.
:- oo_inner_class_begin(chats).
:- oo_inner_class_end(chats).

% integrations	Details of the integrations set up on this guild.
:- oo_inner_class_begin(integrations).
:- oo_inner_class_end(integrations).

% text	textual utils.
:- oo_inner_class_begin(text).
:- oo_inner_class_end(text).

% debug	Debugger fidling.
:- oo_inner_class_begin(debug).
:- oo_inner_class_end(debug).

% events	Registered callbacks.
:- oo_inner_class_begin(events).
:- oo_inner_class_end(events).

% files	registered storage.
:- oo_inner_class_begin(files).
:- oo_inner_class_end(files).

:- oo_class_end(discord_client).

:- endif.



/* tests to see if logicmoo utils are installed.. If not, create the predicates it will use */
:- if( \+ current_predicate( wdmsg/1 )).

:- meta_predicate(with_visible_leash(0)).
with_visible_leash(G):-
   '$leash'(A, A),'$visible'(V, V),
   (tracing->CU=trace;CU=notrace),
   (debugging->CU2=debug;CU2=nodebug),!,
   call_cleanup(G, (notrace,'$leash'(_, A),'$visible'(_, V),call(CU2),call(CU))).

:- meta_predicate(rtrace(0)).
rtrace(G):-  with_visible_leash(( notrace,leash(-all),visible(+full),leash(+exception),trace,debug, call(G))).

:- meta_predicate(must(0)).
must(G):- G *->true;throw(must_failed(G)).

fresh_line:- format(user_error,'~N',[]).

nop(_).

:- endif.

notrace_catch(G):- notrace(catch(G,_,fail)).
debug_console(G):- (tmp:discord_debug(X);X=user_error), with_output_to(X,G).

if_debug(_).

ddbg(_):- \+ thread_self(main),!.
ddbg(O):- debug_console(ddbg0(O)).
ddbg0(O):- into_dbg_string(O,OO),
 locally(current_prolog_flag(debugger_write_options,[quoted(true), portray(true), 
   max_depth(500), attributes(portray), spacing(next_argument)]),
   in_cmt(print_tree(OO))).
%ddbg(F,Args):- if_debug((fresh_line, format(user_error,F,Args))).

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

% 2nd - Checks for a local file called ".discord_auth.pl" for tmp:discord_token/1 as above
:- if(( \+ tmp:discord_token(_) , exists_file('.discord_auth.pl'))).
:- include('.discord_auth.pl').
:- endif.

% 3rd - Checks env for DISCORD_API_TOKEN
%  ( defined by# export DISCORD_API_TOKEN=xoxb-01234567890-xxxxxxxxxxxxxxxx )
:- if(( \+ tmp:discord_token(_))).
:- getenv('DISCORD_API_TOKEN',Was)->asserta(tmp:discord_token(Was));true.
:- endif.

% 4th - Checks users config directory for file called ".discord_auth.pl"  tmp:discord_token/1 as above
:- if(( \+ tmp:discord_token(_) , exists_file('~/.discord_auth.pl'))).
:- include('~/.discord_auth.pl').
:- endif.

:- if(( \+ tmp:discord_token(_))).
:- throw(missing(tmp:discord_token(_))).
:- endif.

% ===============================================
% Utility functions
% ===============================================

discord_token_string(S):-tmp:discord_token(T),atom_string(T,S).

%curl -H "Authorization: Bot $AUTH_TOK" -H "User-Agent: DiscordBot" -H "Content-Type: application/json" 
%  https://discord.com/api/v9/

me:-  discord_http(users/'@me').
guilds:- me, discord_http(users/'@me'/guilds).
channels:- 
   %discord_http(guilds/{guilds-id}),
   discord_http(guilds/{guilds-id}/channels),
   %discord_http(guilds/{guilds-id}/members),
   %discord_http(guilds/{guilds-id}/roles),
   !.


% ===============================================
% Thread Pool Models
% ===============================================

ensure_thread_model2(M,Already):-
 \+ thread_self(M),!, (is_thread_running(M) -> call(Already) ; show_call(thread_create(M,_,[alias(M),detached(true)]))).
ensure_thread_model(M,Pause,G):- ensure_thread_model2(M,G) -> true ; (call(Pause),call(G),call(M)),!.
%ensure_thread_model(M,Pause,G):- 

ping_discord:- disable_gateway,!.
ping_discord:-  
  ensure_thread_model(ping_discord,
    sleep(41),discord_send({"op": 1, "d": $s})).


discord_message_checking:- \+ disable_gateway,!.
discord_message_checking:-  
  ensure_thread_model(discord_message_checking,
    sleep(1), check_4_tasks).

discord_message_checking_01:- disable_gateway,!.
discord_message_checking_01:-  
  ensure_thread_model(discord_message_checking_01,
    sleep(1), check_4_tasks).

discord_message_checking_02:-  
  ensure_thread_model(discord_message_checking_02,
    sleep(1), check_4_tasks).


rtrv_new_messages :- 
 forall(
 (tmp_discord_info(ChannelID,last_message_id,MessageID),
  \+ tmp_discord_info(MessageID,content,_)),
  (sleep(1),discord_http(channels/ChannelID/messages/MessageID))).

check_4_tasks:- forall(how_often(Often,Task),ignore(check_since(Often,Task))),fail.
check_4_tasks.

:- volatile(tmp:doing_task/1).
:- dynamic(tmp:doing_task/1).
:- volatile(tmp:last_done/2).
:- dynamic(tmp:last_done/2).

check_since(_Often,Task):- with_mutex(last_done_mutex, (\+ tmp:last_done(Task,_),remember_task(Task))),!,do_task(Task).
check_since(Often,Task):- with_mutex(last_done_mutex, ((tmp:last_done(Task,When), get_time(Now), When+Often > Now, remember_task(Task)))),do_task(Task).

do_task(Task):- tmp:doing_task(Task),!.
do_task(Task):-
  setup_call_cleanup(asserta(tmp:doing_task(Task),R),
     setup_call_cleanup(
         remember_task(Task), ignore(call(Task)), remember_task(Task)),erase(R)).

remember_task(Task):- with_mutex(last_done_mutex, 
  ignore((retractall(tmp:last_done(Task,_)),get_time(Time),asserta(tmp:last_done(Task,Time))))).

%how_often(200, guilds).
%how_often(30, channels).

how_often(41, discord_send({"op": 1, "d": $s})).

%how_often(1, rtrv_new_messages).
how_often(120, report_last_done).
%how_often(300, rtrv_messages).
%how_often(10, discord_message_checking_01).
%how_often(10, discord_message_checking_02).


report_last_done:- listing(tmp:doing_task/1),forall(tmp:last_done(Task,When),ignore(report_last_done(Task,When))).
report_last_done(Task,When):- get_time(Now), Ago is integer(Now-When) , how_often(Often,Task),
  dmsg(report_last_done_ago(Task,Ago/Often)).

% https://discord.com/oauth2/authorize?response_type=code&client_id=157730590492196864&scope=identify+guilds.join&state=15773059ghq9183habn&redirect_uri=https%3A%2F%2Fgoogle.com&prompt=consent
rtrv_messages:- \+ thread_self(discord_message_checking_01), forall(channel_to_id(_Channel,ID),  rtrv_messages(ID)).
rtrv_messages(ID):- tmp_discord_info(ID,last_message_id,MessageID),tmp_discord_info(MessageID,content,_),!.
rtrv_messages(ID):- discord_http(channels/ID/'messages?limit=20'),sleep(1).

% should return wss://gateway.discord.gg
%discord_get_websocket_url('wss://127.0.0.1:50051/'):-!.
discord_get_websocket_url('wss://gateway.discord.gg/'):-!.
discord_get_websocket_url(URL):- discord_http(gateway), get_discord(url,URL).

into_discord_url_object(UCmd,Prop):- atomic_list_concat([Prop2,_|_],'?',UCmd),!,into_discord_url_object(Prop2,Prop).
into_discord_url_object(UCmd,Prop):- atomic_list_concat([_,I|ST],'/',UCmd),member_rev(Prop2,[I|ST]), \+ atom_number(Prop2,_),!,into_discord_url_object(Prop2,Prop).
into_discord_url_object(Prop,Prop).

member_rev(X,[_|L]):-  member_rev(X,L).
member_rev(X,[X|_]).

discord_http(Cmd):- discord_http(Cmd,[]),!.
discord_http(Cmd,Opts):-
 must_det_l((
  into_discord_url(Cmd,UCmd),
  into_discord_url_object(UCmd,Prop),
  discord_http(Prop,Cmd,Opts))).

check_pausing:- ignore((retract(tmp : discord_info(_,retry_after,Sleep)),sleep(Sleep),sleep(0.3))).


    
discord_http(Prop,Cmd,Opts):- select(json(JSON),Opts,OptsWo), \+ is_dict(JSON), \+ atomic(JSON),
  any_to_json_dict(JSON,Dict),!,discord_http(Prop,Cmd,[json(Dict)|OptsWo]). 
discord_http(Prop,Cmd,Opts):-
 must_det_l((  
  into_discord_url(Cmd,UCmd),
  bot_discord_token(TokenHeader),
  sformat(URL,'https://discord.com/api/v9/~w',[UCmd]))),
  %wdmsg(URL=Opts),
  check_pausing,
  http_open(URL, In, [status_code(Status), 
          request_header('Authorization'=TokenHeader), 
          request_header('User-Agent'='DiscordBot'),
          request_header('Content-Type'='application/json')|Opts]),
  ignore((Status\==200,dmsg((URL:Opts=Status)))),
  must_det_l((json_read_dict(In,Term), close(In), discord_receive(Prop,Term))),
  nop(listing(tmp:discord_info/3)),!.



into_discord_url(A,O):- \+ compound(A),!,A=O.
into_discord_url('$'(A),O):- !, get_discord(A,M),into_discord_url(M,O),!.
into_discord_url(A / B,O):- !, into_discord_url(A,AA),into_discord_url(B,BB),!,sformat(O,"~w/~w",[AA,BB]).
into_discord_url({A - B},O):- !, get_discord(A,B,M),into_discord_url(M,O).
into_discord_url(A,O):- A=O.

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
skip_propname(K):- var(K),!.
skip_propname(_-_):-!,fail.
skip_propname(Type):-string(Type),!,string_to_atom(Type,K),!,skip_propname(K).
skip_propname(rtm).
skip_propname(gateway).
skip_propname(data).
skip_propname(var).

discord_propname(Type,var):-var(Type),!.
discord_propname(Type,K):-string(Type),!,string_to_atom(Type,K).
discord_propname(Key-Type,NewType):-!,discord_propname(Key,Type,NewType).
%discord_propname(Dict,NewType):- nonvar(Dict),Dict=Key.Type,nonvar(Type),!,discord_propname(Key,Type,NewType).
%discord_propname(Key.Type,NewType):-!,discord_propname(Key,Type,NewType).
discord_propname(Key,Key).

discord_propname(Type,Key,NewType):- skip_propname(Type),!,discord_propname(Key,NewType).
discord_propname(Key,Type,NewType):- skip_propname(Type),!,discord_propname(Key,NewType).
discord_propname(_Type,Key,NewType):-discord_propname(Key,NewType).

discord_start_gateway:- disable_gateway,!.
discord_start_gateway:- tmp:jpl_websocket(_),!.
discord_start_gateway:- is_thread_running(discord_start_gateway),!.
%discord_start_gateway:- discord_gateway_proc,!.
discord_start_gateway:- \+ thread_self(discord_start_gateway), 
  !,thread_create(discord_gateway_proc,_,[alias(discord_start_gateway)]),!.

string_to_dict:-
 string_to_dict("{\"type\":\"dnd_updated_user\",\"user\":\"U3T3R279S\",\"dnd_status\":{\"dnd_enabled\":false,\"next_dnd_start_ts\":1,\"next_dnd_end_ts\":1},\"event_ts\":\"1485012634.280271\"}",Dict),
  ddbg(Dict).


string_to_dict(String,Dict):-
   open_string(String,Stream),
   notrace_catch(json_read_dict(Stream,Dict)),!.

system:discord_websocket_hook(Event,Message):- debug_console(disco_websocket_hook(Event,Message)).



disco_websocket_hook(onMessage,Message):- atom_contains(Message,'"op":11'),!.
disco_websocket_hook(onMessage,Message):-
    fail, must_det_l((string_to_dict(Message,Dict),discord_receive(gateway,Dict))),!.
disco_websocket_hook(Type,Message):-  
     assertz(tmp:discord_websocket_event(Type,Message)),
     writeln(user_error,discord_websocket_hook(Type,Message)),!.

discord_gateway_proc:- tmp:jpl_websocket(_),!.
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


% ignored
%discord_event(reconnect_url,_Dict):-!.
/*
  must((Dict.url=URL,
   ddbg(reconnect(URL)),!,
   ddbg(discord_open_websocket(URL,_)))).
*/

% simplify the data objects
discord_event(Type,O):- is_dict(O),O.Key=Data,Key=data,!,discord_receive(Type,Data),!.
% typify the data objects
discord_event(gateway,O):- is_dict(O),O.Key=Type,Key=type,!,discord_receive(Type,O),!.
% typify the data objects
discord_event(gateway,O):- is_dict(O),O.Key=Type,Key=op,!,discord_receive(Type,O),!.


% text:"This content can't be displayed."

% Notice newly created IMs
discord_event(im_open,Dict):- is_dict(Dict),
  Dict.channel=IDI,
  Dict.user=User,
  undict(IDI,ID),
  string_to_atom(ID,IDA),
  add_discord_info2(ims, hasInstance- IDA),
  add_discord_info2(IDA, id- ID),
  add_discord_info2(IDA, user-User).

%discord_event(Evt,end_of_file):- throw(discord_event(Evt,end_of_file)).


% discord_event(Type,Data):-add_discord_info2(now,Type,Data).

discord_unused(user_typing).
discord_unused(reconnect_url).
  

discord_receive(Type,Data):- number(Type), gw_op(Type,Dispatch,_,_),!,discord_receive(Dispatch,Data).
discord_receive(heartbeat_ack(11),_Data):- !.
discord_receive(Type,Data):- string(Data),string_to_dict(Data,Dict),!,discord_receive(Type,Dict).
 % discord_send({op:11}), 
%   sleep(1),discord_send({op:1,d:null}),!,discord_identify. 
discord_receive(Type,Data):- ddbg((discord_receive(Type,Data))),fail.

discord_receive(Type,Data):- discord_propname(Type,NewType)-> Type\==NewType,!,discord_receive(NewType,Data).
discord_receive(Type,Dict):- type_to_url(K,Type)-> K\==Type,!, discord_receive(K,Dict).
%discord_receive(Type,Data):- discord_event(Type,Data),!.
discord_receive(Type,Data):- discord_unused(Type), nop(ddbg(unused(discord_receive(Type,Data)))),!.
%discord_receive(Type,Data):- ddbg(unknown(discord_receive(Type,Data))).
discord_receive(Type,Data):- once(add_discord_info2(Type,Data)),fail.
discord_receive(_Type,_Data).

/*
request_members:- 
discord_send({
  "op": 8,
  d: {
    "guild_id": $guild_id,
    "query": "",
    "limit": 0
  }
}).
*/
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
}).

discord_resume:- 
 discord_send( {
  "op": 6,
  d: {
    "token": $token,
    "session_id": $session_id,
    "seq": $s
  }
}).




get_kv(K:V,K,V):- must(nonvar(K);throw(get_kv(K:V,K,V))).
get_kv(K-V,K,V).
get_kv(K=V,K,V).


% curl -H "Authorization: Bot $AUTH_TOKEN" -H "User-Agent: DiscordBot" https://discord.com/api/channels/892809238710222930/messages


add_discord_info2(Prop,List):- discord_grouping(Prop), fail,
 is_list(List),!, maplist(add_discord_info2(Prop),List).

add_discord_info2(Type,Pairs):- is_list(Pairs),select(KV,Pairs,Rest),get_kv(KV,id,ID),!,
  add_discord_info(ID,isa,Type), add_discord_info(Type,hasInstance,ID),
  add_discord_info(ID,id,ID),
  add_discord_info2(ID,Rest),!.
add_discord_info2(Type,Pairs):- is_list(Pairs), Pairs\==[], !, maplist(add_discord_info2(Type),Pairs),!.

add_discord_info2(Type,KV):- get_kv(KV,K,V),!,add_discord_info(Type,K,V),!.
add_discord_info2(Type,Data):- is_dict(Data),dict_pairs(Data,_Tag,Pairs),!,add_discord_info2(Type,Pairs),!.
add_discord_info2(Type,Data):- %retractall(tmp:discord_info(Type,_,_)),
  add_discord_info(Type,hasInstance,Data),!.


%753344235805343785
int_to_name(S,V):- S>1420070400,get_time(T),TT is T + 6000,TT>S,stamp_date_time(S,Date,local),!,
  format_time(string(V),'[%a, %d %b %Y %T PST]',Date,posix).
int_to_name(S,V):- tmp_discord_info(S,username,VV),!,sformat(V,'{~q/user-~w}',[S,VV]).
int_to_name(S,V):- tmp_discord_info(S,name,VV),!,sformat(V,'{~q/~w}',[S,VV]).
%int_to_name(S,V):- tmp_discord_info(Vc,ontent,S),!.
int_to_name(S,V):- S> 4194304, id_to_time(S,T),int_to_name(T,TT),sformat(V,'{~q/~w}',[S,TT]).

into_dbg_string(S,V):- compound(S), compound_name_arguments(S,F,As),maplist(into_dbg_string,As,AAs),!,compound_name_arguments(V,F,AAs).
into_dbg_string(S,V):- string(S), notrace_catch(atom_number(S,N)), !, into_dbg_string(N,V).
into_dbg_string(S,V):- number(S), int_to_name(S,V),!.
into_dbg_string(V,V).

from_string(S,V):- compound(S), compound_name_arguments(S,F,As),maplist(from_string,As,AAs),!,compound_name_arguments(V,F,AAs).
from_string(S,V):- \+ string(S), \+ atom(S), !, V=S.
from_string(S,V):- atom_length(S,L),L>40,!,S=V.
from_string(S,V):- \+ atom(S), text_to_string(S,SS),string_to_atom(SS,A),notrace_catch(atom_number(A,V)),!.
from_string(S,V):- parse_time(S,_,V),!.
from_string(V,V).

id_to_time(ID,UTC):- integer(ID), UTC is (( ID >> 22) / 1000) + 1420070400.

add_discord_info(gateway,Prop,Data):- !, add_discord_info2(Prop,Data).
add_discord_info(d,Prop,Data):- Prop\==hasInstance,!, add_discord_info2(Prop,Data).
add_discord_info(Guild,Prop,Data):- default_guild(Guild), !, add_discord_info2(Prop,Data).

add_discord_info(ID,Prop,Data):- 
  from_string(ID,ID2), from_string(Prop,Prop2), from_string(Data,Data2),!,
  add_discord_info4(ID2,Prop2,Data2).

add_discord_info4(_,Prop,Data):- default_info(Prop,Data),!.
add_discord_info4(ID,Prop,Data):-
  TmpR=tmp:R,
  R= discord_info(ID,Prop,Data),
  (\+ \+ call(TmpR) -> (retract(TmpR),assert(TmpR)) ; (asserta(TmpR),dmsg(TmpR))).

get_discord(ID,Data):- tmp_discord_info(ID,hasInstance,Data).
get_discord(ID,Prop,Value):- tmp_discord_info(ID,Prop,Value)*->true;get_discord2(ID,Prop,Value).

get_discord2(Type,Prop,Value):- tmp_discord_info(Type,hasInstance,ID),tmp_discord_info(ID,Prop,Value).

get_discord_info(ID,Prop,Data):- tmp_discord_info(ID,Prop,Data)*-> true;
  (\+ integer(ID), \+ var(ID), any_to_id(ID,ID2),!, tmp_discord_info(ID2,Prop,Data)).

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

any_to_each_1(U1,Each):- notrace_catch(text_to_string(U1,Each)).
any_to_each_1(U1,Each):- term_to_atom(U1,Each). 
any_to_each_1(U1,Each):- from_string(U1,Each), \+ integer(Each).
any_to_each_1(U1,Each):- any_to_id(U1,Each).


discord_name_id_type(Name,ID,Type):- nonvar(Type),  !,
  (tmp_discord_info(Type,hasInstance,ID)*->true;tmp_discord_info(ID,isa,Type)),
  once(tmp_discord_info(ID,name,Name);tmp_discord_info(ID,username,Name)).
discord_name_id_type(Name,ID,Type):- 
  (tmp_discord_info(ID,username,Name);tmp_discord_info(ID,name,Name)),
  integer(ID),
  once(tmp_discord_info(Type,hasInstance,ID);tmp_discord_info(ID,isa,Type)).

tmp_discord_info(A,B,C):- tmp:discord_info(A,B,C).
  %get_discord(Type,hasInstance,ID),
  %get_discord(ID,id,ID),
  %true.872902388623757364


channel_to_id(Name,ID):- discord_name_id_type(Name,ID,channels).
%channel_to_id("prologmud_server",892806433970716692).
%channel_to_id(Name,ID):- any_to_id(Name,ID).

any_to_id(Name,ID):-var(Name),!, fail,ID=Name.
any_to_id(Name,ID):-integer(Name),ID=Name.
any_to_id(Name,ID):- notrace_catch(text_to_string(Name,NameS)),discord_name_id_type(NameS,ID,_),integer(ID),!.
any_to_id(Name,ID):-from_string(Name,ID),integer(ID),!.
%any_to_id(Name,ID):-text_to_string(Name,NameS),get_discord(_,hasInstance,ID), get_discord(ID,_,NameS),!.

same_ids(ID,IDS):-any_to_id(ID,IDA),any_to_id(IDS,IDB),IDA==IDB.

discord_ensure_im2(ID,IM):- get_discord(IM,user,ID),!.

discord_ensure_im(To,IM):- get_discord(IM, name, To), get_discord(IM, is_channel, true),!.
discord_ensure_im(To,IM):- any_to_id(To,ID),!, discord_ensure_im(ID,IM).
discord_ensure_im(To,IM):- discord_ensure_im2(To,IM),!.
% OLD discord_ensure_im(To,IM):- any_to_id(To,ID), discord_send({type:'im_open',user:ID}),!,must(discord_ensure_im2(To,IM)),!.
discord_ensure_im(To,IM):- discord_send({type:'conversations_open',users:To}),!,must(discord_ensure_im2(To,IM)),!.


discord_id_time(ID,TS):-flag(discord_id,OID,OID+1),ID is OID+1,get_time(Time),number_string(Time,TS).
discord_me(Self):-get_discord('@me', id, Self).


find_discord_info(Str):-
 forall(tmp:discord_info(X,Y,Z),
 ignore((sformat(S,'~q.',[tmp:discord_info(X,Y,Z)]),sub_string(S, _Offset0, _Length, _After, Str),
   ddbg(S)))).

discord_say11:- 
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

as_msg_string(call(Msg),SF):- wots(Str,call(Msg)),!,sformat(SF,'```~w```',[Str]).
as_msg_string(Msg,SF):- compound(Msg),wots(Str,in_cmt(print_tree(Msg))),!,sformat(SF,'```~w```',[Str]).
as_msg_string(Msg,Str):- any_to_string(Msg,Str).

%discord_say:- discord_say(_,'From Prolog').
% {"id":3,"type":"message","channel":"D3U47CE4W","text":"hi there"}
discord_say(X):- discord_say('prologmud_server',X).
discord_say :- discord_say('prologmud_server',"test message to logicmoo").
discord_say1 :- discord_say('#prologmud_server',"test message to logicmoo").
discord_say2:- discord_say(dmiles,"test message to dmiles").
discord_say3:- discord_say(general,"test message to general channel").
% https://discord.com/oauth2/authorize?client_id=772113231757574185&scope=bot&permissions=268823638
discord_say(Channel,Msg):- 
 any_to_id(Channel,ID), 
 as_msg_string(Msg,Str),
 % Nick = 'some1',
 % atomic_string_concat(Str,StrO),
 Str=StrO,
  Dict=
    _{username: "irc0", content: StrO,
       avatar : "98fb2a9b870148862265b65d02b5d200",
    %embeds: [_{ title: "Hello, Embed!", description: "This is an embedded message."},
    tts: false},
 %sformat(S,'~q',[Dict]),
 %ddbg(post=S),
 discord_http(channels/ID/messages,[post(json(Dict))]),!.

discord_say(To,Msg):-
  discord_ensure_im(To,IM),
  discord_send_im(IM,'irc0',Msg).

discord_send_im(IM,From,Msg):-
    discord_send({
            type: "message", 
            username:From,
	    channel: IM,
            text: Msg
	   }),!.

discord_post(Cmd,Params,NewDict):- 
          tmp:discord_token(Token),
	  make_url_params(Params,URLParams),
	  format(string(S),'https://discord.com/api/v9/~w',[Cmd,Token,URLParams]),
	  dmsg('~N DISCORD-POST ~q ~n',[S]),!,
	  http_open(S,Out,[]),!,
	  json_read_dict(Out,Dict),
	  dict_append_curls(Dict,Params,NewDict),!.
	  

discord_post(Cmd,Params):-
  discord_post(Cmd,Params,NewDict),
	  discord_receive(Cmd,NewDict).

dict_append_curls(Dict,Params,NewDict):-any_to_curls(Params,Curly),
	dict_append_curls3(Dict,Curly,NewDict).

dict_append_curls3(Dict,{},Dict):-!.
dict_append_curls3(Dict,{Curly},NewDict):-!,dict_append_curls3(Dict,Curly,NewDict).
dict_append_curls3(Dict,(A,B),NewDict):-!,dict_append_curls3(Dict,A,NewDictM),dict_append_curls3(NewDictM,B,NewDict).
dict_append_curls3(Dict,KS:V,NewDict):- string_to_atom(KS,K), put_dict(K,Dict,V,NewDict).

discord_history(To,History):- discord_ensure_im(To,IM),
 % (get_discord(IM, is_channel, true)-> Method = 'channels.history' ; Method = 'conversations.history'),
  Method = 'conversations.history',
  discord_send_receive({type:Method,channel:IM},History).


bot_discord_token(TokenHeader):- tmp:discord_token(Token),sformat(TokenHeader,"Bot ~w",[Token]).




type_to_url("message",'chat.postMessage').
type_to_url("im_open",'im.open').
type_to_url("conversations_open",'conversations.open').
type_to_url(X,X):-!.

make_url_params({In},Out):-!,make_url_params(In,Out).
make_url_params((A,B),Out):-!,make_url_params(A,AA),make_url_params(B,BB),format(atom(Out),'~w&~w',[AA,BB]).
make_url_params([A|B],Out):-!,make_url_params(A,AA),make_url_params(B,BB),format(atom(Out),'~w&~w',[AA,BB]).
make_url_params([A],Out):-!,make_url_params(A,Out).
make_url_params(KV,Out):-get_kv(KV,K,A),www_form_encode(A,AA),format(atom(Out),'~w=~w',[K,AA]).


discord_send(DataI):- any_to_curls(DataI,Data),discord_send000(Data).
discord_send_receive(DataI,Recv):- any_to_curls(DataI,Data),discord_send_receive000(Data,Recv).


discord_send_receive000({"type":TypeT,Params},Recv):- text_to_string(TypeT,Type), type_to_url(Type,Cmd),!, discord_post(Cmd,Params,Recv).
% @TODO comment the above and fix this next block
discord_send_receive000(Data,Recv):- 
  discord_send_ws(WebSocket,Data),!,
  once(my_ws_receive(WebSocket,Recv,[format(json)])),!.

%discord_send000({"type":TypeT,Params}):- text_to_string(TypeT,Type), type_to_url(Type,Cmd),!, discord_post(Cmd,Params).
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
%discord_send_ws(WsOutput,Data):- is_stream(WsOutput), format(WsOutput,'~q',[Data]),flush_output(WsOutput),ddbg(discord_sent(Data)),flush_output.

any_to_json_dict(D,D):- is_dict(D,_). 
any_to_json_dict(List,Dict):- is_list(List),dict_create(Dict,_,List).

any_to_json_dict({C},D):- conjuncts_to_list(C,L),maplist(any_to_json_dict_arg,L,L2),dict_create(D,_,L2).
any_to_json_dict(S,V):- compound(S), compound_name_arguments(S,F,As),maplist(any_to_json_dict,As,AAs),!,compound_name_arguments(V,F,AAs).
any_to_json_dict(String,D):- notrace_catch(atom_json_term(String,D,[as(atom)])),!.
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
any_to_curls(KV,AA:BB):-get_kv(KV,A,B),!,any_to_curls(A,AA),any_to_curls(B,BB).
any_to_curls('$'(Var),ValO):- get_discord(Var,Val),!,any_to_curls(Val,ValO).
any_to_curls({DataI},{Data}):-!,any_to_curls(DataI,Data).
any_to_curls((A,B),(AA,BB)):-!,any_to_curls(A,AA),any_to_curls(B,BB).
any_to_curls(A,O):- string(A),!,from_string(A,O).
any_to_curls(A,O):- notrace_catch(text_to_string(A,AA)),!,any_to_curls(AA,O).
any_to_curls(A,O):- from_string(A,O).

gw_op(0,'dispatch','receive','an event was dispatched.').
gw_op(1,'heartbeat',_,'fired periodically by the client to keep the connection alive.').
gw_op(2,'identify','send','starts a new session during the initial handshake.').
gw_op(3,'presence update','send','update the client''s presence.').
gw_op(4,'voice state update','send','used to join/leave or move between voice channels.').
gw_op(6,'resume','send','resume a previous session that was disconnected.').
gw_op(7,'reconnect','receive','you should attempt to reconnect and resume immediately.').
gw_op(8,'request guild members','send','request information about offline guild members in a large guild.').
gw_op(9,'invalid session','receive','the session has been invalidated. you should reconnect and identify/resume accordingly.').
gw_op(10,'hello','receive','sent immediately after connecting and contains the heartbeat_interval to use.').
gw_op(11,heartbeat_ack(11),'receive','sent in response to receiving a heartbeat to acknowledge that it has been received.').

default_guild(748871194572226661).

no_default_info(topic).
no_default_info(position).
no_default_info(parent_id).
no_default_info(hasInstance).
no_default_info(id).
no_default_info(isa).

default_info(X,_):- nonvar(X), no_default_info(X),!,fail.
default_info(flags,0). % probably this doesnt belong
default_info(accent_color,null).
default_info(attachments,[]).
default_info(avatar,null).
default_info(banner,null).
default_info(banner_color,null).
default_info(components,[]).
default_info(edited_timestamp,null).
default_info(embeds,[]).
default_info(guild_id,GuildID):- default_guild(GuildID).
default_info(last_message_id,null).
default_info(mention_everyone,false).
default_info(mention_roles,[]).
default_info(mentions,[]).
default_info(nsfw,false).
default_info(permission_overwrites,[]).
default_info(pinned,false).
default_info(rate_limit_per_user,0).
default_info(rtc_region,null).
default_info(tts,false).
default_info(type,0).
default_info(user_limit,0).
default_info(public_flags,0).
default_info(email,null).
default_info(features,[]).
default_info(messages,[]).
default_info(owner,false).


default_info(X,Y):- default_info_value(Y),!, nop(dmsg(default_info(X,Y))).

default_info_value(null).
default_info_value(0).
default_info_value([]).
default_info_value(false).


:- fixup_exports.


%:- autoload_all.
discord_restore_1:- 
  add_discord_info2(s,null),
  bot_discord_token(TokenHeaderB), add_discord_info2(token,TokenHeaderB),
  discord_token_string(TokenHeader), add_discord_info2(token,TokenHeader),
  get_time(Time), ITime is integer(Time), add_discord_info2(time,ITime).

:- if( \+ prolog_load_context(reloading, true)).
:- discord_restore_1.
:- endif.


discord_restore_2:- 
  %setenv('CLASSPATH','/opt/logicmoo_workspace/packs_sys/swicli/build/application/classes:/opt/logicmoo_workspace/packs_sys/swicli/lib/javax.websocket-api-1.0.jar'),
  jpl_new('org.logicmoo.jpl.callback.PrologWebSocket',['wss://gateway.discord.gg','discord_websocket_hook'],O),
  assert(tmp:jpl_websocket(O)),!.
:- if( \+ prolog_load_context(reloading, true)).
:- discord_restore_2.
:- endif.


% start discord gateway in a thread
:- discord_start_gateway.
% start discord pinger in a thread
:- ping_discord.
% start discord message checker in a thread 
:- discord_message_checking.
/*
:- discord_message_checking_01.
:- discord_message_checking_02.
*/

end_of_file.


% if the above fails .. run in debug mode
:- if(( \+ (is_thread_running(discord_start_gateway)))).
:- discord_gateway_proc -> true; rtrace(discord_gateway_proc).
:- endif.

% start discord gateway in a thread
:- if(( \+ (is_thread_running(discord_start_gateway)))).
:- thread_create(discord_start_gateway,_,[alias(discord_start_gateway),detached(true)]).
:- endif.

% if the above fails .. run in debug mode
:- if(( \+ (is_thread_running(discord_start_gateway)))).
:- rtrace(discord_start_gateway).
:- endif.




%discord_receive(Type,Data):- loop_check(discord_inform(Type,Data),true),!.

discord_inform(Type,Data):-is_dict(Data),Data.Key=ID,Key=id,!,string_to_atom(ID,Atom),
   add_discord_info2(Type,Atom-Data),!.
discord_inform(rtm,Data):- is_list(Data),!, maplist(discord_receive(rtm),Data).


discord_inform(Type,Key-[A|Data]):-is_dict(A),is_list(Data),!,maplist(discord_receive(Type-Key),[A|Data]),!.

discord_inform(Type,Key-Data):- % atomic(Key),atomic_list_concat([Type,Key],'_',TypeKey),ddbg(list(discord_receive(TypeKey,Data))),
  is_list(Data),!,
  retractall(tmp:discord_info(Type,Key,_)),
  maplist(add_discord_info(Type,Key),Data),!.

discord_inform(Type,Key-Data):- atomic(Data),!,add_discord_info2(Type,Key-Data),!.
discord_inform(Type,Key-Data):- is_dict(Data),dict_pairs(Data,Tag,Pairs),!,maplist(discord_receive(Type-Key-Tag),Pairs),!.
discord_inform(Type,Key-Data):- add_discord_info2(Type,Key-Data),!.


