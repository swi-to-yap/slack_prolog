% ====================================
% Discord Chat Calls
% ====================================

:- use_module(library(eggdrop)).

:- dynamic(t_l:discord_channel/1).
:- dynamic(t_l:channel_output_buffer/2).


discord_write_done(det('Yes',_)):-!. 
discord_write_done(Data):- write(' '),write(Data).

discord_show_each_result(_CMD,Done,_,[]):- !, Done==true,  
  get_channel_output_buffer(Str),trim_message(Str,Trimed),
  (Trimed == "" -> write("% Yes.") ; true).

discord_show_each_result(_CMD,Done,N,Vs):- 
  once(once((Done==true -> (once(\+ \+ write_varbindings_discord(Vs))) ; (once(\+ \+ write_varbindings_discord(Vs)),N>200)))).

write_varbindings_discord(Vs):- 
 wots(Str,eggdrop:write_varbindings(Vs)),
 discord_chat_say(Str).

discord_chat_say(Str):- t_l:discord_channel(DEST), discord_chat_say_buff(DEST,Str),!.
discord_chat_say(_,Msg):- t_l:discord_channel(DEST), discord_chat_say_buff(DEST,Msg),!.

discord_chat_say_buff(DEST,Msg):-
 text_to_string(Msg,Str), 
 my_wdmsg(discord_chat_say_buff(DEST,Str)),
 discord_set_typing(DEST),
 assertz(t_l:channel_output_buffer(DEST,Str)).
  

flush_channel_output_buffer(DEST):- 
 get_channel_output_buffer(DEST,Str),
 discord_say(DEST,Str),
 retractall(t_l:channel_output_buffer(DEST,_)).

get_channel_output_buffer(Str):- 
 t_l:discord_channel(DEST),
 get_channel_output_buffer(DEST,Str),!.
get_channel_output_buffer(DEST,Str):- 
 flush_output,
 LineNo = line_no(0),
 wots(Str,
  (write(' '),
   forall(t_l:channel_output_buffer(DEST,Msg),
   ( ignore((LineNo\==line_no(0),format('~N'))),write(Msg),nb_setarg(1,LineNo,1))))).

%discord_chat_override(Goal):- dmsg(discord_chat_override(Goal)),fail.
discord_chat_override(put_msg(DEST,Msg)):- !, discord_chat_say(DEST,Msg).
discord_chat_override(put_notice(DEST,Msg)):- !, discord_chat_say(DEST,Msg).
discord_chat_override(emlmp(Goal)):- !, with_no_xdbg(notrace(Goal)). % overrides `maybe_long_message_printer(4, Goal).`
discord_chat_override(Goal):- functor(Goal,F,A),atom_concat('discord_',F,DF), discord_client:current_predicate(DF/A), !,
  Goal=..[F|Args],DGoal=..[DF|Args],!,with_no_xdbg(DGoal).
discord_chat_override(Goal):- dmsg(skipped_chat_override(Goal)).
%discord_chat_override(Goal):- discord_client:call(Goal). % in-case there are new ones


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
 

discord_say_text(Channel,Msg):- any_to_chan_id(Channel,ID), discord_say_text(ID,Msg, []).

discord_say_text(_Channel,Msg,_):- trim_message(Msg,Msg2), Msg2=="",!.
discord_say_text(Channel,Msg, AddMentions):- use_file_upload(Msg), !, discord_say_text_as_file(Channel,Msg, AddMentions),!.
discord_say_text(Channel,Msg, AddMentions):- backquote_multiline_string(Msg,Str), !,
   add_to_dict(_{content: Str},AddMentions,NewDict),
   discord_http(channels/Channel/messages,[post(json(NewDict))]),!.

json_to_string(JSON,Str):- wots(Str,json_write(current_output,JSON,[])).

atom_or_string(S):- atom(S);string(S).

discord_say_text_as_file(Channel,Msg):- any_to_chan_id(Channel,ID), discord_say_text_as_file(ID,Msg, []).
discord_say_text_as_file(Channel,Msg, AddMentions):-
 add_to_dict(_{content: ""},AddMentions,NewDict),
 tmp_file('discord_say',Tmp), atom_concat(Tmp,'.txt',File),
 setup_call_cleanup(
   setup_call_cleanup(open(File,write,O),write(O,Msg),close(O)),
   discord_say_file(Channel,File,NewDict),
   delete_file(File)),!.

% todoo fix this!
discord_say_file(ID,File,JSON):- fail,
  json_to_string(JSON,Str),
  discord_http(channels/ID/messages,[
       post([ payload_json =  Str,
              filename     =  file(File)])]),!.

discord_say_file(Channel,File,JSON):-  
 http_discord_token_string(Token),
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


