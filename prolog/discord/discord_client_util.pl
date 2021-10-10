
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
