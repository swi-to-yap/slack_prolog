# slack_prolog
Prolog Slack Client

Provides a websocket API to slack

https://github.com/swi-to-yap/slack_prolog/


I am developing a MUD in prolog and needed it so team members could play the MUD chatting a bot.


It works .

getting data over the RTM.
Posts over the https.

Still has a way to go but it gets users started by seeing how easy it was.
If you could list it on Language API bindings that be great!
Thank you in advance

Douglas Miles
Dec 13, 2035


Install.. 

1) Create a bot @  https://my.slack.com/services/new/bot
    to get the bot's token it should look something like:  xoxb-130154379991-ogFL0OFP3w6AwdJuK7wLojpK

2) ?- pack_install(slack_prolog).

3) Create a ~/.slack_auth.pl with the one secret line

slack_token('xoxb-130154379991-ogFL0OFP3w6AwdJuK7wLojpK').

4) use_module(library(prolog_client)).

5)  slack_chat('username', "some message").


TODO: add event hooks for users



