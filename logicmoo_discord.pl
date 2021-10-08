%#!/usr/bin/swipl 

:- module(logicmoo_ircbot,[]).

% ==============================================
% [Required] Load the Logicmoo Common Utils
% ==============================================
:- ensure_loaded(library(logicmoo_common)).

:- whenever_flag_permits(load_network,load_library_system(library(logicmoo_network))).

end_of_file.


https://discord.com/api/oauth2/authorize?client_id=157730590492196864&scope=bot&permissions=1

/channels/{channel.id}/messages

Name=logicmoo-app
USERNAME=PrologMUD#4124
PERMISSION=536536415425
Public=63920eac424255a5a57fc6b48a7c958c46621ab18505506b1d3fa32f3708e638
Client=772113231757574185
Channel=892809238710222930
Token=


curl -v -H "Authorization: bvz7rRSe4N6ol-ss0Jo1yH4LwDv9nBxmNZ3qdXCMIlU7j86VuRShYxqGo4nviUQkbwe7" -H "User-Agent: curl" -H "Content-Type: application/json" -H "Content-Length: 0" -X GET https://discord.com/api/v9/channels/892809238710222930/messages
curl -v -H "Authorization: NzcyMTEzMjMxNzU3NTc0MTg1.X518ew.QDgS_sK7lbG36t69emm2usGdek4" -H "User-Agent: curl" -H "Content-Type: application/json" -H "Content-Length: 0" -X GET https://discord.com/api/v9/channels/892809238710222930/messages
curl -v -H "Authorization: Bot $AUTH_TOK" -H "User-Agent: curl" -H "Content-Type: application/json" -XGET https://discord.com/api/v9/channels/892809238710222930/messages



NzUyMzM4MTY2NzczNDQ4NzA0.X1WLhA.3Vyw68oU99NmY11OlqM8MSjqEf8
cJ_O7j79k_3LYY5-UiNJ5FvWDpdKe0e5

 curl -v -H "Authorization: BNzUyMzM4MTY2NzczNDQ4NzA0.X1WLhA.3Vyw68oU99NmY11OlqM8MSjqEf8" -H "User-Agent: DiscordBot" -H "Content-Type: application/json" -H "Content-Length: 0" -X GET https://discord.com/api/channels/892809238710222930/messages

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


https://discord.com/api/oauth2/authorize?response_type=code&client_id=157730590492196864&scope=webhook.incoming&state=15773059ghq9183habn&redirect_uri=https%3A%2F%2Fnicememe.website



 {
    "application": {
        "id": "159799960412356608",
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