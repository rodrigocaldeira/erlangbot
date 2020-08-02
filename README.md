erlangbot
=====

Um simples chatbot da Twitch escrito em erlang.

Rodando localmente
------------------

    $ export twitch_user="<usuário da twitch>"
    $ export twitch_password="oauth:<Token de autenticação da Twitch>"
    $ rebar3 shell
    
Uso
---
    1> erlangbot_server:connect().
    2> erlangbot_server:join("#<canal>").
    3> erlangbot_server:part("#<canal>").
    4> erlangbot_server:disconnect().

