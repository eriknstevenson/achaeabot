# Achaea Twitter Bot
Providing real-time game information on [twitter](http://twitter.com/achaeabot).

## Features
* Weekly class usage statistics
* Top 3 players posted every day and week
* Worst 3 players posted every day
* Stores data for 30 days

## Planned features
* Raid alerts

## Todo
See [Issues](https://github.com/narrative/achaeabot/issues)

## Tech
* [Iron Realms Entertainment API](http://www.ironrealms.com/IREAPIdocumentation.pdf)
* [Servant](https://hackage.haskell.org/package/servant)
* [Redis](http://redis.io)

## Building
    $ git clone https://github.com/narrative/achaeabot.git
    $ cd achaeabot
    $ stack build
    $ stack exec achaeabot-exe
    
The environment variables ```ACHAEACONSUMERKEY```, ```ACHAEACONSUMERSECRET```, ```ACHAEATOKEN```, and ```ACHAEATOKENSECRET``` are used for Twitter authentication. Visit [Twitter Developers](http://dev.twitter.com) for information on obtaining access tokens.

Additionally, the bot requires redis-server to be running on the default port (6379).

## Running the (non-existent) tests
    $ stack test

