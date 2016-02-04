# Achaea Twitter Bot
Providing real-time game information on [twitter](http://twitter.com/achaeabot).

## Planned Features
* Notifications for player kills
* Weekly class usage statistics
* Stores data for 30 days
* Raid alerts

## Todo
- [ ] Make new tweet upon kill.
- [ ] Expire events after a month.
- [ ] Create a bunch of random tweet templates to be filled in with relevant details.
- [ ] Calculate weekly # of kills by class
- [ ] Calculate top killer per day
- [ ] Calculate top killer per week
- [ ] Following completion of each "task" insert a key that expires when the task needs to be reperformed. check for the presence of this key in runBot and if it's there, don't do the task.

## Tech
* [Iron Realms Entertainment API](http://www.ironrealms.com/IREAPIdocumentation.pdf)
* [Servant](https://hackage.haskell.org/package/servant)
* [Redis](http://redis.io)

## Building
    $ git clone https://github.com/narrative/achaeabot.git
    $ stack build

## Running the (non-existent) tests
    $ stack test

