BGG_feed is a collection of useful utilities, can be used to handle BGG feeds.
Next to this one this library contains small utilities to get prepared for Messe Essen:
- We keep track of Games we are interested in a google sheet, where the format is fixed. 1st worksheet contains the links to the game's BGG page.
  `essen2016.py` python script creates a 2nd sheet and pulls out all relevant data from BGG for these games -> this will help us creating our plan for the 2 days visit.
  
- Eric Martin is creating a big list, where all relevant information is available; this list is read through and the booth numbers, games prices (list might be longer in the future) will be collected and stored into riak. This can be triggered by calling: `review_eric:read().`
As his list for 2016 not avalibale yet, currently I'm using his 2015's list to get prepared.

- There is an RSS field called `"http://boardgamegeek.com/recentadditions/rss?domain=boardgame"` an other utility willl check the neew feeds and store new games as well. To start this functionality call `bgg_feed:start().`


TODO:
1. make rebar up-to-dae to handle mochiweb dependencies - DONE
2. start riak on the local machine and make sure data we collect will be stored into that database (try to write the code in a way that storage should be possible changing to mySQL easily)
3. modify the python script to use data stored in the database when we are dealing with fileds not available on BGG (Booth number, Price)
4. http://www.merz-verlag-en.com/new-releases.html page contains a link to a pdf file (http://www.merz-verlag-en.com/uploads/2/1/4/0/21405396/neuheiten_8.7.pdf) where new releases are listed.
Write a parser which will pull information from this sheet might be interesting for us.
5. try to find a solution making sure google sheet will be user friendly (format, etc)

