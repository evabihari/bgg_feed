BGG_feed is a collection of useful utilities, can be used to handle BGG feeds.

Next to this one this library contains small utilities to get prepared for Messe Essen:

- We keep track of Games we are interested in a google sheet, where the format is fixed. 1st worksheet contains the links to the game's BGG page.
 
	 `essen2016.py` python script creates a 2nd sheet and pulls out all relevant data from BGG for these games -> this will help us creating our plan for the 2 days visit.
  
- Eric Martin is creating a big list, where all relevant information is available; this list is read through and the booth numbers, games prices (list might be longer in the future) will be collected and stored into riak. This can be triggered by calling: `review_eric:read().`

- There is an RSS field called `"http://boardgamegeek.com/recentadditions/rss?domain=boardgame"` an other utility willl check the new feeds and store new games as well. 

To start this functionality call `bgg_feed:start().`


TODO:

1. make rebar up-to-dae to handle mochiweb dependencies - DONE

2. start riak on the local machine and make sure data we collect will be stored into that database (try to write the code in a way that storage should be possible changing to mySQL easily)

		2.1 install riak on your local machine
   
		2.2 start riak
   
	   `cd ~/external/riak-2.1.4`
	   `bin/riak start`
	   
		2.3 let's assume 127.0.0.1:8098 is set as istener.http.internal
   
		listener.protobuf.internal = 127.0.0.1:8087
   
3. modify the python script to use data stored in the database when we
   are dealing with fields not available on BGG (Booth number, Price)
   - DONE
4. From Eric's 2016's GenCON list -> Price sometimes added as MSRP tag
   (simliar to the Price) in the Body section
   -DONE - check formatting and unicode characters - EUR is now
   handled correctly
5. Create an API to the Riak and Mnesia database to be able to include
   price information from local shops, etc...
   -partly done:
   `bgg_feed_utils:add_price(Game_id,PriceStr).`
6. http://www.merz-verlag-en.com/new-releases.html page contains a
link to a pdf file
(http://www.merz-verlag-en.com/uploads/2/1/4/0/21405396/neuheiten_8.7.pdf)
where new releases are listed.

Try to use: `pdftotext input_pdf output_file -table`

Table is not available until Sept

Write a parser which will pull information from this sheet might be interesting for us.
7. Unicode not supported for Riak keys -> find out a method to convert
   Publisher names to a string which does not contain unicode
   char's. (need to be done on the parsing side (Erlang) and the
   google docs handling (Python) side as well
   
   -DONE
   Idea:
   1. We should URL encode the Publisher name and use that one for
   key! - not working special chars remained in the string + url
   encoding resulted different string in Python and Erlang
   2. Remove non_asscii characters from the name when creating the key
      -> this is now working! Problem can be if a Publisher name will
      contain only non-ASCII chars (ex. Japaneese nam)
8. try to find a solution making sure google sheet will be user friendly (format, etc)


9. Write a helper to go through the Math trade list -
   "https://boardgamegeek.com/xmlapi/geeklist/212782/essen-spiel-2016-no-shipping-math-trade"
   Create a table which contains the games which were offered,
   extracting the following information:
   - item no
   - BGG id
   - Title:
   - Publisher:
   - Language(s):
   - Attendance at SPIEL'16: (e.g.: Wed, Thu, Fri, Sat, Sun)
   - min no of players
   
   To load the information into google sheet use:
   `python math_trade.py`
   To read the list from BGG and store to Mnesia&riak use
   `math_trade:read()` in the Erlang shell
   Now this function has been modified a bit:
   - performance improvement: The game is added to the games database
     as well, next time we don't read to ask BGG for data just read
     from Mnesia
	 - after the list is read through it will be dumped to a CSV file
       which is then copied to my Public Dropbox directory, so it
       public and available at: 
	   <https://dl.dropboxusercontent.com/u/4001011/LeftOvers_LeftOver.csv>
   At this moment `Essen2016 no shipping leftovers leftovers math trade`
       list is parsed.

Other
====
Airtable (https://airtable.com) API support added.

Basic usage:
1. initialize:

`hackney:start().
 AT=airtable:init(Base_id, API_key)`
 
 where `Base_id` is the ID connected to the Airtable table you want to
 modify
 `API_key` is the key AIrtable provided towards you - please keep it
 for your personal usage!

2. create new record:

 `airtable:create(AT, Table_name, Data). -> {ok, #airtable_record}|{error,Reason}`
 creates a new record in the given Airtable Table.
 
 Data can be encoded by calling:`airtable_record:encode(...)`
 
 examples:
` 
 encode([{"id","value"},{"key","value1"},{"key2",["v1","v2"]}])
 
 > "{\"fields\": {\"id\": \"value\", \"key\": \"value1\", \"key2\":  [ \"v1\",\"v2\" ]  }}"

 attachment handling:
 encode([{"id","value"},{"key","value1"},{"attachments",[{"id","value"},{"url","urlvalue"}]}])
>  "{\"fields\": {\"id\": \"value\", \"key\": \"value1\",\"attachments\":
>                                       [{ \"id\": \"value\", \"url\": \"urlvalue\"}] }}"`
 
3. list records:
`airtable:get(AT, Table_name) -> {ok,[#airtable_record]}|{error,Reason}`

4. update some fields in a record:

`airtable:update(AT, Table_name, Record_id, Data). -> {ok,
#airtable_record}|{error,Reason}`

Record_id is internal Airtable record idenity (see at  airtable:get)

5. update all fields in a record:
`airtable:update_all(AT, Table_name, Record_id, Data). -> {ok,
#airtable_record}|{error,Reason}`

Record_id is internal Airtable record idenity (see at  airtable:get)
 
 6. Delete an airtable record:
 
`airtable:delete(AT, Table_name, Record_id) -> {ok, "deleted"} |
{erreor, Reason}`

7. Find records in a Table where a field equals to a specific value:

`airtable:find(AT, Table_name,Filed_name, Value). -> `{ok,
[#airtable_record]}|{error,Reason}`


8. Filter records in a Table where a logical query is true:
`airtable:filter(AT, Table_name,Query). -> {ok,
[#airtable_record]}|{error,Reason}`

`airtable:filter(AT, Table_name,{FieldName,Operator,Value}). -> {ok,
[#airtable_record]}|{error,Reason}`

9. Game can be added to the "Games" airtable, some of the
   corresponding links are created as well (Publishers, etc)
   			  
			  `bgg_to_airtable:game_to_airtable(Id)`
			  
10. `essen2016.py` script now also storing information into Airtable
(includingg adding attachments to the games and creating links" 
			  
## Functionality ##

### BGG MathTrade ###
1. `math_trade:read().`
`?ESSEN_MT_2016` URL parsed, offered items are stored into
`math_trade_items` table.
Database is dumped to `?OUT` file (actual default is
LeftOvers_LeftOver.csv) and copied to `?DROPBOX_PUBLIC` directory.

2. `math_trade:dump_to_file(<FILE>`
Offered items database dumped to the given file

Example:

`item_no |	 id |	 title |	 publisher |	 language |
attendance |	 min_player |	 max_player |	 interested |
condition |

1	|159141|	 Code of Nine	|  Z-Man Games (2015)	|  English	|
Thursday, Friday, Saturday, Sunday |	3	|4	| not decided	|  new
and still in shrink. No damages, pristine condition.`


### BGG Auction list ###
1.` auction_list:read().`

`?ESSEN_NSA_2016` URL parsed, items are stored into `auction_items`
table.

2. `auction_list:find(Game).`

Where Games is either GameId (integer) or GameName (string).
Finds and lists items with the given ID/Name on the auction list;
Example:
`
153> auction_list:find("Argonauts").
------------ITEM FOUND-----------
Item_no:         723
Id:              4805073
Game id:         171356
Game name:       Argonauts
Owned by:        rahdo
Sold:            false
Condition:        Like new
Language:
Lang_dep:
Version:
Starting_bid:    Starting Bid â¬15
BIN:
Actual bid:      20
Actual winner:   MithrasSWE
Pres. & Essen:    Thursday to Sunday
Auction ends:     October 1st, random time
------------ITEM END-----------
------------ITEM FOUND-----------
Item_no:         2219
Id:              4850678
Game id:         171356
Game name:       Argonauts
Owned by:        Vagos
Sold:            false
Condition:        New in shrink
Language:         English
Lang_dep:
Version:
Starting_bid:     14â¬
BIN:              38â¬
Actual bid:      15
Actual winner:   A skinned math nerd
Pres. & Essen:
Auction ends:     September 30th at random time
------------ITEM END----------- 
`

3. `auction_list:what_wins(User)`
Where User is the string or atom of the userName.

Lists these Items where the User is the actual winner of the bid.

Example:
`
auction_list:what_wins("A skinned math nerd").
------------ITEM FOUND-----------
Item_no:         2219
Game name:       Argonauts
Owned by:        Vagos
Condition:        New in shrink
Language:         English
Lang_dep:
BIN:              38â¬
Actual bid:      15
Actual winner:   A skinned math nerd
Pres. & Essen:
Auction ends:     September 30th at random time
------------ITEM END-----------

`
4. `auction_list:traded_by(User).`
Where User is the string or atom of the userName.

Lists these items which were added by the user to the list.
Example:

`
150> auction_list:traded_by(bithalver).
------------ITEM FOUND-----------
Item_no:         77
Id:              4770539
Game id:         165023
Game name:       Concordia: Britannia & Germania
Owned by:        bithalver
Sold:            true
Condition:
Language:
Lang_dep:
Version:
Starting_bid:     â¬8
BIN:              â¬10
Actual bid:      10
Actual winner:   Laner
Pres. & Essen:    whole Thursday and Friday
Auction ends:     October 2nd, random time
------------ITEM END-----------
------------ITEM FOUND-----------
Item_no:         79
Id:              4770542
Game id:         134631
Game name:       Crazy Lab
Owned by:        bithalver
Sold:            false
Condition:
Language:
Lang_dep:
Version:
Starting_bid:     â¬5
BIN:              â¬8
Actual bid:      no_bid
Actual winner:
Pres. & Essen:    whole Thursday and Friday
Auction ends:     October 2nd, random time
------------ITEM END-----------

...

`
