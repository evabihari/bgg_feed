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
   key! - not working special chars remained in teh string + url
   encoding resulted different string in Python and Erlang
   2. Remove non_asscii characters from the name when creating the key
      -> this is now working! Problem can be if a Publisher name will
      contain only non-ASCII chars (ex. Japaneese nam)
8. try to find a solution making sure google sheet will be user friendly (format, etc)



Other
====
Airtable (https://airtable.com) API support added.

Basic usage:
1. initialize:

`hackney:start().
 AT=airtable:init(Base_id, API_key)`
 
 where `Base_id` is the ID connected to teh Airtable table you want to
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
(includingg adding attachments to teh games and creating links" 
			  
