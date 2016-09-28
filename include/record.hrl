-define(BGG_URL,"http://boardgamegeek.com").
-define(BGG_HOST_NAME,"www.boardgamegeek.com").
-define(BGG_RSS_FEED,"http://boardgamegeek.com/recentadditions/rss?domain=boardgame").
-define(BGG_GAME_URL,"http://boardgamegeek.com/xmlapi/boardgame/").
-define(HTTPS_PREFIX, "https://").
-define(FREQ, 300000).

-record(enclosure, {
	  url :: undefined | binary(),
	  length :: undefined | binary(),
	  type :: undefined | binary()
	 }).

-record(entries,
	{
	  author :: undefined | binary(),
	  duration :: undefined | binary(),
	  enclosure :: undefined | enclosure(),
	  id :: undefined | binary(),
	  image :: undefined | binary(),
	  link :: undefined | binary(),
	  subtitle :: undefined | binary(),
	  summary :: undefined | binary(),
	  title :: undefined | binary(),
	  updated :: undefined | binary(),
	  type :: undefined | atom()
	}).

-record(entry,
	{
	  author :: undefined | binary(),
	  duration :: undefined | binary(),
	  enclosure :: undefined | enclosure(),
	  id :: undefined | binary(),
	  image :: undefined | binary(),
	  link :: undefined | binary(),
	  subtitle :: undefined | binary(),
	  summary :: undefined | binary(),
	  title :: undefined | binary(),
	  updated :: undefined | binary()
	}).

-type enclosure() :: #enclosure{}.

-record(game,
	{
	  id ::undefined | string(),
	  name="",
	  family=[],
	  mechanics=[],
	  yearpublished,
	  minplayers="",
	  maxplayers="",
	  publishers="",
	  gamedesigners="",
	  categories="",
	  types="",
	  lang_dependence = "0",
	  price = "undefined",
	  updated = "0", %timestamp of the date last update has been done
	  artists=[]
	}).

-record(designer,
	{ name :: string(),
	  games,
	  notes
}).

-record(artist,
	{ name :: string(),
	  games,
	  notes
}).

-record(mechanics,
	{ name :: string(),
	  games,
	  notes
}).

-record(publisher,
	{ publisher_id :: string(),
	  name :: string(),
	  booth,
	  games,
	  notes
}).


-record(categories,
	{ name :: string(),
	  value,
	  games,
	  notes
}).

-record(families,
	{ name :: string(),
	  value,
	  games,
	  notes
}).

-record(booth, 
	{
	  key = [], %note: key is the URL coded version of publisher name
	  publisher = [],
	  id = "",
	  booth = "undefined"
	}).

-record(airtable_record, {id, fields, createdTime}).
-record(field,{name,value}).

-record(picture, {id,url}). 
-record(math_trade_item, {item_no,
	                  id,
			  title,
			  publisher,
			  language,
			  attendance,
			  min_player,
			  max_player,
			  interested="not decided",
			  condition}).
			  
-record(auction_item, {item_no,
		       id,
		       object_type,
		       sub_type,
		       object_id,
		       object_name,
		       username,
		       body,
		       sold=false,
		       condition="",
		       language="",
		       lang_dep="",
		       version="",
		       starting_bid,
		       bin=no_bin,
		       actual_bid,
		       actual_winner="",
		       presense_at_essen="",
		       auction_ends="",
		       comments=[]}).
		       
-record(comment, {connected_item_no,
		  connected_object_id,
		  username,
		  text,
		  bid}).
		       
		       
