-define(BGG_URL,"http://boardgamegeek.com").
-define(BGG_HOST_NAME,"www.boardgamegeek.com").
-define(BGG_RSS_FEED,"http://boardgamegeek.com/recentadditions/rss?domain=boardgame").
-define(BGG_GAME_URL,"http://boardgamegeek.com/xmlapi/boardgame/").
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
	  publishers=[],
	  gamedesigners=[],
	  categories=[],
	  types=[],
	  lang_dependence = "0",
	  price = "undefined"
	}).

-record(booth, 
	{
	  publisher = [],
	  id = "",
	  booth = "undefined"
	}).
