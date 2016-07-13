


# Connect to the client store the 1st objects: #


## Python Client: ##

`import riak
myClient=riak.RiakClient(pb_port=8087)  # protocol can be 'pbc'/'http'
myBucket = myClient.bucket('bgg')

myClient.get_buckets()
[<RiakBucket 'bgg'>]

>>> val1 = 1
>>> key1 = myBucket.new('one', data=val1)
>>> key1.store()
<riak.riak_object.RiakObject object at 0x1035fdd50>
>>> val2 = "two"
>>> key2 = myBucket.new('two', data=val2)
>>> key2.store()
<riak.riak_object.RiakObject object at 0x1035fdd90>
>>> val3 = {"myValue": 3}
>>> key3 = myBucket.new('three', data=val3)
>>> key3.store()
<riak.riak_object.RiakObject object at 0x1035fddd0>
>>> fetched1 = myBucket.get('one')
>>> fetched1.data
1

myBucket.get_keys()
['three', 'two', 'one']
`

## Erlang Client: ##

`{ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087),
MyBucket = <<"bgg">>,
Obj1=riakc_obj:new(MyBucket,<<"one">>,1),
    riakc_pb_socket:put(Pid,Obj1),
    {ok, Fetched1} = riakc_pb_socket:get(Pid, MyBucket, "one"),
		io:format("Read from Riak =~p~n",[Fetched1]),
    riakc_pb_socket:stop(Pid).

riakc_pb_socket:list_keys(Pid, <<"bgg">>).
{ok,[<<"one">>,<<"three">>,<<"two">>]}

riakc_pb_socket:list_buckets(Pid).
{ok,[<<"bgg">>]}

`
# Try to fetch objects form with the other client: #

## Check buckets with Python client ##

`myClient.get_buckets()
[<RiakBucket 'bgg'>]
`

## create a new bucket with the Python client and check it can be listed: ##

`
>>> myClient.get_buckets()
[<RiakBucket 'bgg'>]
>>> newBucket=myClient.bucket('new')
>>> myClient.get_buckets()
[<RiakBucket 'bgg'>]
`
Note new bucket is not listed until at least 1 object is stored
`
>>> val4=['vmi','mas']
>>> key4=newBucket.new('my_new',data=val4)
>>> key4.store()
<riak.riak_object.RiakObject object at 0x10361a450>
>>> myClient.get_buckets()
[<RiakBucket 'new'>, <RiakBucket 'bgg'>]
`

## Check buckets with curl ##

`curl -i http://localhost:8098/buckets?buckets=true`
Response:

`HTTP/1.1 200 OK
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.10.8 (that head fake, tho)
Date: Tue, 12 Jul 2016 16:59:59 GMT
Content-Type: application/json
Content-Length: 25

{"buckets":["new","bgg"]}
`

## Check buckets with Erlang Client ##

`riakc_pb_socket:list_buckets(Pid).
{ok,[<<"new">>,<<"bgg">>]}

riakc_pb_socket:list_keys(Pid,<<"new">>).
{ok,[<<"my_new">>]}
`

# Check content types #

1. store object with the Erlang client 
`
> Obj2=riakc_obj:new(NewBucket,<<"123">>,{recordtype,1,2,[4,5,6],"valami"}).
> riakc_pb_socket:put(Pid,Obj2).
{riakc_obj,<<"new">>,<<"123">>,undefined,[],undefined,
           {recordtype,1,2,[4,5,6],"valami"}}
ok.
> Obj3=riakc_obj:new(NewBucket,<<"456">>,<<"textbe atkuldve">>). 
{riakc_obj,<<"new">>,<<"456">>,undefined,[],undefined,
	       <<"textbe atkuldve">>}
> riakc_pb_socket:put(Pid,Obj3).
ok.

`
2. fetch the objects from Python:
`
>>> newBucket.get('123')
<riak.riak_object.RiakObject object at 0x10361a610>
>>> newBucket.get('123').content_type
'application/x-erlang-binary'
>>> newBucket.get('123').content_encoding
>>> newBucket.get('123').encoded_data
'\x83h\x05d\x00\nrecordtypea\x01a\x02k\x00\x03\x04\x05\x06k\x00\x06valami'

>>> newBucket.get_keys()
[u'123', u'my_new', u'456']
>>> newBucket.get('456').data
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/riak/riak_object.py", line 27, in _getter
    return getattr(self.siblings[0], name)
  File "/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/riak/content.py", line 47, in _get_data
    self._data = self._deserialize(self._encoded_data)
  File "/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/riak/content.py", line 98, in _deserialize
    format(self.content_type))
TypeError: No decoder for content type "application/octet-stream"

`
Note: Erlang client makes a decision regarding teh content type if it is not specified by us.
Text -> 'application/octet-stream'

Complex type -> 'application/x-erlang-binary'

3. define "text/plain"  content type when creating the object in Erlang and check the content from Python:

create object:
`
> Obj4=riakc_obj:new(NewBucket,<<"4567">>,<<"textbe atkuldve">>,<<"text/plain">>). 
	{riakc_obj,<<"new">>,<<"4567">>,undefined,[],
           {dict,1,16,16,8,80,48,
                 {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],...},
                 {{[],[],[],[],[],[],[],[],[],[],[[<<...>>|...]],[],[],...}}},
           <<"textbe atkuldve">>}
> riakc_pb_socket:put(Pid,Obj4).                                                                             

`
fetch object:
`
newBucket_keys()
[u'123', u'my_new', u'456', u'4567']
>>> newBucket.get('4567').data
'textbe atkuldve'
`
4. store object in Python as JSON and fetch from the Erlang client:

Create and store the object:
`
>>> newUser=newBucket.new('johndoe', data={
...     'first_name': 'John',
...     'last_name': 'Doe',
...     'gender': 'm',
...     'website': 'http://example.com/',
...     'is_active': True,
... })

>>> newUser.store()
<riak.riak_object.RiakObject object at 0x10361af50>
`

Fetch the object in Erlang:
`
> {ok,JohnObj}=riakc_pb_socket:get(Pid, NewBucket, <<"johndoe">>).
> JohnObj.
{riakc_obj,<<"new">>,<<"johndoe">>,
           <<107,206,97,96,96,96,204,96,202,5,82,60,202,156,255,126,
             78,185,183,206,159,129,65,93,48,...>>,
           [{{dict,3,16,16,8,80,48,
                   {[],[],[],[],[],[],[],[],[],[],[],[],[],[],...},
                   {{[],[],[],[],[],[],[],[],[],[],[[...]|...],[],...}}},
             <<"{\"website\": \"http://example.com/\", \"gender\": \"m\", \"first_name\": \"John\", \"last_name\":"...>>}],
           undefined,undefined}
		   
> riakc_obj:get_value(JohnObj).
<<"{\"website\": \"http://example.com/\", \"gender\": \"m\", \"first_name\": \"John\", \"last_name\": \"Doe\", \"is_active\": true}">>

> riakc_obj:get_contents(JohnObj).
[{{dict,3,16,16,8,80,48,
        {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
        {{[],[],[],[],[],[],[],[],[],[],
          [[<<"content-type">>,97,112,112,108|...],
           [<<"X-Riak-VTag">>,54,111,109|...]],
          [],[],
          [[<<"X-Ri"...>>|{...}]],
          [],[]}}},
  <<"{\"website\": \"http://example.com/\", \"gender\": \"m\", \"first_name\": \"John\", \"last_name\": \"Doe\", \"is_active\":"...>>}]
  
 >  riakc_obj:get_content_type(JohnObj).
"application/json"

`

5. store object in Erlang as JSON and fetch from the Python client:

Create and store the object:
`
> Obj6=riakc_obj:new(NewBucket,<<"my_key1>>, <<"{\"key\":\"val\"}">>, <<"application/json">>).
{riakc_obj,<<"new">>,<<"my_key">>,undefined,[],
           {dict,1,16,16,8,80,48,
                 {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],...},
                 {{[],[],[],[],[],[],[],[],[],[],[[<<...>>|...]],[],[],...}}},
           <<"{\"key\":\"val\"}">>}
> riakc_pb_socket:put(Pid,Obj6).
`
Fetch in Python:
`
>>> newBucket.get_keys()
[u'my_new', u'456', u'johndoe', u'4567', u'my_key', u'123', u'my_key1']
>>> newBucket.get('my_key')
<riak.riak_object.RiakObject object at 0x10361a410>
>>> newBucket.get('my_key').data
{u'key': u'val'}
>>> newBucket.get('my_key1').data['key']
u'val'
>>> print unicode(newBucket.get('my_key1').data['key'])
val
`

# JSON content_type handling #

1. Create an Erlang record, convert it to JSON, and store to Riak 

`ejson` should be used; an example has been created in `game.erl`

use `game:start().`

2. fetch it's content from Python: 
`
>>> myBucket.get_keys()
['three', 'one', '18602', 'two']
>>> myBucket.get('18602')
<riak.riak_object.RiakObject object at 0x103641310>
>>> C=myBucket.get('18602')
>>> C.content_type
'application/json'
>>> C.content_encoding
>>> C.encoded_data
'{"id":"18602","name":"Caylus","family":"Caylus , Country: France , Ystari originals","mechanics":"Worker Placement","yearpublished":"2005","minplayers":"2","maxplayers":"5","publishers":"Ystari Games , Brain Games , Edge Entertainment , Esdevium , hobbity.eu , Hobby Japan , HUCH! & friends , Korea Boardgames co., Ltd. , Lautapelit.fi , Quined White Goblin Games , Rio Grande Games , uplay.it edizioni , What\'s Your Game? , White Goblin Games","gamedesigners":"William Attia","categories":"City Building , Economic , Medieval","types":"Strategy Games","lang_dependence":"1","price":"undefined"}'
>>> C.data
{u'mechanics': u'Worker Placement', u'publishers': u"Ystari Games , Brain Games , Edge Entertainment , Esdevium , hobbity.eu , Hobby Japan , HUCH! & friends , Korea Boardgames co., Ltd. , Lautapelit.fi , Quined White Goblin Games , Rio Grande Games , uplay.it edizioni , What's Your Game? , White Goblin Games", u'name': u'Caylus', u'family': u'Caylus , Country: France , Ystari originals', u'minplayers': u'2', u'yearpublished': u'2005', u'maxplayers': u'5', u'gamedesigners': u'William Attia', u'price': u'undefined', u'lang_dependence': u'1', u'types': u'Strategy Games', u'id': u'18602', u'categories': u'City Building , Economic , Medieval'}
>>>
`

