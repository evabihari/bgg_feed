import re
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from boardgamegeek import BoardGameGeek
import riak
import urllib
import subprocess
import shlex
#trick?
import sys
reload(sys)
sys.setdefaultencoding("utf-8")
#trick?

myClient=riak.RiakClient(pb_port=8087)  # protocol can be 'pbc'/'http'
bggBucket = myClient.bucket('bgg')
boothBucket = myClient.bucket('booth')
bgg = BoardGameGeek()

def update_game_in_riak(game):
    Id=str(game.id)
    Obj=bggBucket.get(Id)
    if (Obj.data==None):
        newGame=bggBucket.new(Id, data={
        'id': Id,
        'name': game.name,
        'family': ','.join(game.families),
        'mechanics': ','.join(game.mechanics),
        'yearpublished':str(game.year),
        'minplayers':str(game.min_players),
        'maxplayers':str(game.max_players),
        'publishers':','.join(game.publishers),
        'gamedesigners':','.join(game.designers),
        'categories':','.join(game.categories),
        'types':'',
        'lang_dependence':'0',
        'price':  'undefined'
        })
        newGame.store()
    else:
        Obj.data['id']=Id,
        Obj.data['name']= game.name,
        Obj.data['family']= ','.join(game.families),
        Obj.data['mechanics']= ','.join(game.mechanics),
        Obj.data['yearpublished']=str(game.year),
        Obj.data['minplayers']=str(game.min_players),
        Obj.data['maxplayers']=str(game.max_players),
        Obj.data['publishers']=','.join(game.publishers),
        Obj.data['gamedesigners']=','.join(game.designers),
        Obj.data['categories']=','.join(game.categories),        
        Obj.store()
    
def convert_rank(DictList):
    #dict = [{u'friendlyname': 'Board Game Rank',
    #         u'name': 'boardgame',
    #         u'value': None}]
    result=""
    for dict in DictList:
        if (dict['value'] != None):
#            s=dict['friendlyname'] + ":" + dict['name'] + "=" + str(dict['value'])
            s=dict['friendlyname'] + ":" + str(dict['value'])
            result+=s
        result+=" "
    return result

def expand(Things):
    result=''
    for thing in Things:
        g=bgg.game(game_id=thing.id)
        if (g !=None):
            s=g.name
            if (result==''):
                result=s
            else:
                result=result+","+s
    return result

def find_price(Id):
    print "Id=",str(Id)
    Data=bggBucket.get(str(Id)).data
    if (Data != None):
        return Data['price']
    else:
        return "not known yet"

def remove_non_ascii(text):
    return ''.join(i for i in text if ord(i)<128)

def find_booth(Publishers):
    for P in Publishers:
        key=remove_non_ascii(P)
        print "Publisher key=", key
        try:
            if (boothBucket.get(key).data != None):
                Data = boothBucket.get(key).data
                print Data
                return Data['booth']
 
            else:
                return "not known yet"
        except TypeError:            
            print TypeError
            return "publisher name in unicode"
    
def update_games_info(row,game):
    i=0
    Data=[game.name,
          game.id,                    
          ','.join(game.publishers),  
          ','.join(game.designers),
          game.year,
          ','.join(game.artists),
          convert_rank(game.ranks),
          game.min_players,
          game.max_players,
          game.min_age,
          ','.join(game.mechanics),
          game.playing_time,
          ','.join(game.families),
          game.rating_average,
          ','.join(game.alternative_names),
          expand(game.expands),
          find_price(game.id),
          find_booth(game.publishers)]
    rangeString=convert(wks_output,row,2,18)
    cell_list=wks_output.range(rangeString)
    j=1
    for cell in cell_list:
        cell.value=Data[j]
        j+=1
    try:           
        wks_output.update_cells(cell_list)
        update_game_in_riak(game)
    except gspread.exceptions.HTTPError:
        print " wks_output.update_cell resulted in error try again"
        i+=1
        if (i<3):
            update_games_info(row,game)
    

def convert(sheet,row,startPos,stopPos):
    startId=sheet.get_addr_int(row,startPos)
    stopId=sheet.get_addr_int(row,stopPos)
    return startId+':'+stopId

def copy_row(input,output,input_row,output_row):
        try:
                start_cell=input.get_addr_int(input_row,1)
                end_cell=input.get_addr_int(input_row,input.col_count)
                range=start_cell+':'+end_cell
                cell_list = input.range(range)
                max_col=output.col_count
                start_new_cell=output.get_addr_int(output_row,1)
                end_new_cell=output.get_addr_int(output_row,output.col_count)
                new_range=start_new_cell+':'+end_new_cell       
                #copy cells to the output_sheet
                new_cell_list= output.range(new_range)
                i=0
                for cell in cell_list:
                    if (i<max_col):
                        new_cell_list[i].value=cell_list[i].value
                    i+=1
                output.update_cells(new_cell_list)
        except gspread.exceptions.HTTPError:
                print " output.update_cells resulted in error try again"
                copy_row(input,output,input_row,output_row)

# main

scope = ['https://spreadsheets.google.com/feeds']
credentials = ServiceAccountCredentials.from_json_keyfile_name('GetBGGInfo-a1f867326e07.json', scope)
gc = gspread.authorize(credentials)
sh = gc.open("Essen 2016")
wks_input = sh.sheet1
sheetName="data_from_BGG"
try:
    sh.add_worksheet(title=sheetName,rows="100", cols="25")
except gspread.exceptions.RequestError:
    print sheetName, " sheet already exist"
wks_output=sh.worksheet(sheetName)
print "output worksheet title=",wks_output.title
Fields=["Name","Id","Publisher","Designers","Year","Artists", "Ranks", "Min_players",
        "Max_players","Min_age","Mechanics","Plying_time","Families",
        "Rating_average","Alternative_names","Expands","Price","Booth"]
col=1
for field in Fields:
    wks_output.update_cell(1,col,field)
    col+=1
input_row=2
output_row=2
end_value='#end'
while (wks_input.cell(input_row,1).value!=end_value):
    cell=wks_input.cell(input_row,1)
    value=cell.value
    input_value=cell.input_value
    if ((value!=input_value) & (value!='')):
        String = re.search("(?P<url>https?://[^\s]+)/",input_value).group("url")
        URL = String.split('"')[0]
        Id=[s for s in URL.split("/") if s.isdigit()][0]
    #    print "URL=", URL, "Id=", Id, "Value=", value, " input_value=",input_value 
        subprocess.call(["./at_escript.sh", Id])    
        game = bgg.game(game_id=Id)
     #   print "Name=", game.name
        print "id=", game.id
        for n in game.alternative_names: print n.encode("utf-8")
        wks_output.update_cell(output_row,1,input_value)
        update_games_info(output_row,game)
    else:
        copy_row(wks_input,wks_output,input_row,output_row)
    input_row+=1
    output_row+=1
print "End"


