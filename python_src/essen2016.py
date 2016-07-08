import re
import gspread
from oauth2client.service_account import ServiceAccountCredentials
from boardgamegeek import BoardGameGeek
bgg = BoardGameGeek()

def convert_rank(DictList):
    #dict = [{u'friendlyname': 'Board Game Rank', u'name': 'boardgame', u'value': None}]
    result=""
    for dict in DictList:
        if (dict['value'] != None):
            s=dict['friendlyname'] + ":" + dict['name'] + "=" + str(dict['value'])
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
    
def update_games_info(row,game):
        wks_output.update_cell(row,1,game.name)
        wks_output.update_cell(row,2,game.id)
        wks_output.update_cell(row,3,','.join(game.publishers))
        wks_output.update_cell(row,4,','.join(game.designers))
        wks_output.update_cell(row,5,game.year)
        wks_output.update_cell(row,6,','.join(game.artists))
        wks_output.update_cell(row,7,convert_rank(game.ranks))
        wks_output.update_cell(row,8,game.min_players)
        wks_output.update_cell(row,9,game.max_players)
        wks_output.update_cell(row,10,game.min_age)
        wks_output.update_cell(row,11,','.join(game.mechanics))
        wks_output.update_cell(row,12,game.playing_time)
        wks_output.update_cell(row,13,','.join(game.families))
        wks_output.update_cell(row,14,game.rating_average)
        wks_output.update_cell(row,15,','.join(game.alternative_names))
        wks_output.update_cell(row,16,expand(game.expands))

        
# main

scope = ['https://spreadsheets.google.com/feeds']
credentials = ServiceAccountCredentials.from_json_keyfile_name('GetBGGInfo-a1f867326e07.json', scope)
gc = gspread.authorize(credentials)
sh = gc.open("Essen 2016")
wks_input = sh.sheet1
sheetName="data_form_BGG"
try:
    sh.add_worksheet(title=sheetName,rows="100", cols="20")
except AttributeError:
    print sheetName, " sheet already exist"
wks_output=sh.worksheet(sheetName)
print "output worksheet title=",wks_output.title
Fields=["Name","Id","Publisher","Designers","Year","Artists", "Ranks", "Min_players",
        "Max_players","Min_age","Mechanics","Plying_time","Families",
        "Rating_average","Alternative_names","Expands"]
col=1
for field in Fields:
    wks_output.update_cell(1,col,field)
    col+=1
row=2
end_value='#end'
while (wks_input.cell(row,1).value!=end_value):
    cell=wks_input.cell(row,1)
    value=cell.value
    input_value=cell.input_value
    if ((value!=input_value) & (value!='')):
        String = re.search("(?P<url>https?://[^\s]+)/",input_value).group("url")
        URL = String.split('"')[0]
        Id=[s for s in URL.split("/") if s.isdigit()][0]
        print "URL=", URL, "Id=", Id, "Value=", value, " input_value=",input_value 
        game = bgg.game(game_id=Id)
        print "Name=", game.name
        print "id=", game.id
        for n in game.alternative_names: print n.encode("utf-8")
        update_games_info(row,game)
    else:
        wks_output.update_cell(row,1,input_value)
    row+=1
print "End"


