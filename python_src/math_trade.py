import re
import gspread
from oauth2client.service_account import ServiceAccountCredentials
import riak
import urllib
import subprocess
import shlex

#trick?
import sys
reload(sys)
sys.setdefaultencoding("utf-8")

myClient=riak.RiakClient(pb_port=8087)  # protocol can be 'pbc'/'http'
mtiBucket = myClient.bucket('math_trade')

def insert_data(key,row):
    if (mtiBucket.get(key).data != None):
        Data=mtiBucket.get(key).data
        insert_to_sheet(Data,row)
        return row+1
    else:
        return row
                
def insert_to_sheet(Data, row):
    i=0
    try:
        wks_output.update_cell(row,1,Data['item_no'])
        wks_output.update_cell(row,2,Data['id'])
        wks_output.update_cell(row,3,Data['title'])
        wks_output.update_cell(row,4,Data['publisher'].replace('Publisher:','',1).replace('[b][/b]','',1))
        wks_output.update_cell(row,5,Data['language'].replace('Language:','',1).replace('[b][/b]','',1))
        wks_output.update_cell(row,6,Data['min_player'])
        wks_output.update_cell(row,7,Data['attendance'].replace("Attendance at SPIEL'16:",'',1).replace('[b][/b]','',1))
        wks_output.update_cell(row,8,Data['condition'].replace('Condition:','',1).replace('[b][/b]','',1))
        wks_output.update_cell(row,9,Data['interested'])

    except gspread.exceptions.HTTPError:
        print " wks_output.update_cell resulted in error try again"
        i+=1
        if (i<3):
            insert_to_sheet(Data, row)
                               
# main

              
scope = ['https://spreadsheets.google.com/feeds']
credentials = ServiceAccountCredentials.from_json_keyfile_name('GetBGGInfo-a1f867326e07.json', scope)
gc = gspread.authorize(credentials)
sh = gc.open("Essen 2016")
wks_input = sh.sheet1
sheetName="no-shipping-leftovers-math-trade"
try:
    sh.add_worksheet(title=sheetName,rows="2500", cols="25")
except gspread.exceptions.RequestError:
    print sheetName, " sheet already exist"
wks_output=sh.worksheet(sheetName)
print "output worksheet title=",wks_output.title
Fields=["Number","Id","Title","Publisher", "Language", "Min_players","Attendance","Condition","Interested"]
col=1
for field in Fields:
    wks_output.update_cell(1,col,field)
    col+=1
row=1
cont = 1
value='1'
values_list = wks_output.col_values(9)
for value in values_list:
    if (value!=''):
        row+=1

Keys=mtiBucket.get_keys()
IntKeys=map(int,Keys)
IntKeys.sort()

cell=wks_output.cell(row-1,1)
value=cell.value
print value

#index=0

#if type(value) is int:
#    index=IntKeys.index(int(value)) + 1

index=IntKeys.index(int(value)) + 1
print index

del IntKeys[:index]
for key in IntKeys:
    print key
    new_row=insert_data(str(key),row)
    row=new_row
