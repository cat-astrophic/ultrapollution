# This creates urls for the NOAA API

# Importing required modules

import pandas as pd
from geopy.distance import geodesic
import requests

# Define username + filepath

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/'

# Read in a csv

print('Reading in data.......')
sites = pd.read_csv(filepath + 'noaa_sites.csv')

# Subset for US sites

sites = sites[sites.LATITUDE > 25]
sites = sites[sites.LATITUDE < 49]
sites = sites[sites.LONGITUDE < -66]
sites = sites[sites.LONGITUDE > -125].reset_index(drop = True)

# Read in the ultradata file and subset for unique events

ultra = pd.read_csv(filepath + 'ultrapollution/ultra_data/ultradata.csv')
ultra = ultra[['RACE_City', 'RACE_State']].reset_index(drop = True)
ultra = ultra.drop_duplicates().reset_index(drop = True)

# Reading in and prepping data for converting cities to counties

cases = pd.read_csv(filepath + 'ultrapollution/ultra_data/time_series_covid19_confirmed_US.csv') # Contains lattitude and longitude
ccmap = pd.read_csv(filepath + 'ultrapollution/ultra_data/ccmap.csv', sep = '|') # The city to county map from before
ccmap = ccmap.replace(to_replace = 'Washington, D.C.', value = 'District of Columbia') # Update DC naming convention
ccmap.City = ccmap.City.str.lower()
ccmap.County = ccmap.County.str.lower()
cases.Admin2 = cases.Admin2.str.lower()

# Function to go from city to coordinates

def city_to_county(inp):
    
    city = inp.RACE_City.lower()
    state = inp.RACE_State.upper().strip('"')
    sx = list(ccmap['State short']).index(state)
    st = ccmap['State full'][sx]
    
    try:
        
        cc = ccmap[ccmap['City'] == city]
        cc = cc[cc['State short'] == state]
        county = cc.iloc[0]['County']
        
    except:
        
        county = 'NOPE'
    
    if county != 'NOPE':
        
        if (county[0:5] == 'saint') and (county != 'saint marys'):
            
            back = county[5:]
            county = 'st.' + back
        
        elif county == 'virginia beach city':
            
            county = 'virginia beach'
            
        elif county ==  'alexandria city':
            
            county = 'alexandria'
            
        elif county == 'norfolk city':
            
            county = 'norfolk'
            
        elif county == 'fredericksburg city':
            
            county = 'fredericksburg'
            
        elif county == 'chesapeake city':
            
            county = 'chesapeake'
        
        elif county == 'lexington city':
            
            county = 'lexington'
            
        elif county == 'falls church city':
            
            county = 'falls church'
            
        elif county == 'staunton city':
            
            county = 'staunton'
            
        elif county == 'la porte':
            
            county = 'laporte'
            
        elif county == 'suffolk city':
            
            county = 'suffolk'
            
        elif county == 'newport news city':
            
            county = 'newport news'
            
        elif county == 'hampton city':
            
            county = 'hampton'
        
        elif county == 'manassas city':
            
            county = 'manassas'
            
        elif county == 'harrisonburg city':
            
            county = 'harrisonburg'
            
        elif county == 'prince georges':
            
            county = "prince george's"
            
        elif county == 'la salle':
            
            county = 'lasalle'
            
        elif county == 'saint marys':
            
            county = "st. mary's"
            
        elif county == 'lynchburg city':
            
            county = 'lynchburg'
            
        elif county == 'portsmouth city':
            
            county = 'portsmouth'
            
        elif county == 'poquoson city':
            
            county = 'poquoson'
            
        elif county == 'queen annes':
            
            county = "queen anne's"
            
        elif county == 'matanuska susitna':
            
            county = 'matanuska-susitna'
            
        elif county == 'st joseph':
            
            county = 'st. joseph'
            
        elif county == 'de kalb':
            
            county = 'dekalb'
            
        elif county == 'waynesboro city':
            
            county = 'waynesboro'
            
        elif county == 'winchester city':
            
            county = 'winchester'
            
        elif county == 'martinsville city':
            
            county = 'martinsville'
            
        elif county == 'danville city':
            
            county = 'danville'
            
        elif county == 'bristol city':
            
            county = 'bristol'
            
        elif county == 'de witt':
            
            county = 'dewitt'
            
        elif county == 'galax city':
            
            county = 'galax'
            
        elif county == 'colonial heights city':
            
            county = 'colonial heights'
            
    tmp = cases[cases.Province_State == st]
    tmp = tmp[tmp.Admin2 == county]
    
    if len(tmp) > 0:
        
        lat = tmp.iloc[0]['Lat']
        long = tmp.iloc[0]['Long_']
        coord = [lat,long]
        
    else:
        
        coord = [None,None]
    
    return coord

# Get a unique list of lats + longs

print('Generating race coordinates.......')
coordinates = [city_to_county(ultra.iloc[x]) for x in range(len(ultra))]
coordinates = [c for c in coordinates if c != [None,None]]

# Get nearest site for each coordinate

print('Identifying nearest NOAA statiions.......')
station_ids = []

for c in coordinates:
    
    tmp = sites[sites.LATITUDE > c[0] - .1]
    tmp = tmp[tmp.LATITUDE < c[0] + .1]
    tmp = tmp[tmp.LONGITUDE > c[1] - .1]
    tmp = tmp[tmp.LONGITUDE < c[1] + .1].reset_index(drop = True)
    
    if len(tmp) > 0:
        
        dists = []
        
        for t in range(len(tmp)):
            
            dists.append(geodesic(c,[tmp.LATITUDE[t],tmp.LONGITUDE[t]]))
            
        idx = dists.index(min(dists))
        station_ids.append(tmp.STATION[idx])

station_ids = list(set(station_ids))

# Making the urls

print('Requesting data through the NOAA API.......')
a = 'https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries&dataTypes=PRCP,TEMP,DEWP&startDate='
b = '&endDate='
c = '&stations='
d = '&boundingBox=49,-125,25,-66&includeStationLocation=1'

days = [0,31,28,31,30,31,30,31,31,30,31,30,31]
starts = []
ends = []

for y in range(2010,2020):
    
    for m in range(1,13):
        
        x = str(days[m])
        
        if m < 10:
            
            m = '0' + str(m)
            
        s = str(y) + '-' + str(m) + '-01'
        e = str(y) + '-' + str(m) + '-' + x
        starts.append(s)
        ends.append(e)

# Getting data

for i in station_ids:
    
    print('Station ' + str(station_ids.index(i)+1) + ' of ' + str(len(station_ids)) + '.......')
    
    for s in starts:
        
        print('Month-Year :: ' + s[:7] + '.......')
        string = a + s + b + ends[starts.index(s)] + c + str(i) + d        
        req = requests.get(string)
        dat = req.content
        
        if len(dat) > 73:
            
            with open(filepath + 'ultrapollution/noaa_data/' + str(i) + '_' + s[:7] + '.txt', 'w') as f:
                
                f.write(str(dat))
                f.close()

