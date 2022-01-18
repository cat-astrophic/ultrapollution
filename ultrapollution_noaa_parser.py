# This script parses noaa data and creates weather variables

# Importing required modules

import pandas as pd
import glob

# Declaring username + directory

username = ''
direc = 'C:/Users/' + username + '/Documents/Data/ultrapollution/noaa_data/'

# Creating a list of the files to parse

to_parse = glob.glob(direc + '*')

# Data storage

stations = []
lats = []
longs = []
dates = []
dewps = []
preps = []
temps = []

# Parsing the files

for f in to_parse:
    
    # Status update
    
    print('Parsing file ' + str(1+to_parse.index(f)) + ' of ' + str(len(to_parse)) + '.......')
    
    # Get the text
    
    tmp = open(f)
    dat = tmp.read()
    tmp.close()
    
    # Parse the text
    
    idx = dat.find('\\n')
    dat = dat[idx+3:]
    
    while len(dat) > 10:
        
        idx = dat.find(',')
        stations.append(dat[:idx-1])
        dat = dat[idx+1:]
        idx = dat.find(',')
        lats.append(dat[1:idx-1])
        dat = dat[idx+1:]
        idx = dat.find(',')
        longs.append(dat[1:idx-1])
        dat = dat[idx+1:]
        idx = dat.find(',')
        dat = dat[idx+1:]
        idx = dat.find(',')
        dates.append(dat[1:idx-1])
        dat = dat[idx+1:]
        idx = dat.find(',')
        
        if idx-1 > 1:
            
            dewps.append(dat[1:idx-1])
            
        else:
            
            dewps.append(None)
            
        dat = dat[idx+1:]
        idx = dat.find(',')
        
        if idx-1 > 1:
            
            preps.append(dat[1:idx-1])
            
        else:
            
            preps.append(None)
            
        dat = dat[idx+1:]
        idx = dat.find('\\n')
        
        if idx-1 > 1:
            
            temps.append(dat[1:idx-1])
            
        else:
            
            temps.append(None)
            
        idx = dat.find('\\n')
        dat = dat[idx+3:]

# Cleaning the data lists

def bad_spaces_go_bye_bye(x):
    
    if x != None:
        
        while x[0] == ' ':
            
            x = x[1:]
        
    return x

dewps_clean = [bad_spaces_go_bye_bye(x) for x in dewps]
preps_clean = [bad_spaces_go_bye_bye(x) for x in preps]
temps_clean = [bad_spaces_go_bye_bye(x) for x in temps]

# Make a dataframe

s = pd.Series(stations, name = 'Station')
d = pd.Series(dates, name = 'Date')
la = pd.Series(lats, name = 'Latitude')
lo = pd.Series(longs, name = 'Longitude')
p = pd.Series(preps_clean, name = 'Precipitation')
df = pd.concat([s,d,la,lo,p], axis = 1)

# Remove some bad entries

indices = [1 if str(df.Station[i])[:2] == 'US' else 0 for i in range(len(df))]
df = pd.concat([df, pd.Series(indices, name = 'Keep')], axis = 1)
df = df[df.Keep == 1].reset_index(drop = True)
df = df[list(df.columns)[:-1]]

# Save to file

df.to_csv(direc[:-10] + 'ultra_data/NOAA.csv', index = False)

