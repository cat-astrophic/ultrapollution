# Cleaning the USGS data and getting mean county level altitude data

# The USGS data (not provided due to size) can be downloaded at: 
# https://www.usgs.gov/core-science-systems/ngp/board-on-geographic-names/download-gnis-data
# The data file has been renamed usgs.txt for convenience

# Importing required modules

import pandas as pd
import numpy as np

# Defining the username + filepath where the data is

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/ultrapollution/'

# Reading in the USGS data

uggs = pd.read_csv(filepath + 'usgs.txt', sep = '|')

# Adding a state + county variable to the data set

NEW_VAR = [str(uggs.COUNTY_NAME[i]) + str(uggs.STATE_ALPHA[i]) for i in range(len(uggs))]
NEW_VAR = pd.Series(NEW_VAR, name = 'NEW_VAR')
uggs = pd.concat([uggs, NEW_VAR], axis = 1)

# Getting mean county level data

ids = list(uggs.NEW_VAR.unique()) # Unique counties list
counties_name = [] # Initializing counties list
counties_code = [] # Initializing counties list
states_name = [] # Initializing states list
states_code = [] # Initializing states list
alts = [] # Initializing altitudes list

for idx in ids:

    print(str(ids.index(idx)+1) + ' / ' + str(len(ids)))
    ugg = uggs[uggs.NEW_VAR == idx].reset_index(drop = True) # Subset for the county
    vals = list(ugg.ELEV_IN_FT) # Get the elevation data
    vals = [float(v) for v in vals if v > -100] # There are a few large negative values in the raw data
    
    if len(vals) > 0:
        
        counties_name.append(ugg.COUNTY_NAME[0])
        counties_code.append(ugg.COUNTY_NUMERIC[0])
        states_name.append(ugg.STATE_ALPHA[0])
        states_code.append(ugg.STATE_NUMERIC[0])
        alts.append(np.mean(vals))

# Create a dataframe

counties_name = pd.Series(counties_name, name = 'County')
counties_code = pd.Series(counties_code, name = 'County_Code')
states_name = pd.Series(states_name, name = 'State')
states_code = pd.Series(states_code, name = 'State_FIPS')
alts = pd.Series(alts, name = 'Altitude')
df = pd.concat([counties_name, counties_code, states_name, states_code, alts], axis = 1)

# Creating county level FIPS codes

def county_fips(st,co):
    
    try:
        
        if len(str(int(co))) == 1:
            
            fip = str(int(st)) + '00' + str(int(co))
            
        elif len(str(int(co))) == 2:
            
            fip = str(int(st)) + '0' + str(int(co))
            
        else:
            
            fip = str(int(st)) + str(int(co))
            
    except:
        
        fip = None
        
    return fip

fips = [county_fips(df.State_FIPS[i],df.County_Code[i]) for i in range(len(df))]

# Add fips to df

fips = pd.Series(fips, name = 'FIPS')
df = pd.concat([df, fips], axis = 1)

# Remove rows for null counties

nullspace = [i for i in range(len(df)) if pd.isnull(df.County[i]) == True]
df = df.drop(nullspace, axis = 0).reset_index(drop = True)

# Save df to file

df.to_csv(filepath + 'altitude_data.csv', index = False)

