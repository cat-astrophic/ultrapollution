# This script creates a daily mean pollution score for each pollutant in each county

# Importing required modules

import pandas as pd

# Declaring username + filepath

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/ultrapollution/'

# Reading in the data sets

pm = pd.read_csv(filepath + 'epa_aqs_data_pm.csv')
pm10 = pd.read_csv(filepath + 'epa_aqs_data_pm10.csv')
co = pd.read_csv(filepath + 'epa_aqs_data_co.csv')
no2 = pd.read_csv(filepath + 'epa_aqs_data_no2.csv')
pb = pd.read_csv(filepath + 'epa_aqs_data_pb.csv')
o3 = pd.read_csv(filepath + 'epa_aqs_data_ozone.csv')
#so2 = pd.read_csv(filepath + 'epa_aqs_data_so2.csv')

# A FIPS-creating function

def F(s,c):
    
    s = str(s)
    c = str(c)
    
    if len(s) == 1:
        
        s = '0' + s
        
    if len(c) == 1:
        
        c = '00' + c
        
    elif len(c) == 2:
        
        c = '0' + c
        
    fip = s + c
    
    return fip

# Creating the county-pollutant level data sets

sets = [pm, pm10, co, no2, pb, o3]#, so2]
names = ['pm', 'pm10', 'co', 'no2', 'pb', 'ozone', 'so2']

for p in sets:
    
    dates = []
    states = []
    counties = []
    fips = []
    values = []
    state_list = list(p.State.unique())
    
    for s in state_list:
        
        tmp = p[p.State == s]
        county_list = list(tmp.County.unique())
        
        for c in county_list:
            
            print('Pollutant ' + str(sets.index(p)+1) + ' of ' + str(len(sets)) + ' :: State ' + str(state_list.index(s)+1) + ' of 50 :: County ' + str(county_list.index(c)+1) + ' of ' + str(len(county_list)) + '.......') # Visualize progress
            tmpc = tmp[tmp.County == c]
            tmp_dates = list(tmpc.Date.unique())
            
            for d in tmp_dates:
                
                tmpd = tmpc[tmpc.Date == d]
                dates.append(d)
                states.append(s)
                fips.append(F(s,c))
                counties.append(c)
                values.append(tmpd.Value.mean())
                
    dates = pd.Series(dates, name = 'Date')
    states = pd.Series(states, name = 'State')
    counties = pd.Series(counties, name = 'County')
    fips = pd.Series(fips, name = 'FIPS')
    values = pd.Series(values, name = 'Value')
    df = pd.concat([dates, states, counties, fips, values], axis = 1)
    df.to_csv(filepath + names[sets.index(p)] + '_data.csv')

