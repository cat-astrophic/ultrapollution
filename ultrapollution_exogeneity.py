# This script creates a data set for running a diff-in-diff to test for the exogeneity of pollution in ultramarathon performance

# Importing required modules

import pandas as pd
import numpy as np

# Defining username + filepath

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/ultrapollution/'

# Reading in the data sets

ultra_data = pd.read_csv(filepath + 'ultra_data/ultradata.csv')
ccmap = pd.read_csv(filepath + 'ultra_data/ccmap.csv', sep = '|')
cases = pd.read_csv(filepath + 'ultra_data/time_series_covid19_confirmed_US.csv')
mhi = pd.read_csv(filepath + 'ultra_data/Median_Household_Income.csv')
pop = pd.read_csv(filepath + 'ultra_data/Population.csv')
pm = pd.read_csv(filepath + 'pm_data.csv')
pm10 = pd.read_csv(filepath + 'pm10_data.csv')
co = pd.read_csv(filepath + 'co_data.csv')
o3 = pd.read_csv(filepath + 'ozone_data.csv')
no2 = pd.read_csv(filepath + 'no2_data.csv')

# Cleaning the ccmap city to county conversion file and the cases file

ccmap = ccmap.replace(to_replace = 'Washington, D.C.', value = 'District of Columbia') # Update DC naming convention
ccmap.City = ccmap.City.str.lower()
ccmap.County = ccmap.County.str.lower()
cases.Admin2 = cases.Admin2.str.lower()

# Extracting races + race dates + race counts + cities + states from ultra_data

races = list(ultra_data.RACE_ID.unique())
days = [ultra_data[ultra_data.RACE_ID == r].median()['RACE_Date'] for r in races]
months = [ultra_data[ultra_data.RACE_ID == r].reset_index(drop = True)['RACE_Month'][0] for r in races]
years = [ultra_data[ultra_data.RACE_ID == r].median()['RACE_Year'] for r in races]
runners = [ultra_data[ultra_data.RACE_ID == r].median()['RACE_Finisher_Count'] for r in races]
cities = [ultra_data[ultra_data.RACE_ID == r].reset_index(drop = True)['RACE_City'][0] for r in races]
states = [ultra_data[ultra_data.RACE_ID == r].reset_index(drop = True)['RACE_State'][0] for r in races]

# Defining a helper function for assigning FIPS via ccmap and cases

def flipsadelphia(city, state):
    
    city = city.lower()
    state = state.upper().strip('"')
    sx = list(ccmap['State short']).index(state)
    st = ccmap['State full'][sx]
        
    try:
        
        cc = ccmap[ccmap['City'] == city]
        cc = cc[cc['State short'] == state]
        county = cc.iloc[0]['County'].lower()
        
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
            
        elif county == 'galax city':
            
            county = 'galax'
            
        elif county == 'colonial heights city':
            
            county = 'colonial heights'
            
        try:
            
            tmp = cases[cases['Admin2'] == county]
            tmp = tmp[tmp['Province_State'] == st]
            flips = int(tmp.iloc[0]['FIPS'])
            
        except:
            
            flips = None
            
    else:
        
        flips = None
        
    return flips

# Mapping cities to counties / FIPS

counties = [flipsadelphia(cities[i], states[i]) for i in range(len(states))]

# Matching

matches = []
cpop = []
mpop = []
cinc = []
minc = []

for c in counties:
    
    if c != None:
        
        try:
            
            year = int(years[counties.index(c)]) # get year of observation
            incs = mhi[mhi.year == year] # subset mhi for year
            refinc = incs[incs.countyid == c]['medianhouseholdincome'].mean() # get obs mhi
            incs = incs[incs['medianhouseholdincome'] > .95*refinc] # subset for within 5%
            incs = incs[incs['medianhouseholdincome'] < 1.05*refinc].reset_index(drop = True) # subset for within 5%
            pyear = 'pop' + str(year) # colname for pop + year
            pops = pop[['fips', 'stname', pyear]] # subset pop for year
            refpop = pops[pops.fips == c][pyear].mean() # get obs pop
            support = list(incs.countyid.unique()) # get potential matches from mhi
            support.remove(c) # drop c from support
            pops = pops[pops.fips.isin(support)] # subset pops for support
            pops = pops[pops[pyear] > .95*refpop] # subset for within 5%
            pops = pops[pops[pyear] < 1.05*refpop].reset_index(drop = True) # subset for within 5%
            cpop.append(refpop)
            cinc.append(refinc)
            
            if len(pops) > 0:
                
                diffs = [abs(refpop - pops[pyear][i]) for i in range(len(pops))]
                min_id = diffs.index(min(diffs))
                matches.append(pops.fips[min_id])
                mpop.append(pops[pyear][min_id])
                minc.append(incs[incs.countyid == pops.fips[min_id]].mean()['medianhouseholdincome'])
                
            else:
                
                matches.append(None)
                mpop.append(None)
                minc.append(None)
                
        except:
            
            matches.append(None)
            cpop.append(None)
            cinc.append(None)
            mpop.append(None)
            minc.append(None)
            
    else:
        
        matches.append(None)
        cpop.append(None)
        cinc.append(None)
        mpop.append(None)
        minc.append(None)

# Creating a pollution dataframe from matching

m_ids = [m for m in range(len(matches)) if matches[m] != None]
matches = [matches[m] for m in m_ids]
races = [races[m] for m in m_ids]
days = [days[m] for m in m_ids]
months = [months[m] for m in m_ids]
years = [years[m] for m in m_ids]
runners = [runners[m] for m in m_ids]
cities = [cities[m] for m in m_ids]
states = [states[m] for m in m_ids]
cpop = [cpop[m] for m in m_ids]
mpop = [mpop[m] for m in m_ids]
cinc = [cinc[m] for m in m_ids]
minc = [minc[m] for m in m_ids]

dpop = cpop + mpop
dinc = cinc + minc

FIPS = races + matches # FIPS list for df
treat = [1]*2828 + [0]*2828 # Indicator of race or no
count = runners + [0]*2828 # Finisher counts
state = [int(np.floor(f/1000)) for f in FIPS] # State FE

def date_fx(yr, mon, day):
    
    month_fx = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
    
    y = str(int(yr))
    m = str(month_fx.index(mon) + 1)
    d = str(int(day))
    
    if len(m) < 2:
        
        m = '0' + m
        
    if len(d) < 2:
        
        d = '0' + d
        
    out = int(y + m + d)
    
    return out

dates = [date_fx(years[i], months[i], days[i])  for i in range(len(years))] # Making dates to use to get the pollution data
dates = dates + dates

dpm = []
dpm10 = []
dco = []
do3 = []
dno2 = []

for i in range(len(dates)):
    
    tpm = pm[pm.Date == dates[i]]
    tpm10 = pm10[pm10.Date == dates[i]]
    tco = co[co.Date == dates[i]]
    to3 = o3[o3.Date == dates[i]]
    tno2 = no2[no2.Date == dates[i]]
    
    dpm.append(tpm[tpm.FIPS == FIPS[i]].mean()['Value'])
    dpm10.append(tpm10[tpm10.FIPS == FIPS[i]].mean()['Value'])
    dco.append(tco[tco.FIPS == FIPS[i]].mean()['Value'])
    do3.append(to3[to3.FIPS == FIPS[i]].mean()['Value'])
    dno2.append(tno2[tno2.FIPS == FIPS[i]].mean()['Value'])
    
FIPS = pd.Series(FIPS, name = 'FIPS')
state = pd.Series(state, name = 'State')
dates = pd.Series(dates, name = 'Date')
treat = pd.Series(treat, name = 'Event')
count = pd.Series(count, name = 'Count')
dpop = pd.Series(dpop, name = 'Population')
dinc = pd.Series(dinc, name = 'Income')

dpm = pd.Series(dpm, name = 'PM2.5')
dpm10 = pd.Series(dpm10, name = 'PM10')
do3 = pd.Series(do3, name = 'O3')
dno2 = pd.Series(dno2, name = 'NO2')
dco = pd.Series(dco, name = 'CO')

output_df = pd.concat([FIPS, state, dates, treat, count, dpop, dinc, dpm, dpm10, do3, dno2, dco], axis = 1)

# Write output_df to file

output_df.to_csv(filepath + 'ultra_data/exo_match.csv', index = False)

