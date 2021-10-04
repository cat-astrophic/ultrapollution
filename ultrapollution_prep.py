# This script transforms variables into a usable format for regressing

# Importing required modules

import pandas as pd
from geopy.distance import geodesic

# Specifying the path to the data -- update this accordingly!

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/ultrapollution/ultra_data/'

# Reading in the raw ultramarathon results data

ultradata = pd.read_csv(filepath + 'raw_results_data.csv')

# Subsetting for US data only

ultradata = ultradata[ultradata.Country == 'USA'].reset_index(drop = True)
ultradata = ultradata[ultradata.RACE_Nation == 'US'].reset_index(drop = True)
ultradata = ultradata[ultradata.RACE_State != 'PR'].reset_index(drop = True)
ultradata = ultradata[ultradata.State != 'a'].reset_index(drop = True)

# Creating a fixed effect for in-state runners

instate = [1 if ultradata['RACE_State'][i] == ultradata['State'][i] else 0 for i in range(len(ultradata['RACE_State']))]
instate = pd.Series(instate, name = 'In_State')
ultradata = pd.concat([ultradata, instate], axis = 1)

# Determining the distance between runner and event

cases = pd.read_csv(filepath + 'time_series_covid19_confirmed_US.csv') # Contains lattitude and longitude
ccmap = pd.read_csv(filepath + 'ccmap.csv', sep = '|') # The city to county map from before
ccmap = ccmap.replace(to_replace = 'Washington, D.C.', value = 'District of Columbia') # Update DC naming convention
ccmap.City = ccmap.City.str.lower()
ccmap.County = ccmap.County.str.lower()
cases.Admin2 = cases.Admin2.str.lower()

# The functions for mapping city to county for runners and races

def city_to_county(inp):
    
    city = inp.City.lower()
    state = inp.State.upper().strip('"')
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

def city_to_county_2(inp):
    
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

# Use the functions to get the coordinates for runner and race locations

rnr_coords = [city_to_county(ultradata.iloc[i]) for i in range(len(ultradata))]
race_coords = [city_to_county_2(ultradata.iloc[i]) for i in range(len(ultradata))]

# Use geopy.distances.geodesic to compute distances between runner and race for all observations

distances = []

for i in range(len(rnr_coords)):
    
    if (rnr_coords[i] != [None,None]) and (race_coords[i] != [None,None]):
        
        distances.append(geodesic(rnr_coords[i], race_coords[i]).mi)
        
    else:
        
        distances.append(None)

# Adding distances to the dataframe

distances = pd.Series(distances, name = 'Travel_Distance')
ultradata = pd.concat([ultradata, distances], axis = 1)

# Creating proxy variables for runner ability - mean of function of gender place in all races

runners = ultradata.Runner_ID.to_list() # Runners
gplace = ultradata.Gender_Place.to_list() # Gender places
genders = ultradata.Gender.to_list() # Gender
rids = ultradata.RACE_ID.to_list() # Races

# Create per race score

g_score = []

for i in range(len(runners)):
    
    tmp = ultradata[ultradata['RACE_ID'] == rids[i]]
    tmp = tmp[tmp['Gender'] == genders[i]]
    gp = min(gplace[i], len(tmp)) - 1
    g_score.append(1 - (gp/len(tmp)))

# Aggregate scores into an average score per runner

ability = []

for i in range(len(runners)):
    
    ids = [j for j in range(len(runners)) if runners[j] == runners[i]]
    vals = [g_score[j] for j in ids]
    ability.append(sum(vals)/len(vals))

# Adding this ability proxy score to the data frame

ability = pd.Series(ability, name = 'Ability')
ultradata = pd.concat([ultradata, ability], axis = 1)

# Getting FIPS codes for runner and race counties

# Defining a helper function for assigning FIPS via ccmap and cases

def flipsadelphia(inp,spec):
    
    if spec == 'rnr':
        
        city = inp.City.lower()
        state = inp.State.upper().strip('"')
        sx = list(ccmap['State short']).index(state)
        st = ccmap['State full'][sx]
        
    else:
        
        city = inp.RACE_City.lower()
        state = inp.State.upper().strip('"')
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

# Using the function to get FIPS codes

rnr_fips = []
race_fips = []

for i in range(len(ultradata)):
    
    rnr_fips.append(flipsadelphia(ultradata.iloc[i], 'rnr'))
    race_fips.append(flipsadelphia(ultradata.iloc[i], 'race'))

# Computing data on previous appearances at events

py_attend  = [] # did you attend py
prev_attend = [] # total previous attendances
attend = [] # have you ever attended (prev_attend > 0)

for i in range(len(ultradata)):
    
    rnr = ultradata.Runner_ID[i]
    race = ultradata.RACE_Name[i]
    yr = ultradata.RACE_Year[i]
    tmp = ultradata[ultradata.Runner_ID == rnr]
    tmp = tmp[tmp.RACE_Name == race]
    tmp2 = tmp[tmp.RACE_Year < yr]
    tmp3 = tmp[tmp.RACE_Year == yr-1]
    prev_attend.append(len(tmp2))
    py_attend.append(len(tmp3))
    attend.append(int(len(tmp2) > 0))

prev_attend = pd.Series(prev_attend, name = 'Total_Prev_Attend')
py_attend = pd.Series(py_attend, name = 'Attended_PY')
attend = pd.Series(attend, name = 'Ever_Prev_Attend')
ultradata = pd.concat([ultradata, prev_attend, py_attend, attend], axis = 1)

# Adding FIPS codes to the main dataframe

rnr_fips = pd.Series(rnr_fips, name = 'FIPS_Runner')
race_fips = pd.Series(race_fips, name = 'FIPS_Race')
ultradata = pd.concat([ultradata, rnr_fips, race_fips], axis = 1)

# Reading in county level socioeconomics controls data via FIPS

pop = pd.read_csv(filepath + 'Population.csv')
pov = pd.read_csv(filepath + 'Poverty.csv')
unemp = pd.read_csv(filepath + 'Labor_Force.csv')
inc = pd.read_csv(filepath + 'Median_Household_Income.csv')
educ = pd.read_csv(filepath + 'Education.csv')

# Adding county level socioeconomics controls to the data

pop_vals = []
pov_vals = []
unemp_vals = []
inc_vals = []
hs_grad = []
some_co = []
ass_grad = []
bs_grad = []
grad_grad = []

f = [int(g) if g > 0 else None for g in rnr_fips.tolist()]

for i in range(len(f)):
    
    g = f[i]
    yr = ultradata.RACE_Year[i]
    
    # Population
    
    try:
        
        pop_tmp = pop[pop.fips == g].reset_index(drop = True)
        popcol = 'pop' + str(yr)
        pop_vals.append(pop_tmp[popcol][0])
        
    except:
        
        pop_vals.append(None)
        
    # Poverty rates
    
    try:
        
        pov_tmp = pov[pov.fips == g].reset_index(drop = True)
        pov_tmp = pov_tmp[pov_tmp.year == yr].reset_index(drop = True)
        pov_vals.append(pov_tmp.allagesinpovertypercent[0])
        
    except:
        
        pov_vals.append(None)
        
    # Unemployment rates
    
    try:
        
        un_tmp = unemp[unemp.fips == g].reset_index(drop = True)
        un_tmp = un_tmp[un_tmp.year == yr].reset_index(drop = True)
        unemp_vals.append(un_tmp.unemploymentrate[0])
        
    except:
        
        unemp_vals.append(None)
        
    # Income
    
    try:
        
        inc_tmp = inc[inc.countyid == g].reset_index(drop = True)
        inc_tmp = inc_tmp[inc_tmp.year == yr].reset_index(drop = True)
        inc_vals.append(inc_tmp.medianhouseholdincome[0])
        
    except:
        
        inc_vals.append(None)
        
    # Education
    
    try:
        
        e_tmp = educ[educ.fips == g].reset_index(drop = True)
        ecol_hs = 'percenthsgrad_' + str(yr)
        ecol_sc = 'percentsomecollege_' + str(yr)
        ecol_ass = 'percentassociates_' + str(yr)
        ecol_bs = 'percentbachelors_' + str(yr)
        ecol_grad = 'percentgrad_degree_' + str(yr)
        
        hs_grad.append(e_tmp[ecol_hs][0])
        some_co.append(e_tmp[ecol_sc][0])
        ass_grad.append(e_tmp[ecol_ass][0])
        bs_grad.append(e_tmp[ecol_bs][0])
        grad_grad.append(e_tmp[ecol_grad][0])
        
    except:
        
        hs_grad.append(None)
        some_co.append(None)
        ass_grad.append(None)
        bs_grad.append(None)
        grad_grad.append(None)

pop_vals = pd.Series(pop_vals, name = 'Population')
pov_vals = pd.Series(pov_vals, name = 'Poverty_Rate')
unemp_vals = pd.Series(unemp_vals, name = 'Unemployment_Rate')
inc_vals = pd.Series(inc_vals, name = 'Income')
hs_grad = pd.Series(hs_grad, name = 'Education_High_School')
some_co = pd.Series(some_co, name = 'Education_Some_College')
ass_grad = pd.Series(ass_grad, name = 'Education_Associates')
bs_grad = pd.Series(bs_grad, name = 'Education_Bachelors')
grad_grad = pd.Series(grad_grad, name = 'Education_Graduate')
ultradata = pd.concat([ultradata, pop_vals, pov_vals, unemp_vals, inc_vals,
                       hs_grad, some_co, ass_grad, bs_grad, grad_grad], axis = 1)

# Reading in the altitude data

altdata = pd.read_csv(filepath[:len(filepath)-11] + 'altitude_data.csv')

# Merging altitude data and the main dataframe

alts = []
f2 = [int(g) if g > 0 else None for g in race_fips.tolist()]

for g in f2:
    
    try:
        
        tmp = altdata[altdata.FIPS == g].reset_index(drop = True)
        alts.append(tmp.ALtitude[0])
        
    except:
        
        alts.append(None)

alts_home = []

for g in f:
    
    try:
        
        tmp = altdata[altdata.FIPS == g].reset_index(drop = True)
        alts_home.append(tmp.ALtitude[0])
        
    except:
        
        alts_home.append(None)

alts = pd.Series(alts, name = 'Altitude')
alts_home = pd.Series(alts_home, name = 'Altitude_Home')
ultradata = pd.concat([ultradata, alts, alts_home], axis = 1)











# Reading in the pollution data

ozone_raw = pd.read_csv(filepath[:len(filepath)-11] + 'ozone_data.csv')
pb_raw = pd.read_csv(filepath[:len(filepath)-11] + 'pb_data.csv')
co_raw = pd.read_csv(filepath[:len(filepath)-11] + 'co_data.csv')
no2_raw = pd.read_csv(filepath[:len(filepath)-11] + 'no2_data.csv')
pm_raw = pd.read_csv(filepath[:len(filepath)-11] + 'pm_data.csv')
pm10_raw = pd.read_csv(filepath[:len(filepath)-11] + 'pm10_data.csv')

# A pair of functions for matching dates between data sets

def month_fx(inp):
    
    months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
    mo = str(months.index(inp) + 1)
    
    if len(mo) == 1:
        
        mo = '0' + mo
    
    return mo

def day_fx(inp):
    
    dia = str(inp)
    
    if len(dia) == 1:
        
        dia = '0' + dia
    
    return dia

# Merging pollution data and the main dataframe

ozone = []
pb = []
co = []
no2 = []
pm = []
pm10 = []

ozone_home = []
pb_home = []
co_home = []
no2_home = []
pm_home = []
pm10_home = []

for i in range(len(f)):
    
    g = f[i]
    date = str(ultradata.RACE_Year[i]) + month_fx(ultradata.RACE_Month[i]) + day_fx(ultradata.RACE_Date[i])
    date = int(date)
    
    tmp1 = ozone_raw[ozone_raw.FIPS == g].reset_index(drop = True)
    tmp2 = pb_raw[pb_raw.FIPS == g].reset_index(drop = True)
    tmp3 = co_raw[co_raw.FIPS == g].reset_index(drop = True)
    tmp4 = no2_raw[no2_raw.FIPS == g].reset_index(drop = True)
    tmp5 = pm_raw[pm_raw.FIPS == g].reset_index(drop = True)
    tmp6 = pm10_raw[pm10_raw.FIPS == g].reset_index(drop = True)
    
    tmp1 = tmp1[tmp1.Date == date].reset_index(drop = True)
    tmp2 = tmp2[tmp2.Date == date].reset_index(drop = True)
    tmp3 = tmp3[tmp3.Date == date].reset_index(drop = True)
    tmp4 = tmp4[tmp4.Date == date].reset_index(drop = True)
    tmp5 = tmp5[tmp5.Date == date].reset_index(drop = True)
    tmp6 = tmp6[tmp6.Date == date].reset_index(drop = True)
    
    try:
        
        ozone.append(tmp1.Value[0])
        
    except:
        
        ozone.append(None)
        
    try:
        
        pb.append(tmp2.Value[0])
        
    except:
        
        pb.append(None)
        
    try:
        
        co.append(tmp3.Value[0])
        
    except:
        
        co.append(None)
        
    try:
        
        no2.append(tmp4.Value[0])
        
    except:
        
        no2.append(None)
        
    try:
        
        pm.append(tmp5.Value[0])
        
    except:
        
        pm.append(None)
        
    try:
        
        pm10.append(tmp6.Value[0])
        
    except:
        
        pm10.append(None)

for i in range(len(f2)):
    
    g = f2[i]
    date = str(ultradata.RACE_Year[i]) + month_fx(ultradata.RACE_Month[i]) + day_fx(ultradata.RACE_Date[i])
    date = int(date)
    
    tmp1 = ozone_raw[ozone_raw.FIPS == g].reset_index(drop = True)
    tmp2 = pb_raw[pb_raw.FIPS == g].reset_index(drop = True)
    tmp3 = co_raw[co_raw.FIPS == g].reset_index(drop = True)
    tmp4 = no2_raw[no2_raw.FIPS == g].reset_index(drop = True)
    tmp5 = pm_raw[pm_raw.FIPS == g].reset_index(drop = True)
    tmp6 = pm10_raw[pm10_raw.FIPS == g].reset_index(drop = True)
    
    tmp1 = tmp1[tmp1.Date == date].reset_index(drop = True)
    tmp2 = tmp2[tmp2.Date == date].reset_index(drop = True)
    tmp3 = tmp3[tmp3.Date == date].reset_index(drop = True)
    tmp4 = tmp4[tmp4.Date == date].reset_index(drop = True)
    tmp5 = tmp5[tmp5.Date == date].reset_index(drop = True)
    tmp6 = tmp6[tmp6.Date == date].reset_index(drop = True)
    
    try:
        
        ozone_home.append(tmp1.Value[0])
        
    except:
        
        ozone_home.append(None)
        
    try:
        
        pb_home.append(tmp2.Value[0])
        
    except:
        
        pb_home.append(None)
        
    try:
        
        co_home.append(tmp3.Value[0])
        
    except:
        
        co_home.append(None)
        
    try:
        
        no2_home.append(tmp4.Value[0])
        
    except:
        
        no2_home.append(None)
        
    try:
        
        pm_home.append(tmp5.Value[0])
        
    except:
        
        pm_home.append(None)
        
    try:
        
        pm10_home.append(tmp6.Value[0])
        
    except:
        
        pm10_home.append(None)

ozone = pd.Series(ozone, name = 'O3')
pb = pd.Series(pb, name = 'Pb')
co = pd.Series(co, name = 'CO')
no2 = pd.Series(no2, name = 'NO2')
pm = pd.Series(pm, name = 'PM2.5')
pm10 = pd.Series(pm10, name = 'PM10')

ozone_home = pd.Series(ozone_home, name = 'O3_Home')
pb_home = pd.Series(pb_home, name = 'Pb_Home')
co_home = pd.Series(co_home, name = 'CO_Home')
no2_home = pd.Series(no2_home, name = 'NO2_Home')
pm_home = pd.Series(pm_home, name = 'PM2.5_Home')
pm10_home = pd.Series(pm10_home, name = 'PM10_Home')

ultradata = pd.concat([ultradata, ozone, pb, co, no2, pm, pm10, ozone_home,
                        pb_home, co_home, no2_home, pm_home, pm10_home], axis = 1)

# Calculating differences between race and home county pollution levels

o3_diff = []
pb_diff = []
co_diff = []
no2_diff = []
pm_diff = []
pm10_diff = []

for i in range(len(ultradata)):
    
    try:
        
        o3_diff.append(ozone[i] - ozone_home[i])
        
    except:
        
        o3_diff.append(None)
        
    try:
        
        pb_diff.append(ozone[i] - ozone_home[i])
        
    except:
        
        pb_diff.append(None)
        
    try:
        
        co_diff.append(ozone[i] - ozone_home[i])
        
    except:
        
        co_diff.append(None)
        
    try:
        
        no2_diff.append(ozone[i] - ozone_home[i])
        
    except:
        
        no2_diff.append(None)
        
    try:
        
        pm_diff.append(ozone[i] - ozone_home[i])
        
    except:
        
        pm_diff.append(None)
        
    try:
        
        pm10_diff.append(ozone[i] - ozone_home[i])
        
    except:
        
        pm10_diff.append(None)    

o3_diff = pd.Series(o3_diff, name = 'O3_Diff')
pb_diff = pd.Series(pb_diff, name = 'Pb_Diff')
co_diff = pd.Series(co_diff, name = 'CO_Diff')
no2_diff = pd.Series(no2_diff, name = 'NO2_Diff')
pm_diff = pd.Series(pm_diff, name = 'PM2.5_Diff')
pm10_diff = pd.Series(pm10_diff, name = 'PM10_Diff')

ultradata = pd.concat([ultradata, o3_diff, pb_diff, co_diff, no2_diff, pm_diff, pm10_diff], axis = 1)

# Finally, converting time into seconds as a dependent variable

def time_fx(inp):
    
    try:
        
        if inp == 'nan':
            
            secs = None
            
        elif len(inp) == 7:
            
            h = int(inp[0])
            m = int(inp[2:4])
            s = int(inp[5:])
            secs = s + 60*m + 3600*h
            
        elif len(inp) == 8:
            
            h = int(inp[0:2])
            m = int(inp[3:5])
            s = int(inp[6:])
            secs = s + 60*m + 3600*h
            
        elif len(inp) == 9:
            
            h = int(inp[0:3])
            m = int(inp[4:6])
            s = int(inp[7:])
            secs = s + 60*m + 3600*h
            
        else:
            
            secs = None
            
    except:
        
        secs = None
        
    return secs

times = [time_fx(str(t)) for t in ultradata.Time]
times = pd.Series(times, name = 'Seconds')
ultradata = pd.concat([ultradata, times], axis = 1)

# Writing the final data frame to file

ultradata.to_csv(filepath + 'ultradata.csv', index = False)

