# This script cleans raw EPA AQS data

# Importing required modules

import pandas as pd
import os
import re

# Defininig some parameters

username = '' # Local username
filepath = 'C:/Users/' + username + '/Documents/Data/ultrapollution/' # Data directory -- update as needed

# Create a list of all .txt files to parse

files = os.listdir(filepath + 'raw_data/')

# Initializing some lists

states = []
counties = []
sites = []
pollutants = []
values = []
dates = []

# Main loop

for file in files:
    
    print(file) # Tracking progress
    f = open(filepath + 'raw_data/' + file, 'r') # Open file
    d = f.read() # Read in the data as a string
    
    state_ids = [x.start() for x in re.finditer('state_code', d)] # Get positions of all state_codes
    county_ids = [x.start() for x in re.finditer('county_code', d)] # Get positions of all county_codes
    site_ids = [x.start() for x in re.finditer('site_number', d)] # Get positions of all site_numbers
    pollutant_ids = [x.start() for x in re.finditer('parameter_code', d)] # Get positions of all parameter_codes
    value_ids = [x.start() for x in re.finditer('reported_sample_measurement', d)] # Get positions of all reported_measurements
    date_ids = [x.start() for x in re.finditer('sample_begin_date', d)] # Get positions of all sample_dates
    
    for idx in state_ids: # Get all state_codes
        
        states.append(d[idx+14:idx+16])

    for idx in county_ids: # Get all county_codes
        
        counties.append(d[idx+15:idx+18])

    for idx in site_ids: # Get all site_numbers
        
        sites.append(d[idx+15:idx+19])

    for idx in pollutant_ids: # Get all pollutants
        
        pollutants.append(d[idx+18:idx+23])

    for idx in value_ids: # Get all pollution data
        
        a = d[idx+31:].find('"') # The number of digits presented is inconsistent
        values.append(d[idx+31:idx+31+a])

    for idx in date_ids: # Get all dates
        
        dates.append(d[idx+21:idx+29])
        
    f.close()

# Create a dataframe containing the complete data set
    
dates = pd.Series(dates, name = 'Date')
states = pd.Series(states, name = 'State')
counties = pd.Series(counties, name = 'County')
sites = pd.Series(sites, name = 'Site')
pollutants = pd.Series(pollutants, name = 'Pollutant')
values = pd.Series(values, name = 'Value')
df = pd.concat([dates, states, counties, sites, pollutants, values], axis = 1)

# Some of the entries for Value were 'ull,\n      ' whenever the result was null, so we drop those

df = df[df.Value != 'ull,\n      '].reset_index(drop = True)

# Write the dataframe to file

df.to_csv(filepath + 'epa_aqs_data_no2.csv', index = False)

