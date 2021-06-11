# This scripts pulls data from the EPA AQS API

# Importing required modules

import time
import urllib
from bs4 import BeautifulSoup as bs

# Defining some variables

username = '' # Local username
filepath = 'C:/Users/' + username + '/Documents/Data/ultrapollution/raw_data/' # Where data will be stored -- update as needed
email =  # Username for EPA AQS API
key =  # User key
params = ['88101', '46202', '44201', '81102', '42401', '42101', '14129'] # Desired pollutants
param_names = ['PM2.5', 'NO2', 'O3', 'PM10', 'SO2', 'CO', 'Pb'] # Names for these pollutants
months = [str(i) if i>9 else '0'+str(i) for i in range(1,13)] # Months
edates = ['31', '28', '31', '30', '31', '30', '31', '31', '30', '31', '30', '31'] # End dates
states = [str(i) if i>9 else '0'+str(i) for i in range(1,57)] # States FIPS codes

for s in ['03', '07', '11', '14', '43', '52']: # These are not valid FIPS

    states.remove(s) # Bye bye

# Defining the url components

url0 = 'https://aqs.epa.gov/data/api/transactionsSample/byState?email='
url1 = '&key='
url2 = '&param='
url3 = '&bdate='
url4 = '&edate='
url5 = '&state='

# Retrieving the data

#for param in params: # CURRENTLY THIS JUST RUNS FOR PM10

for state in states:
    
    for year in range(2010,2021):
        
        for month in months:
            
            try: # For FIPS == 47 there is no data from month == 06 to month == 12; this prevents break
                
                print('State / Year / Month :: ' + str(states.index(state)+1) + ' / ' + str(year) + ' / ' + month) # Status update
                bdate = str(year) + month + '01' # Define bdate
                edate = str(year) + month + edates[months.index(month)] # Define edate
                url = url0 + email + url1 + key + url2 + '81102' + url3 + bdate + url4 + edate + url5 + state # Define url
                page = urllib.request.Request(url, headers = {'User-Agent': 'Mozilla/5.0'}) # Make request
                response = urllib.request.urlopen(page) # Request data
                soup = bs(response, 'html.parser') # Clean data
                
                with open(filepath + 'PM10' + '__' + state + '__' + str(year) + '__' + month + '.txt', 'a') as file:
                    
                    file.write(str(soup)) # Write raw data to file
                    file.close() # Close file
            
            except:
                
                continue
            
            time.sleep(5) # Be kind to your local API

