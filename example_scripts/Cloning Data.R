# Sample Cloning Script
# Assumes you already have all metadata in place from the source server in the destination server

dhis2R_dir <- '/home/dhis/R tool' # "./" or "/home/aaron/dhis2R" etc
location <- 'some_location_in_credentials' # Edit the credentials file to make sure the name matches.  Could be something like 'haiti', 'malawi', etc
n_months_history <- 6

# The script-------------------------------------------------
setwd(dhis2R_dir)

# load all of the packages
source('settings.R')

# set credentials
setDHIS2_credentials(location)


start_date <- (Sys.Date() - 1) - months(n_months_history)
end_date <- Sys.Date() - 1
# run the update
cloneDHIS2_data(usr.src, pwd.src, url.src, # source credentials
                usr.dest, pwd.dest, url.dest, # destination credentials
                startDate = start_date, endDate = end_date, # timeframe
                match_on = 'id', match_on_prefix = '') # matching parameters if not using matching id values