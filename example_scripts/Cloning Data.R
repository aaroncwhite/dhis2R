# Sample Cloning Script
# Assumes you already have all metadata in place from the source server in the destination server

dhis2R_dir <- '/home/dhis/R tool' # "./" or "/home/aaron/dhis2R" etc
location <- 'some_location_in_credentials' # Edit the credentials file to make sure the name matches.  Could be something like 'haiti', 'malawi', etc


# The script-------------------------------------------------
setwd(dhis2R_dir)

# load all of the packages
source('settings.R')

# set credentials
setDHIS2_credentials(location)

# run the update
cloneDHIS2_data(usr.src, pwd.src, url.src, usr.dest, pwd.dest, url.dest, match_on = 'displayName', match_on_prefix = '')