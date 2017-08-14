# Sample Update Script
dhis2R_dir <- 'dhis2R_location' # "./" or "/home/aaron/dhis2R" etc
location <- 'some_location_in_credentials' # Edit the credentials file to make sure the name matches.  Could be something like 'haiti', 'malawi', etc

# The script-------------------------------------------------
setwd(dhis2R_dir)

setDHIS2_credentials(location)

cloneDHIS2_data(usr.src, pwd.src, url.src, usr.dest, pwd.dest, url.dest)