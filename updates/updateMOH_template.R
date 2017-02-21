# Update data from moh system
source_usr
source_pwd
source_url
dest_usr
dest_pwd
dest_url

x <- transferDHIS2_data(source_usr, source_pwd, source_url, dest_usr, dest_pwd, dest_url)
write(toJSON(x), paste0("Transfer results ", Sys.Date(), '.json'))