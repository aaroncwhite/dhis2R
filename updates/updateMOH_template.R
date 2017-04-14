# Update data from moh system
source_usr <- usr.moh
source_pwd <- pwd.moh
source_url <- url.moh
dest_usr <- usr
dest_pwd <- pwd
dest_url <- url

x <- transferDHIS2_data(source_usr, source_pwd, source_url, dest_usr, dest_pwd, dest_url)
write(toJSON(x), paste0("Transfer results ", Sys.Date(), '.json'))