# # Assumes settings/functions already loaded
# THIS DID NOT WORK CORRECTLY DO NOT USE THIS FILE. 










# # Charts first --------------------------------------------------------------------------------------
# charts <- getDHIS2_Resource('charts', usr, pwd, url)
# 
# # download the metadata for each chart
# charts_configuration <- lapply(charts$id, function(x) getDHIS2_elementInfo(x, 'charts', usr, pwd, url))
# 
# # update each one to use user assigned org unit
# charts_configuration %<>% lapply(function(x) {x$userOrganisationUnit <- 'true'; x})
# 
# # send the updated data back to the server
# upload_result <- lapply(charts_configuration, function(x) putDHIS2_metaData(x, usr, pwd, x$href))
# 
# 
# 
# # Pivot Tables next --------------------------------------------------------------------------------------
# tables <- getDHIS2_Resource('reportTables', usr, pwd, url)
# 
# # download the metadata for each chart
# tables_configuration <- lapply(tables$id, function(x) getDHIS2_elementInfo(x, 'reportTables', usr, pwd, url))
# 
# # update each one to use user assigned org unit
# tables_configuration %<>% lapply(function(x) {x$userOrganisationUnit <- 'true'; x})
# 
# # send the updated data back to the server
# upload_result <- lapply(tables_configuration, function(x) putDHIS2_metaData(x, usr, pwd, x$href))
