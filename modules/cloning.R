# Cloning functions for one-way sync between a source DHIS2 instance and destination instance


cloneDHIS2_data <- function(usr.from, pwd.from, url.from, usr.to, pwd.to, url.to, 
                               parent_ous = NULL, specific_dataSets = NULL, yearly_to_monthly=T,
                               startDate = Sys.Date() - months(6), endDate= Sys.Date() + years(1), existing_upload_file=NA) {
  # Pull data from one dhis2 and post to another.  If specific_dataSets is specified, it will take each character vector
  # element and find those dataSets from the source dhis2 instance and attempt to download.  If NULL, it will attempt all.
  # this only works when uuids match for BOTH systems.  Otherwise it will kick errors. 
  
  if(!dir.exists('temp/')) dir.create('temp')
  library(lubridate)
  
  from.ds <- getDHIS2_Resource('dataSets', usr.from, pwd.from, url.from, 'periodType')
  upload_results <- list()
  
  orgUnits.dest <- getDHIS2_Resource('organisationUnits', usr, pwd, url)
  
  if (is.null(parent_ous)) {
    cat('Trying to auto-match organisationUnits\n')
    # if no parent orgUnits defined, try to intuit by matching the highest level units for each system
    # requires that ids match
    orgUnits.dest <- getDHIS2_Resource('organisationUnits', usr.to, pwd.to, url.to, c('parent', 'level'))
    orgUnits.source <- getDHIS2_Resource('organisationUnits', usr.from, pwd.from, url.from, c('parent', 'level'))
    for (ll in 1:length(unique(orgUnits.dest$level))) { # look at each level, stop when matches are found. this will cause problems if hierarchy doesn't match exactly
      test <-  orgUnits.dest$id[orgUnits.dest$level == ll] %in% orgUnits.source$id[orgUnits.source$level == ll]
      if (any(test)) break # stop if we found matches. we'll assume any children match in this case. again, this will cause problems if the hierarchy doesn't match
    }
    if (!any(test)) stop('Try explicitly naming orgUnits') # just in case we iterate down to the bottom of the tests and still don't have matches
    parent_ous <- orgUnits.source$parent.id[orgUnits.source$level == ll][test] %>% unique()
    cat('Successfully matched!\n')
    
  }
  
  
  for (i in 1:nrow(from.ds)) {
    if (from.ds$displayName[i] != 'Test') {
      
      # download data from each dataset
      print(from.ds$displayName[i])
      d <- data.frame()
      for (org in parent_ous){
        sub <- try(getDHIS2_dataSet(from.ds$id[i], org, startDate, endDate, usr.from, pwd.from, children='true', url.from, lookup_names = F))
        # Sys.sleep(5)
        if (is.data.frame(sub)) {
          d <- rbind.fill(d, sub)
        }
      }
      
      if (nrow(d) > 0) {
        d <- d[d$value != 0,]
        
        if (from.ds$periodType[i] == 'Yearly' & nrow(d) > 0 & yearly_to_monthly) {
          print('Convert to monthly...')
          # if it's yearly, we want it reproduced at the monthly level for reports. 
          dy <- data.frame()
          # repeat each month
          for (m in 1:12) {
            if (nchar(m) == 1) m <- paste0('0', m)
            sub <- d
            sub$period <- paste0(sub$period, m)
            dy <- rbind.fill(dy, sub)
          }
          d <- dy
        }
        # trim down to just orgUnits in our system
        d <- d[d$orgUnit %in% orgUnits.dest$id,]
        
        write.csv(d, paste0('temp/', gsub("\\/", "-", from.ds$displayName[i]), ".csv"), row.names = F)
      }
    }
  }
  d <- data.frame()
  for (f in list.files('temp')) {
    d %<>% rbind.fill(read.csv(paste0('temp/', f), stringsAsFactors = F))
  }
  # d <- read.csv(d, stringsAsFactors = F)
  resp <- postDHIS2_Values(d[,c('dataElement', 'orgUnit', 'period', 'categoryOptionCombo', 'attributeOptionCombo', 'value')], 1000, usr.to, pwd.to, url.to)
  file.remove('temp/upload.csv')
  returh(resp)
  
}


cloneDHIS2_objects <- function(obj_type, usr.from, pwd.from, url.from, usr.to, pwd.to, url.to, prefix='', update=T) {
  # Clone the metadata from a resource endpoint into a destination instance
  
}

mod_element <- function(element, prefix, id=NULL) {
  
}