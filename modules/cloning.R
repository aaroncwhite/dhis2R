# Cloning functions for one-way sync between a source DHIS2 instance and destination instance
library(lubridate)

# Object types that are pulled from the system.  These are a select set specifically for
# aggregate space data collection and analysis.  This does NOT cover Tracker related objects. 
.config_objects <- c(
  # Aggregate dimension objets
  'categoryOptions', 'categories', 'categoryCombos', 'categoryOptionCombos', 
  # Additional answer sets and attributes
  'optionSets', 'attributes', 
  # Data Elements
  'dataElements', 'dataElementGroups', 'dataElementGroupSets',
  # Indicators
  'indicatorTypes', 'indicators', 'indicatorGroups', 'indicatorGroupSets',
  # Data Sets
  'dataEntryForms', 'dataSets')


cloneDHIS2_data <- function(usr.src, pwd.src, url.src, usr.dest, pwd.dest, url.dest, 
                            parent_ous = NULL, specific_dataSets = NULL, match_on='code', match_on_prefix='MOH-',
                            yearly_to_monthly=F, startDate = Sys.Date() - months(6), 
                            endDate= Sys.Date() + years(1), files_dir='temp/', clean_files=T) {
  # Pull data from one dhis2 and post to another.  If specific_dataSets is specified, it will take each character vector
  # element and find those dataSets from the source dhis2 instance and attempt to download.  If NULL, it will attempt all.
  # this only works when uuids match for BOTH systems.  Otherwise it will kick errors. 
  # match_on determines the property to match the data in the source system to the destination system. 
  # For our configurations, we're storing the source system id in the 'code' property and adding a prefix of 'MOH-'.
  # Change the match_on_prefix to match whatever has been set in the system.  match_on defaults to 'code' and match_on_prefix 
  # defaults to 'MOH-'
  # files_dir is where the script will store temporary csvs of the successfull downloads and clean_files=T will remove 
  # all of the files after completing. 

  if(!dir.exists(files_dir)) dir.create(files_dir)
  
  from.ds <- getDHIS2_Resource('dataSets', usr.src, pwd.src, url.src, 'periodType')
  upload_results <- list()
  
  orgUnits.dest <- getDHIS2_Resource('organisationUnits', usr, pwd, url)
  
  if (is.null(parent_ous)) {
    cat('Trying to auto-match organisationUnits\n')
    # if no parent orgUnits defined, try to intuit by matching the highest level units for each system
    # requires that ids match
    orgUnits.dest <- getDHIS2_Resource('organisationUnits', usr.dest, pwd.dest, url.dest, c('parent', 'level'))
    orgUnits.source <- getDHIS2_Resource('organisationUnits', usr.src, pwd.src, url.src, c('parent', 'level'))
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
        sub <- try(getDHIS2_dataSet(from.ds$id[i], org, startDate, endDate, usr.src, pwd.src, children='true', url.src, lookup_names = F))
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
        
        write.csv(d, paste0(files_dir, gsub("\\/", "-", from.ds$displayName[i]), ".csv"), row.names = F)
      }
    }
  }
  d <- data.frame()
  for (f in list.files(files_dir)) {
    d %<>% rbind.fill(read.csv(paste0(files_dir, f), stringsAsFactors = F))
  }
  # d <- read.csv(d, stringsAsFactors = F)
  
  # Update the id values to match our system
  # orgunits are already downloaded in orgUnits.dest
  cat('Converting id scheme to destination system based on', match_on, '. Hang tight.\n')
  catOptCmbo <- getDHIS2_Resource('categoryOptionCombos', usr.dest, pwd.dest, url.dest)
  de <- getDHIS2_Resource('dataElements', usr.dest, pwd.dest, url.dest)
  
  d %<>% convert_src_to_dest(match_on, match_on_prefix, de, orgUnits.dest, catOptCmbo)
  
  resp <- postDHIS2_Values(d[,c('dataElement', 'orgUnit', 'period', 'categoryOptionCombo', 'attributeOptionCombo', 'value', 'created', 'lastUpdated')], 1000, usr.dest, pwd.dest, url.dest)
  if (clean_files) {
    sapply(list.files(files_dir,full.names = T), file.remove)
  }
  return(resp)
  
}

convert_src_to_dest <- function(df, match_on, match_on_prefix, dataElements, organisationUnits, categoryOptionCombos) {
  df$dataElement %<>% revalue(make_revalue_map(gsub(match_on_prefix,"", dataElements[,match_on]), dataElements$id), warn_missing = F)
  df$orgUnit %<>% revalue(make_revalue_map(gsub(match_on_prefix, "", organisationUnits[,match_on]), organisationUnits$id), warn_missing = F)
  df$categoryOptionCombo %<>% revalue(make_revalue_map(gsub(match_on_prefix,"", categoryOptionCombos[,match_on]), categoryOptionCombos$id), warn_missing = F)
  df$attributeOptionCombo %<>% revalue(make_revalue_map(gsub(match_on_prefix,"", categoryOptionCombos[,match_on]), categoryOptionCombos$id), warn_missing = F)
  return(df)
}

getDHIS2_metadata <- function(usr, pwd, url, individual_endpoints=T, objects=.config_objects) {
  # get the entire metadata, with details from a dhis2 instance

  if (individual_endpoints) {
    # attempt to get the metadata individually. 
    cat('Using individual endpoints. This will take a little longer.\n')

    metadata <- lapply(objects, function(x) {
      cat('\r',x, rep(" ", 20))
      x <- getDHIS2_Resource(x, usr, pwd, url, '*', transform_to_df = F)
      flush.console()
      x
    })
    names(metadata) <- objects
    return(metadata)
  }
  else {
    x <- GET(paste0(url, 'metadata?viewType=detailed'), authenticate(usr, pwd), accept_json())
    
    if (x$status_code != 200) stop('Something went wrong. Are the credentials correct? If the problem continues
                                    try setting individual_endpoints=T.')
    else return(content(x))

  }

}


mod_element <- function(element, type, prefix='', id=NULL, access='r--------') {
  # for transferring metadata from one dhis2 to another
  # need to update and strip out some things
  # Strip: access, ownership, userdata
  # Add prefix if exists
  # Make source id value code value for destination
  # If id is declared, that will overwrite the object id (for use with existing objects)
  config_standard_mods <- list('add_prefix' = c('name', 'displayName', 'description'),
                               'remove' = c('access', 'user.'), # remove anything related to the former user info
                               'access' = "r-------")
  # will this work for elements that do not have a code?
  
  
  # Update code
  element[['code']] <- paste0(prefix, element$id)
  
  # Update/remove id
  if (!is.null(id)) {
    element$id <- id
  }
  # else {
  #   element <- element[-grep('id', names(element))]
  # }
  
  # Update names/labels
  for (n in c('name', 'displayName', 'description')) {
    element[[n]] <- paste0(prefix, element[[n]])
  }
  
  # strip access, user, ownership. set public access privileges to read only
  element <- element[-greps(c('user', 'access'), names(element))]
  
  element$publicAccess <- access

  return(element)
  
}








