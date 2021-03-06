# Cloning functions for one-way sync between a source DHIS2 instance and destination instance
library(lubridate)
library(tidyverse)
library(yaml)
library(assertthat)

# Object types that are pulled from the system.  These are a select set specifically for
# aggregate space data collection and analysis.  This does NOT cover Tracker related objects. 
.config_objects <- c(
  # Aggregate dimension objets
  'categoryOptions', 'categories', 'categoryCombos', 'categoryOptionCombos', 
  # Additional answer sets and attributes
  'options', 'optionSets', 'attributes', 
  # Data Elements
  'dataElements', 'dataElementGroups', 'dataElementGroupSets',
  # Indicators
  'indicatorTypes', 'indicators', 'indicatorGroups', 'indicatorGroupSets',
  # Data Sets
  'dataEntryForms', 'dataSets')

import_config <- function(config_file){
  # import and validate a yaml config file for cloning
  config <- yaml.load_file(config_file)
  
  return(config)
    
}


cloneDHIS2_metaData <- function(usr.src, pwd.src, url.src, usr.dest, pwd.dest, url.dest,
                                dest_prefix, metaData_objects=.config_objects, parallel=T) {
  # The big one.  Take a configuration from a source system and reproduce it in a destination system
  # with some modifications.  Mainly, adding a prefix to all objects denoting it has been cloned (i.e. 
  # 'MOH-' or something like that).  This script is opionated in that it will use the code property
  # to store the prefix+id of the source system in the code property of the destination system.
  # Due to shortName issues, it also stores that same code in the shortName.  This maintains the 
  # requirement of a unique name, shortName, and code.  If the system uses shortName for analytics, 
  # you'll need to come up with something else. You could still use this to upload; however, and then 
  # update the uploaded metadata using patchDHIS2_metaData.
  
  # This is NOT for keeping two systems concurrent in their configuration.  It's mainly to keep the 
  # configuration from one dhis2 available in the destination, but allow for separate configuration to 
  # occur in the destination system. 
  # change metaData_objects if you only want to clone certain aspects. 
  
  # Ex.
  # > cloneDHIS2_metaData('admin', 'district', 'https://play.dhis2.org/demo/api/', )
  
  # get the source metaData
  md <- getDHIS2_metadata(usr.src, pwd.src, url.src, objects = metaData_objects)
  
  # We don't use groupsets, so we'll completely remove those plus some other
  # system specific things that we dont' want to bring over from the source system
  ignore <- c('user', 'userGroupAccesses', 'href', 'access')
  
  # Make some modifications to all meta data elements.  Mainly adding
  # a prefix to name and display name, and making a code that has the 
  # dest_prefix+source_id
  for (i in 1:length(md)) {
    md[[i]] %<>% lapply(function(x) {
      x$code <- paste0(dest_prefix, x$id)
      x$name <- paste0(dest_prefix,x$name)
      # This is an intentional decision to make shortName unusable
      # it's an unnecessary property for metadata and we don't use it. 
      x$shortName <- x$code 
      x$displayShortName <- x$shortName 
      
      # This is also an intentional omission for now.  Name is updating the 
      # database language, and then use that same value for displayName. This 
      # could backfire if the database language is set to french and the user 
      # account performing this action is set to english.  it would overwrite
      # all display names to the french version plus the prefix. 
      x$displayName <- x$name
      x$publicAccess <- 'r-------'
      x[!(names(x) %in% ignore)]
    })
  }
  
  
  
  # First thing we'll do is find all the id and dimension elements
  # in the metadata list.  these are key identifiers and we'll want to 
  # cascade update across the entire metadataset to ensure things match
  # when we upload to the new system. 
  obj_map <- map_property(md, c('id', 'dimension'))
  
  # extract the unique values we're replacing
  ids_to_replace <- sapply(obj_map, function(x) x$property_value) %>% unique()
  
  # generate new ids in the destination system to use
  ids_to_use <- getDHIS2_systemIds(length(ids_to_replace), usr.dest, pwd.dest,url.dest)
  
  # validate our new ids don't collide with ids in the old system (if they did, we might inadvertently assign objects in the wrong place)
  while (T) {
    test <- ids_to_replace %in% ids_to_use
    if (any(test)) {
      print('Some ids matched in the source system to the newly generated ids in the destination system, trying to get different ones...')
      ids_to_use %<>% .[!test] %>% c(getDHIS2_systemIds(length(which(test)), usr.dest, pwd.dest, url.dest))
      
    }  
    else break
  }
  
  # Make the value_pair dataframe to pass to find_replace
  value_pairs <- data.frame('find' = ids_to_replace, 'replace' = ids_to_use, stringsAsFactors = F)
  
  # For each row, recursively replace the value across the metadataset.  Since this is 
  # a huge task, there is an option to run it in parallel
  md <- find_replace(md, value_pairs, obj_map, parallel = parallel)
  
  # Now map the numerator and denominator elements (just in the indicators section)
  obj_nd <- map_property(md$indicators, c('numerator', 'denominator'))
  # use the same value_pair set and update the numerator and denominator references to look for the
  # new objects
  md$indicators <- find_replace(md$indicators, value_pairs, obj_nd, parallel = F)
  
  # Let's do a double check and make sure all of the new ids we expect to see are there
  new_obj_map <- map_property(md, 'id', ignore=ignore)
  check_on_ids <- sapply(new_obj_map, function(x) x$property_value) %>% unique()
  any(check_on_ids == ids_to_replace)
  all(check_on_ids == ids_to_use)
  
  # last thing is to update the T/F objects.  Because of how dhis2 expects to see the information
  # we need to change TRUE and FALSE to 'true' and 'false'. Why does it give you information back 
  # as TRUE and FALSE?  Unclear. I just know it needs to be changed. 
  obj_tf <- map_property(md, c('externalAccess', 'zeroIsSignificant', 'annualized'))
  md <- find_replace(md, data.frame('find' = c(TRUE, FALSE), 'replace' = c('true', 'false')), obj_tf, parallel = F)
  
  # Finally, upload the new metadata.  200s are good.  
  r <- postDHIS2_metadataPackage(md, usr.dest, pwd.dest, url.dest)
  
  # we'll return the newly formatted metadata object and the response we got just incase something goes wrong. 
  # the metadata can still be re-uploaded using uploadDHIS2_metaData again. 
  return(list('metaData' = md, 'response' = r))
}


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
  
  orgUnits.dest <- getDHIS2_Resource('organisationUnits', usr.dest, pwd.dest, url.dest)
  
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
          d <- bind_rows(d, sub)
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
            dy <- bind_rows(dy, sub)
          }
          d <- dy
        }
        # trim down to just orgUnits in our system
        # d <- d[d$orgUnit %in% orgUnits.dest$id,]
        
        write.csv(d, paste0(files_dir, gsub("\\/", "-", from.ds$displayName[i]), ".csv"), row.names = F)
      }
    }
  }
  d <- data.frame()
  for (f in list.files(files_dir)) {
    d %<>% bind_rows(read.csv(paste0(files_dir, f), stringsAsFactors = F))
  }
  # d <- read.csv(d, stringsAsFactors = F)
  
  # Update the id values to match our system
  # orgunits are already downloaded in orgUnits.dest
  cat('Converting id scheme to destination system based on', match_on, '. Hang tight.\n')
  catOptCmbo <- getDHIS2_Resource('categoryOptionCombos', usr.dest, pwd.dest, url.dest)
  de <- getDHIS2_Resource('dataElements', usr.dest, pwd.dest, url.dest)
  
  d %<>% convert_src_to_dest(match_on, match_on_prefix, de, orgUnits.dest, catOptCmbo)
  d %<>% filter(orgUnit %in% orgUnits.dest$id)
  
  
  resp <- postDHIS2_Values(d[,c('dataElement', 'orgUnit', 'period', 'categoryOptionCombo', 'attributeOptionCombo', 'value')], 1000, usr.dest, pwd.dest, url.dest)
  if (clean_files) {
    sapply(list.files(files_dir,full.names = T), file.remove)
  }
  return(resp)
  
}
convert_src_to_dest <- function(df, match_on, match_on_prefix, dataElements, organisationUnits, categoryOptionCombos) {
  df$dataElement %<>% plyr::revalue(make_revalue_map(gsub(match_on_prefix,"", dataElements[,match_on]), dataElements$id), warn_missing = F)
  df$orgUnit %<>% plyr::revalue(make_revalue_map(gsub(match_on_prefix, "", organisationUnits[,match_on]), organisationUnits$id), warn_missing = F)
  df$categoryOptionCombo %<>% plyr::revalue(make_revalue_map(gsub(match_on_prefix,"", categoryOptionCombos[,match_on]), categoryOptionCombos$id), warn_missing = F)
  df$attributeOptionCombo %<>% plyr::revalue(make_revalue_map(gsub(match_on_prefix,"", categoryOptionCombos[,match_on]), categoryOptionCombos$id), warn_missing = F)
  return(df)
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

getDHIS2_specificExport <- function(obj_ids, usr, pwd, url, remap_cats=T, 
                                    usr.dest, pwd.dest, url.dest) {
  # Get specific medatadata info based in input ids from the metadata endpoint
  # _____________________________________________________
  # obj_ids => a character vector of ids to lookup, can be any type
  # usr, pwd, url => credentials and location of source system
  # remap_cats => boolean, whether to update default ids for categoryCombos, etc
  # usr.dest, pwd.dest, url.dest => credentials and location of destination system
  #   only necessary if remap_cats = T
  spec_md <- GET(sprintf("%smetadata.json?filter=id:in:[%s]", url, paste0(obj_ids %>% unlist(), collapse=",")), authenticate(usr, pwd))
  if (spec_md$status_code != 200) stop('Something went wrong')
  
  spec_md %<>% content() %>% removeDHIS2_userInfo()
  
  if (remap_cats) {
    spec_md %<>% remapDHIS2_defaults(usr, pwd, url, usr.dest, pwd.dest, url.dest)
  }
  return(spec_md)
}


getDHIS2_detailedExport <- function(obj_id, obj_type, usr, pwd, url, 
                                    remap_cats = T, 
                                    usr.dest, pwd.dest, url.dest) {
  # Get a detailed metadata export, strip out the user info
  # _______________________________________
  # obj_id => id of for dependency export
  # obj_type => program, dataset, or dashboard
  # usr, pwd, url => credentails and location of source
  # remap_cats => boolean, whether to find category ids and replace with default
  #   dest server info (Seems to happen with pre 2.28, tracker data elements still have
  #   categories defined and needs to be revalued with dest system)
  # usr.dest, pwd.dest, url.dest => destination server credentials 
  #   only necessary if remap_cats = T
  dep_export <- GET(sprintf("%s%s/%s/metadata.json", url, obj_type, obj_id), authenticate(usr, pwd)) %>% content()
  #   
  #   tryCatch(
  #   ,
  #   error = .getDHIS2_detailedMetadataFallback(obj_id, usr, pwd, url)
  # )
  dep_export[!grepl('date|system', names(dep_export))] %<>% removeDHIS2_userInfo()
  
  # # Config depends on default category options, etc. need to look them up from the
  # # dest server (or if fully blank could recode ids to match)
  if (remap_cats) {
    dep_export <- remapDHIS2_defaults(dep_export, usr, pwd, url, usr.dest, pwd.dest, url.dest)
  }
  
  return(dep_export)
 
}

.getDHIS2_detailedMetadataFallback <- function(obj_id, usr, pwd, url, parallel = T, nc=detectCores()) {
  # Attempt to recursively "chase" any ids we find to mimic the metadataDependency export
  # this should only be used if no access is available to the metadata api endpoint as it
  # will take much longer and is more prone to error
  cat('Falling back to individual endpoint strategy. This will take longer...\n')
  if (!parallel) {
    nc <- 1
  }
  cl <- makeCluster(nc)
  registerDoParallel(cl)
  
  setup <- foreach(i=1:nc) %dopar% {source('settings.R')}
  
  lookup <- obj_id 
  collected <- c()
  collected_objs <- list()
  while(T) {
    cat('Looking up',length(lookup), 'objects')
    if (length(lookup) > 0) {
      lookups <- foreach(x = lookup) %dopar% {
        # get information about the object first then strip out the object type
        # from the href
        y <- getDHIS2_elementInfo(x, 'identifiableObjects', usr, pwd, url)
        obj_type <- gsub(sprintf("%s(.+)/.+", url, x), '\\1', y$href)
        # return a named list with the object type. we'll use this to organize things 
        # later
        y <- list(getDHIS2_elementInfo(x, obj_type, usr, pwd, url))
        names(y) <- obj_type
        cat(x, ' ', obj_type,rep(' \t', 20), '\r')
        flush.console()
        y
      }
      cat('\n')
      collected %<>% c(lookup)
      collected_objs %<>% append(lookups)  
      
      obj_map <- map_property(lookups, 'id')
      lookup <- sapply(obj_map, function(x) x$property_value) %>% unique()
      lookup %<>% .[!(. %in% collected)]
      
    }
    else break
    
  }
  stopCluster(cl)
  # now that we have everything,
  # arrange it the right way into a similar metadata package
  # this method actually gets indicator groups and data element groups
  # as well, which are not default in the metadata export
  md <- list()
  for (i in collected_objs) {
    md[[names(i)]] %<>% append(i[[1]])
  }
  
  return(md)
  
}


compareDHIS2_configs <- function(config1, config2, match=NULL, parallel=T, nc=ceiling(detectCores()/2)) {
  # Compare two configs from dhis2 instances
  # _________________________________________
  # config1, config2 => config files from getDHIS2_metadata or getDHIS2_detailedExport
  # match => a named vector with attribute map
  #   ex.- c('id' = 'code') would match id from config1 to code from config2
  # _________________________________________
  # returns a dataframe of value pairs mapping config1 to config2
  if (!parallel) nc = 1
  cl <- makeCluster(nc)
  registerDoParallel(cl)
  value_pairs <- data.frame()
  
  if (is_null(match)) match <- c('id' = 'id')
  c1 <- names(match)
  c2 <- match
  # Run through all attributes of config1 and see if in config2
  for (i in names(config1)) {
    # make a pool of values to compare from config2
    if (i %in% names(config2)) {
      md_codes <- sapply(config2[[i]], function(x) x[match])
    }
    # if we don't have this attribute, we'll assume they're all missing
    else {
      md_codes <- c()
      warning(sprintf("%s attribute not found in config2!", i))
    }
    cat('\n')
    print(i)
    md_update <- lapply(config1[[i]], function(j) {
      if (j[[c1]] %in% md_codes) {
        cat('\r', j[[c1]], '\t\t')
        # we found the config1 value in config2, so get the existing id <==> id mapping
        vp <- data.frame(find = j$id, replace = config2[[i]][[which(j[[c1]] == md_codes)]]$id, type=i, stringsAsFactors = F)
      }
      else {
        # we can't find anything, so make a new id <==> NULL mapping (to be filled in later)
        vp <- data.frame(find = j$id, replace = NA, type=i, stringsAsFactors = F)
      } 
      flush.console()
      vp
    }) %>% bind_rows()
    value_pairs %<>% bind_rows(md_update)
    
  }
  
  stopCluster(cl)
  return(value_pairs)
}


remapDHIS2_defaults <- function(md_export, usr, pwd, url, usr.dest, pwd.dest, url.dest) {
  md <- GET(sprintf('%smetadata.json?filter=name:like:default&categoryOptions=true&categories=true&categoryOptionCombos=true&categoryCombos=true',url.dest), authenticate(usr.dest, pwd.dest))
  md %<>% content()
  ids_to_use <- data.frame('replace' = sapply(md[-1], function(x) x[[1]]$id),
                           'type' = names(md[-1]), stringsAsFactors = F)
  md_src <- GET(sprintf('%smetadata.json?filter=name:like:default&categoryOptions=true&categories=true&categoryOptionCombos=true&categoryCombos=true',url), authenticate(usr, pwd))
  md_src %<>% content()
  ids_to_replace <- data.frame('find' = sapply(md_src[-1], function(x) x[[1]]$id),
                               'type' = names(md_src[-1]), stringsAsFactors = F)
                               
  
  
  # Make the value_pair dataframe to pass to find_replace
  # TODO: pull this functionality into it's own function outside of the full cloning function
  # since that was intended for aggregate systems
  value_pairs <- inner_join(ids_to_use, ids_to_replace, by='type')
  
  # For each row, recursively replace the value across the metadataset.  Since this is
  # a huge task, there is an option to run it in parallel
  # this recursively fines all the values in the find column and replaces with respective
  # replace columns.  :)
  obj_map <- map_property(md_export, 'id')
  
  md_export <- find_replace(md_export, value_pairs, obj_map, parallel = T)
  # md_export$dataElements %<>% lapply(function(x) {x$categoryCombo = NULL; x} )
  md_export[c('categories', 'categoryOptions', 'categoryCombos', 'categoryOptionCombos')] = NULL
  return(md_export)
}

removeDHIS2_userInfo <- function(md_export) {
  # requires that the input list have one element per 
  # dhis2 endpoint (names don't matter)
  md_export %<>% lapply(function(x) lapply(x, function(y) {
    srch <- grepl('^user$|userAccesses', names(y))
    if (any(srch)) {
      # strip the shit out
      y[srch] = NULL
    }
    y
  })
  )
  return(md_export)
}

saveDHIS2_metadataExport <- function(md_export, filename) {
  # Save export in UTF-8 format
  f = file(filename, encoding = 'UTF-8')
  write(toJSON(md_export), file=f)
  close(f)
  # 
}

postDHIS2_metadataPackage <- function(md_export, usr, pwd, url, strategy='CREATE_AND_UPDATE') {
  # Post a payload to the metadata endpoint.  naming this separately
  # to not conflict with postDHIS2_metaData()
  # ___________________________________________
  # md_export => prepared payload, with user info removed
  # usr, pwd, url => credentials and location of server
  # 
  r <- POST(sprintf("%smetadata?&atomicMode=false&importStrategy=%s", url, strategy), authenticate(usr, pwd), body=md_export, encode='json', accept_json())
  return(r)
}


