library(XLConnect)
# library(stringi)
library(magrittr)
library(plyr)
library(rlist)

# source('api.R')
# source('translations.R')
# source('payload_creation.R')
# source('translations.R')
# source('utilities.R')

# META DATA FILE SCRAPING AND UPLOAD --------------------------------------------------------------
# This function uses many of the functions below to combine into one cohesive scraping function. 
# It takes in an excel configuration file, parses out each different part and then posts to the 
# dhis2 server.  It currently supports data elements, category combos, categories, and category options.

uploadDHIS2_configFile <- function(filename, usr, pwd, url, overwrite=F, prompt_overwrite=T, verbose=F, object=NA) {
  # Take a meta data config file, scrape the data off and 
  # upload to dhis2 server for each part. Currently supports
  # category options, categories, category combinations, and data elements
  # still need to add user, indicator, and data set import
  # for now assuming that the file has all of the necessary tabs and information
  # overwrite indicates of overwriting of existing config is allowed.  if overwrite==T
  # will prompt for permission on all potential overwriting operations
  
  # Ex.
  # > uploadConfig('meta-data-config.xlsx', u, p, url)
  # Creating Options
  # Creating Categories
  # Creating Category Combinations
  # Creating Data Elements
  # Config Uploaded in 1.34 minutes
  startTime <- Sys.time()
  
  # we can pass in an already parsed object instead of having to scrape a file
  if (is.na(object)) {
    config <- scrapeDHIS2_configFile(filename)
    cat('\n')
  }
  else {
    config <- object
    rm(object)
    cat('Using already existing config object.\n\n')
  }

  
  results <- list()
  config <- config[names(config) != ('importSummary')]
  for (obj in 1:length(config)) {
    # upload each object type as defined  by the list name and data inside that object
    # determine appropriate format using uploadDHIS2_metaData() switch to appropriately
    # structure upload.
    if (nrow(config[[obj]]) > 0) {
      obj_type <- names(config)[obj]
      resp <- list(uploadDHIS2_metaData(config[[obj]], obj_type = obj_type, usr, pwd, url, 
                                        overwrite=overwrite, prompt=prompt_overwrite, verbose=verbose))
      names(resp) <- obj_type 
      results <- append(results, resp)
    }

  }
  
  cat("------------------ Upload Completed ------------------\n")
  print(t(as.data.frame.list(lapply(results, function(x) unlist(lapply(x, function(j) lapply(j, function(i) length(i))))))))
  print(round(Sys.time() - startTime,2))
  return(results)
  
  
}

removeDHIS2_configFile <- function(filename, usr, pwd, url, de=T, catCombo=T, cats=T, options=F) {
  # Scrape config file and remove configuration elements based on the parameters set to TRUE
  # BE CAREFUL removing options.  This could cause problems with other areas that use the same 
  # values.  Make sure to check first. 
  
  # First import the file
  config <- scrapeDHIS2_configFile(filename)
  result <- list()
  
  if (de== T) {
    if (length(config$dataElements$dataElement) > 0) { # check if we matched anything
      deleted <- list(deleteDHIS2_objects(obj_names = config$dataElements$dataElement, obj_type='dataElements', usr=usr, pwd=pwd, url=url))
      names(deleted) <- 'dataElements'
      result <- append(result, deleted)
      
      
    }
  }
  # remove the data elements
  
  if (catCombo == T) {
    
    # Before we can delete the categoryCombos, we need to check and remove any categoryOptionCombos associated
    categoryCombos <- getDHIS2_Resource('categoryCombos', usr, pwd, url) # download the current list
    catCombos_remove <- config$categoryCombos$Category.Combo.Name
    catCombos_remove <- catCombos_remove[catCombos_remove != "default"]
    if (!is.null(catCombos_remove)) {
      catCombo_ids <- categoryCombos$id[categoryCombos$displayName %in% catCombos_remove]
      catComboErrors <- c()
      sub_results <- list()
      for (cc_id in catCombo_ids) {
        catCombo <- getDHIS2_elementInfo(cc_id, 'categoryCombos', usr, pwd, url)
        if (!is.null(names(catCombo))) {
          catOptCombos_ids <- unlist(catCombo$categoryOptionCombos)
          if (length(catOptCombos_ids) > 0) {
            deleted <- list(deleteDHIS2_objects(ids= catOptCombos_ids, obj_type='categoryOptionCombos', usr=usr, pwd=pwd, url=url))
            names(deleted) <- 'categoryOptionCombos'
            sub_results <- append(result, deleted )
            
          }
        }
        else {
          cat('something went wrong with ', cc_id, "\n")
          catComboErrors <- c(catComboErrors, cc_id)
        }
      }
    }
    result <- append(result, sub_results)
    deleted <- list(deleteDHIS2_objects(ids= catCombo_ids, obj_type='categoryCombos', usr=usr, pwd=pwd, url=url))
    names(deleted) <- 'categoryCombos'
    result <- append(result, deleted)
    
  }
  
  if (cats==T) {
    deleted <- list(deleteDHIS2_objects(config$categories$Disaggreation.Category.Name..alphabetical.order., obj_type='categories', usr=usr, pwd=pwd, url=url))
    names(deleted)
    result <- append(result, deleted)
  }
  
  if (options==T) {
    deleted <- list(deleteDHIS2_objects(config$categoryOptions, obj_type='categoryOptions', usr=usr, pwd=pwd, url=url))
    names(deleted) <- 'categoryOptions'
    result <- append(result, deleted)
  }
  
  return(result)
  
}

scrapeDHIS2_configFile <- function(filename) {
  # Take a meta data config file, scrape the data off 
  # for now assuming that the file has all of the necessary tabs a
  
  # Ex.
  # > scrapeDHIS2_configFile('meta-data-config.xlsx')
  # Config scraped in 1.3 seconds
  # dataElements  categories  categoryOptions ...
  # 45            12          50
  
  startTime <- Sys.time()
  
  wb <- XLConnect::loadWorkbook(filename)
  
  # Load all of the options we're working with
  catOptions <- XLConnect::readWorksheet(wb, 'Category Options')
  
  # CREATE OPTIONS -----------------------------------------------------------
  # take just the options listed, remove na values, and duplicates to just
  # return the unique options we're working with
  options <- as.data.frame(catOptions[,-1] %>% .[!is.na(.)] %>% .[!duplicated(.)])
  names(options) <- 'options'
  
  # CREATE CATEGORIES --------------------------------------------------------
  dataElements <- XLConnect::readWorksheet(wb, "Data Elements")
  columns <- c(findColumn_index('Data.Element.Name', dataElements, "Can't determine column with DATA ELEMENT name!", 'Please specify data element column number: '),
               findColumn_index('Short.Name', dataElements, "Can't determine column with SHORT NAME!", 'Please specify short name column number: '),
               findColumn_index('Code', dataElements, "Can't determine column with SHORT NAME!", 'Please specify short name column number: '),
               findColumn_index('Description',dataElements, "Can't determine column with DESCRIPTION", 'Please specify description column number: '),
               findColumn_index("Value.Type", dataElements,"Can't determine column with VALUE TYPE!",'Please specify value type column number: '),
               findColumn_index('Aggregation.Type',dataElements, "Can't determine column with AGGREGATION TYPE!", 'Please specify aggregation type column number: '),
               findColumn_index('Category.Combo',dataElements, "Can't determine column with CATEGORY COMBINATION", 'Please specify category combo column number: '),
               findColumn_index('Form.Name',dataElements, "Can't determine column with FORM NAME", 'Please specify form name column number: '),
               findColumn_index('Dataset',dataElements, "Can't determine column with DATASET", 'Please specify form name column number: '),
               findColumn_index('Group', dataElements, "Can't determine column with DATAELEMENTGROUP", 'Please specify form name column number: ')
               
               
  )
  # CREATE THE CATEGORY COMBINATIONS -----------------------------------------------
  catCombos <- dataElements[,c(grep('Category', names(dataElements))),drop=F]
  catCombos[,1] <- revalue(catCombos[,1], c('None'='default'))
  catCombos <- catCombos[1:length(catCombos[,1][!is.na(catCombos[,1])]),, drop=F]
  catCombos <- catCombos[!duplicated(catCombos[,1]),,drop=F]
  
  # DATA ELEMENTS -----------------------------------------------------------------
  dataElements <- dataElements[,columns] 
  names(dataElements) <- c('dataElement','shortName','code', 'description', 'valueType', 'aggregationType', 'categoryCombo', 'formName', 'dataSet', 'dataElementGroup')
  dataElements <- dataElements[1:length(dataElements$dataElement[!is.na(dataElements$dataElement)]),]
  dataElements$categoryCombo <- revalue(dataElements$categoryCombo, c('None'='default'))
  
  # DATA ELEMENT GROUP
  # import the dataElementGroup names
  dataElementGroups <- XLConnect::readWorksheet(wb, 'Data Element Groups')
  names(dataElementGroups) <- c('name', 'shortName', 'aggregationType')
  # now find the matching dataElements from that page
  for (deg in dataElementGroups$name[!is.na(dataElementGroups$name)]) {
    de <- dataElements$dataElement[grep(deg, dataElements$dataElementGroup)]
    dataElementGroups$dataElements[dataElementGroups$name == deg] <- list(de[!is.na(de)])
    rm(de)
  }
  
  
  
  # DATA SETS ---------------------------------------------------------------------
  # this one is going to be slightly different as we need information on two pages
  dataSets <- XLConnect::readWorksheet(wb, 'Dataset')
  for (ds in dataSets$Dataset.Name[!is.na(dataSets$Dataset.Name)]) {
    de <- dataElements$dataElement[dataElements$dataSet == ds]
    dataSets$dataElements[dataSets$Dataset.Name == ds] <- list(de[!is.na(de)])
  }
  names(dataSets) <- c('dataSet', 'frequency', 'ou_level', 'attribute', 'catCombo', 'dataElements')
  
  # USER ROLES -------------------------------------------------------------------
  # userRoles <- XLConnect::readWorksheet(wb, 'User Roles')

  # USER INVITATION --------------------------------------------------------------
  
  
  config <- list('categoryOptions' = options, 'categories'= catOptions, 'categoryCombos' = catCombos,
                 'dataElements' = dataElements, 'dataElementGroups' = dataElementGroups, 'dataSets' = dataSets)
  
  cat("----- Scrape Completed -----\n")
  print(Sys.time() - startTime)
  cat('Summary:\n')
  summary <- as.data.frame.list(lapply(config, function(x) {nrow(x[1])}))
  print(summary)
  summary <- list(summary)
  names(summary) <- 'importSummary'
  return(append(config, summary))
  
  
}


cloneDHIS2_userRole <- function(id, usr, pwd, url, new_name) {
  # Clone an existing user role as a new role with new name
  # useful for copying exact priviliges and permissions
  # This ignores dataset associations or current user associations. 
  
  payload <- getDHIS2_elementInfo(id, 'userRoles', usr, pwd, url)
  
  payload <- payload[!(names(payload) %in% c('users','id','href', 'created', 'lastUpdated','dataSets'))]
  
  payload$name <- new_name
  resp <- postDHIS2_metaData(payload, 'userRoles', usr, pwd)
  return(resp)
}

# METADATA UPLOAD BASE FUNCTION --------------------------------------------------------------------
uploadDHIS2_metaData <- function(obj, obj_type, usr, pwd, url,overwrite=F, prompt=T, verbose=F) {
  # take objects from scrapeDHIS2_configFile() and create the appropriate configuration in the system.  this will check if the settings already exist in 
  # the system and will skip those unless overwrite=T.  if prompt or verbose are set to T, lots of text will output.  Otherwise, existing configs 
  # will be overwritten automatically and a simple status output will display.  THIS COULD BE DANGEROUS IF YOU ARE NOT CERTAIN THERE ARE NO OVERLAPPING CONFIGURATIONS
  cat('------------------ Starting',toupper(obj_type),'Upload -------------------------\n')
  # check category combinations that exist in the system
  existingObjects <- getDHIS2_Resource(obj_type, usr, pwd, url)
  # set up some empty counts and objects to return
  req <- list('updated' = list(), 'uploaded' = list(), 'skipped' = list())
  
  # check categories per combination are the same
  
  for (cc in 1:nrow(obj)) {
    # create the upload object.  We can take advantage of R's switch() function and use obj_type 
    # to determine how it will structure the upload object
    
    upload <- switch(obj_type,
                     'dataElements' = ({
                       dataElement <- obj[cc,,drop=F] # the row we're looking at
                       de_name <- dataElement[1,1] # the name
                       createDHIS2_DataElement(de_name, shortName = dataElement$shortName, aggregationType = dataElement$aggregationType,
                                               valueType = dataElement$valueType, categoryCombo = dataElement$categoryCombo, 
                                               description = dataElement$description, formName= dataElement$formName, 
                                               code= dataElement$code)
                     }),
                     'categoryCombos' = ({
                       catCombo <- obj[cc,] # the row we're looking at
                       cat_name <- catCombo[1,1] # the first column is the name
                       if (cat_name != 'default') {
                         cats <- catCombo[-1] %>% .[!is.na(.)] %>% .[!duplicated(.)] # remove the name column and any NA columns, for good measure, remove dupes
                         createDHIS2_CategoryCombo(cat_name, cats, shortName = cat_name) # make the 
                       }
                       else {
                         list('name' = 'default')
                       }

                     }),
                     'categories' = ({ # this follows the same format as above
                       category <- obj[cc,]
                       cat_name <- category[1,1]
                       opts <- category[,-1] %>% .[!is.na(.)] %>% .[!duplicated(.)]
                       createDHIS2_Category(cat_name, opts, shortName = cat_name)
                     }),
                     'categoryOptions' = ({
                       createDHIS2_CategoryOption(as.character(obj[cc,1]))
                     }),
                     'dataSets' = ({
                       ds <- obj[cc,]
                       createDHIS2_DataSet(ds$dataSet, dataElements = unlist(ds$dataElements), periodType = ds$frequency)
                     }),
                     'dataElementGroups' = ({
                       deg <- obj[cc,,drop=F]
                       createDHIS2_DataElementGroup(deg$name, deg$shortName, deg$aggregationType, dataElements= unlist(deg$dataElements))
                     }),
                     'translations' = ({
                       trans <- obj[cc,]
                       createDHIS2_translation(trans$value, trans$property, trans$locale, trans$objectId, trans$className)
                     }),
                     'users' = ({
                       user <- obj[cc,]
                       createDHIS2_user(user$firstName, user$surname, user$username, user$password, user$userRole, user$organisationUnit)
                       
                     })
    )
    
    if (upload$name %in% existingObjects$displayName == TRUE & overwrite == T & upload$name != "default" & obj_type != 'categoryOptions') {
      # does this name already exist in the system? if it does, 
      # and overwrite == T, prompt for permission on each catCombo that already
      # exists
      # get the id
      id <- existingObjects$id[existingObjects$displayName %in% upload$name] 
      # download the detailed information on the category combination
      # get the ids of the attached categories
      
      if (verbose == T | prompt == T) {
        object_relationship <- getDHIS2_objectChildren(id, obj_type, usr, pwd, url)
        # do the ids match? if so, answer will be TRUE
        cat("\nObject with that name already exists and has the following options:\n")
        print(object_relationship$parent)
        cat('Attempted children to upload:\n')
        print(object_relationship$children)
        ifelse(prompt==T, resp <- confirmAction('Overwrite existing configuration? Y/N: '), resp <- "Y")
      }
      else {
        resp <- "Y"
      }
      if (resp == "Y") {
        resp <- content(putDHIS2_metaData(upload, usr, pwd, paste0(url, obj_type, '/',id), verbose=verbose))
        req$updated %<>% append(., list(list('name' = upload$name, 'response' = list(resp))))
      }
      
    }
    else if (upload$name %in% existingObjects$displayName == FALSE) {
      # any categories that DO NOT exist at all will always be uploaded
      resp <- content(postDHIS2_metaData(upload, obj_type=obj_type, usr, pwd, url, verbose=verbose), type = 'application/json')
      req$uploaded %<>% append(., list(list('name' = upload$name, 'response' = list(resp))))
    }
    else {
      if (verbose==T) cat("Skipping!")
      req$skipped %<>% append(., list(list('name' = upload$name, 'response' = list(upload))))
    }
    Sys.sleep(runif(1, 0, .1))
    flush.console()
    ifelse(nchar(upload$name) > 40, extend <- "...                             ",extend <- "                                ")
    print_name <- stri_sub(paste0(stri_sub(upload$name, length=40),extend), length=45)
    cat('\rCURRENT-->', cc, "|", nrow(obj), '\tObject:', print_name ,'\tTOTALS--> Uploaded:', length(req$uploaded), 'Updated:', length(req$updated), 'Skipped:', length(req$skipped),rep('\t',15))
    
    # cat(c('\r',rep('\t',35),'\r'))
  }
  cat('\n----------------- Completed',toupper(obj_type),'Upload -------------------------\n')
  
  cat("Uploaded", length(req$uploaded), 'objects\n')
  cat("Updated", length(req$updated), 'already existing objects\n')
  cat("Skipped", length(req$skipped), 'already existing objects\n\n')
  
  return(req)
  
}

# ORGANISATION UNIT HIERARCHY
uploadDHIS2_orgUnitHierarchy <- function(file, usr, pwd, url) {
  # Scrape the config file for org unit related objects
  # this is going to be treated separately since creating the 
  # org units and relationships will require some back and forth 
  # with the api and won't work exactly like the aggregate upload
  # process
  worksheets <- c('organisationUnits', 'organisationUnitGroupSets', 'organisationUnitLevels') # these obviously have to match
  orgUnit_data <- lapply(worksheets, function(x) all_character(readWorkbook(file, x)))
  names(orgUnit_data) <- worksheets
  
  # create in this order: org units, groups, group sets, levels
  
  # org units first
  current_orgUnits <- getDHIS2_Resource('organisationUnits', usr, pwd, url)
  new_orgUnits <- orgUnit_data$organisationUnits
  if (any(current_orgUnits$displayName %in% new_orgUnits$name) == T) {
    warning('Existing organisation units in configuration file.  They will be skipped.')
    new_orgUnits <- new_orgUnits[!(new_orgUnits$name %in% current_orgUnits$displayName),]
  }
  
  # fill in parent ids if we have them
  current_ids <- current_orgUnits$id 
  names(current_ids) <- current_orgUnits$displayName
  new_orgUnits$parent <- revalue(new_orgUnits$parent, current_ids)
  
  for (ou in 1:nrow(new_orgUnits)) {
    # we're going to upload each record and then update our list as we go
    
  }
  
  
  createDHIS2_OrgUnit()
  
  responses <- list()
  for (l in 1:nrow(orgUnit_data$organisationUnitLevels)) {
    payload <- list('name' = orgUnit_data$organisationUnitLevels$name[l], 'level' = orgUnit_data$organisationUnitLevels$levels[l])
    resp <- postDHIS2_metaData(payload,'organisationUnitLevels', usr, pwd, url)
    reponses[['levels']] %<>% append(., list(resp))
  }
  
  # o
  
  # groups 
  
  
  
}
uploadDHIS2_orgHierarchy <- function()

# TRACKER SPECIFIC FUNCTIONS -----------------------------------------------------------------------
uploadDHIS2_trackerConfig <- function(tracker_config, usr, pwd, url) {
  # upload a tracker config object pulled from scrapeDHIS2_trackerConfigFile()
  
  # first thing, create the options and option sets
  options <- getDHIS2_Resource('options', usr, pwd, url)
  options_created <- list()
  for (i in tracker_config$options[!(tracker_config$options %in% options$displayName)]) {
    options_created <- append(options_created, list(postDHIS2_metaData(createDHIS2_option(i, i), 'options', usr, pwd, url )))
  }
  
  # Update the list of options
  options <- getDHIS2_Resource('options', usr, pwd, url)
  optionSets <- getDHIS2_Resource('optionSets', usr, pwd, url)
  
  optionSets_created <- list()
  for (i in 1:nrow(tracker_config$optionSets)) {
    optSet <- tracker_config$optionSets[i,]
    opts <- optSet[1,2:ncol(optSet)]
    opts <- opts[!is.na(opts)]
    opt_ids <- options[options$displayName %in% opts,]
    # double check there aren't dupes in the system
    if (nrow(opt_ids) != length(opts)) {
      cat("Multiple options available. Please specify.\nStated options:\n")
      print(opts)
      cat("Options matched from system:\n")
      rownames(opt_ids) <- 1:nrow(opt_ids)
      print(opt_ids)
      row <- readline('Please specify row number(s) in comma separated format (enter 0 to auto-select non-duplicates): ')
      row <- eval(parse(text=paste0('c(',row,')')))    
      if (row == 0) {
        opt_ids <- opt_ids$id[!duplicated(opt_ids$displayName)]  
      }
      else {
        opt_ids <- opt_ids$id[row]
      }
    }
    opt_ids <- lapply(opt_ids, function(x) list('id' = x))
    
    
    optSet_name <- optSet[1,1]
    
    if (optSet_name %in% optionSets$displayName) {
      # check if there's already an option set with this name
      # decide what to do about it.
      cat('Option Set with name', optSet_name, 'already exists!\n')
      existing_id <- optionSets$id[optionSets$displayName == optSet_name]
      info <- getDHIS2_objectChildren(existing_id, 'optionSets', usr, pwd, url)
      print(info)
      decision <- confirmAction('Use existing optionSet? (Y/N) ')
      
      if (decision == "N") {
        resp <- "N"
        while (resp == "N") {
          rename <- readline('Please enter a new name: ')
          cat("New name:", rename,'\n')
          resp <- confirmAction("Is this correct? ")
        }
        new_os <- postDHIS2_metaData(createDHIS2_optionSet(resp, opt_ids), 'optionSets', usr, pwd, url)
        optionSets_created <- append(optionSets_created, list(new_os))
        
        # now we'll just take the id out of the response
        new_os <- new_os$response$lastImported
      }
      else {
        # use the current one if no replacement
        new_os <- existing_id
      }
    }
    else {
      new_os <- postDHIS2_metaData(createDHIS2_optionSet(optSet[1,1], opt_ids), 'optionSets', usr, pwd, url)
      optionSets_created <- append(optionSets_created, list(new_os))
      new_os <- new_os$response$lastImported
    }
    # last thing, let's start filling in the ids now in the other places of the 
    # config
    tracker_config$trackedEntityAttributes$optionSet[tracker_config$trackedEntityAttributes$optionSet == optSet_name] <- new_os
    tracker_config$programStageDataElements$Option.Set[tracker_config$programStageDataElements$Option.Set == optSet_name] <- new_os

  }
  
  # Now let's check the attributes 
  teiAttributes <- getDHIS2_Resource('trackedEntityAttributes', usr, pwd, url)
  teiAtt_created <- list()
  for (i in 1:nrow(tracker_config$trackedEntityAttributes)) {
    att <- tracker_config$trackedEntityAttributes[i,]
    att_name <- att$name
    if (att$name %in% teiAttributes$displayName) {
      # check if there's already an attribute with this name
      # decide what to do about it.
      cat('Attribute with name', att_name, 'already exists!\n')
      existing_id <- teiAttributes$id[teiAttributes$displayName == att_name]
      info <- getDHIS2_objectChildren(existing_id, 'trackedEntityAttributes', usr, pwd, url)
      print(info)
      decision <- confirmAction('Use existing attribute? (Y/N) ')
      
      if (decision == "N") {
        resp <- "N"
        while (resp == "N") {
          rename <- readline('Please enter a new name: ')
          cat("New name:", rename,'\n')
          resp <- confirmAction("Is this correct? ")
        }
        new_att <- postDHIS2_metaData(createDHIS2_trackedEntityAttribute(resp, att$shortName, att$aggregationType,
                                                                        att$valueType, att$optionSet), 'trackedEntityAttributes', usr, pwd, url)
        optionSets_created <- append(teiAtt_created, list(new_att))
        
        # now we'll just take the id out of the response
        new_att <- new_att$response$lastImported
      }
      else {
        # use the current one if no replacement
        new_att <- existing_id
      }
    }
    else {
      new_att <- postDHIS2_metaData(createDHIS2_trackedEntityAttribute(att_name, att$shortName, att$aggregationType,
                                                                       att$valueType, att$optionSet), 'trackedEntityAttributes', usr, pwd, url)
      optionSets_created <- append(optionSets_created, list(new_att))
      new_att <- new_att$response$lastImported
    }
    
    # fill in the program ids    
    replace <- which(tracker_config$programs == att_name, T)
    for (j in 1:nrow(replace)) {
      tracker_config$programs[replace[j,'row'], replace[j,'col']] <- new_att
    }
  }
  
  # now create the program. it will be assumed that the sheet is used for one program at a time.
  programs <- getDHIS2_Resource('programs', usr, pwd, url)
  
  if (nrow(tracker_config$programs) > 1) {stop('This is only intended to configure one program at a time!')}
  
  prog <- tracker_config$programs[1,]
  prog_name <- prog$name

  
  check <- checkDHIS2_objectExists(prog_name, 'programs', usr, pwd, url)
  if (check$exist == T) {
    if (check$use_existing == "Y") {
      prog_id <- check$existing_id
    }
    else {
      prog_resp <- postDHIS2_metaData(createDHIS2_program(check$new_name,prog$shortName,prog$description, prog$trackedEntity, prog$attributes), 'programs', usr, pwd, url)      
    }
  }
  
  
}



scrapeDHIS2_trackerConfigFile <- function(filename) {
  # Going to write this one with openxlsx since it seems to be more efficient than XLConnect's functions
  # Import a file with Tracker configuration info
  
  worksheets <- c('Program', 'Option Sets', 'Attributes', 'Stages', 'Data Elements')
  
  config <- lapply(worksheets, function(x) openxlsx::readWorkbook(filename, x))
  names(config) <- c('programs', 'optionSets', 'trackedEntityAttributes', 'programStages', 'programStageDataElements')
  
  # drop some of the extra rows thanks to the config file shortname check
  config$programStageDataElements <- config$programStageDataElements[!is.na(config$programStageDataElements[,1]),]
  
  # add options object to be created before option sets
  options <- unlist(config$optionSets[,2:ncol(config$optionSets)])
  config$options <- options[!is.na(options)]
  
  return(config)

}

checkDHIS2_objectExists <- function(name, obj_type, usr, pwd, url) {
  # To be used for config uploads.  Check if an object name exists
  # in the current configuration. Display relevant info about it
  # and prompt to use same or replace.
  existing_objs <- getDHIS2_Resource(obj_type, usr, pwd, url)
  check <- list('name' = name, 'exists' = F, 'use_existing' = NA,
                'new_name' = NA, 'existing_id' = NA)
  
  if (name %in% existing_objs$displayName) {
    check$exists <- T
    # check if there's already an ob with this name
    # decide what to do about it.
    cat('Object with name', name, 'already exists!\n')
    existing_id <- existing_objs$id[existing_objs$displayName == name]
    check$existing_id <- existing_id
    info <- getDHIS2_objectChildren(existing_id, obj_type, usr, pwd, url)
    print(info)
    decision <- confirmAction('Use existing object? (Y/N) ')
    check$use_existing <- decision
    if (decision == "N") {
      resp <- "N"
      while (resp == "N") {
        rename <- readline('Please enter a new name: ')
        cat("New name:", rename,'\n')
        resp <- confirmAction("Is this correct? ")
      }
      check$new_name <- rename
    }
  }

  return(check)
}


transferDHIS2_data <- function(usr.from, pwd.from, url.from, usr.to, pwd.to, url.to, 
                               parent_ous = NULL, specific_dataSets = NULL, yearly_to_monthly=T,
                               startDate = Sys.Date() - months(6), endDate= Sys.Date() + years(1), existing_upload_file=NA) {
  # Pull data from one dhis2 and post to another.  If specific_dataSets is specified, it will take each character vector
  # element and find those dataSets from the source dhis2 instance and attempt to download.  If NULL, it will attempt all.
  # this only works when uuids match for BOTH systems.  Otherwise it will kick errors. 
  
  if(!dir.exists('temp/')) dir.create('temp')
  library(lubridate)
  
  from.ds <- getDHIS2_Resource('dataSets', usr.from, pwd.from, url.from, 'periodType')
  upload_results <- list()
  
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

        write.csv(d, paste0('temp/', from.ds$displayName[i], ".csv"), row.names = F)
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









