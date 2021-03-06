library(XLConnect)
library(stringi)
library(magrittr)
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
generateDHIS2_configFile <- function(dataSet, usr, pwd, url, filename) {
  # Download all data related to a specific data set and it's dependencies
  # place in the same format as the excel config file for later uploading
  wb <- createWorkbook()
  # data set first
  addWorksheet(wb, 'Dataset')
  dataSets <- getDHIS2_Resource('dataSets', usr, pwd, url, 'periodType')
  dataSet <- getDHIS2_elementInfo(dataSets$id[dataSets$displayName == dataSet], 'dataSets', usr, pwd, url)
  writeData(wb, 'Dataset', data.frame('Dataset Name' = dataSet$displayName, 
                            'Frequency' = dataSet$periodType, 
                            'OU Level' = NA, 
                            'Data Set Attribute' = NA, 
                            'CatCombo' = NA))
  # data elements next
  de <- lapply(dataSet$dataSetElements, function(x) {
    tmp <- getDHIS2_elementInfo(x$dataElement$id, 'dataElements', usr, pwd, url)
    list('dataElement' = tmp,'categoryCombo' = getDHIS2_elementInfo(tmp$categoryCombo$id, 'categoryCombos', usr, pwd, url))
    })
  
  de_to_write <- rbind.fill.matrix(lapply(de, function(x) {
    tmp <- data.frame('Data Element Name' = ifelse(is.null(x$dataElement$displayName), NA, x$dataElement$displayName), 
               'Short Name' = ifelse(is.null(x$dataElement$shortName), NA, x$dataElement$shortName),
               'Code' = ifelse(is.null(x$dataElement$code), NA, x$dataElement$code),
               'Description' = ifelse(is.null(x$dataElement$description), NA, x$dataElement$description),
               'Form Name' = ifelse(is.null(x$dataElement$formName), NA, x$dataElement$formName),
               'Dataset' = dataSet$displayName,
               'Data Element Group' = dataSet$displayName, # Intentionally leaving this as the same as the data set for now
               'Value Type' = ifelse(is.null(x$dataElement$valueType), NA, x$dataElement$valueType),
               'Aggregation Type' = ifelse(is.null(x$dataElement$aggregationType), NA, x$dataElement$aggregationType),
               'Category Combo Name' = x$categoryCombo$displayName
               )
    if (x$categoryCombo$displayName != 'default') {
      cats <- sapply(x$categoryCombo$categories, function(y) y$id)
      tmp <- cbind(tmp, t(cats))
      suppressWarnings(names(tmp)[11:ncol(tmp)] <- paste0("Category.", 1:length(cats), '.Name'))
    }
    tmp
  }))
  # categories
  cats <- unique(de_to_write[,grep('Category', colnames(de_to_write))[-1]]) %>% .[!is.na(.)]
  
  cats <- lapply(cats, function(x) getDHIS2_elementInfo(x, 'categories', usr, pwd, url))
  categories <- rbind.fill.matrix(lapply(cats, function(x) {
    options <- sapply(x$categoryOptions, function(y) y$id)
    tmp <- cbind('Category Name' = x$name, t(options))
    colnames(tmp) <- c('Category Name', paste0('CategoryOptions', 1:length(options)))
    tmp
  }))
  
  catOptions <- getDHIS2_Resource('categoryOptions', usr, pwd, url)
  cat_opt_replace <- catOptions$displayName
  names(cat_opt_replace) <- catOptions$id
  categories %<>% apply(2, function(x) plyr::revalue(x, cat_opt_replace, warn_missing=F))
  
  cat_names <- sapply(cats, function(x) {y <- x$displayName;names(y) <- x$id; y})
  de_to_write[,grep('Category', colnames(de_to_write))] %<>% apply(2, function(i) plyr::revalue(i, cat_names,warn_missing = F))
  
  addWorksheet(wb, 'Data Elements')
  writeData(wb, 'Data Elements', as.data.frame(de_to_write))
  addWorksheet(wb, 'Category Options')
  writeData(wb, 'Category Options', as.data.frame(categories))
  
  addWorksheet(wb, 'Data Element Groups')
  writeData(wb, 'Data Element Groups', data.frame('Data Element Group Name' = dataSet$displayName, 'Short Name' = dataSet$displayName, 'Aggregation Type' = 'SUM'))
  
  if (missing(filename)) filename <- tempfile()
  
  saveWorkbook(wb, filename, overwrite=T)
  return(filename)
  
}


generateDHIS2_metaData <- function(filename, usr, pwd, url, overwrite=F, prompt_overwrite=T, verbose=F, object=NA) {
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
  
  cat('Converting imported data to metaData format...\n')
  objs <- names(config)
  for (i in 1:length(config)) {
    cat(objs[i], '\t')
    if (nrow(config[[i]]) > 0) {
      config[[i]] <- lapply(1:nrow(config[[i]]), function(x) convert_to_metaData(config[[i]][x,], objs[i]))
      
    }
    else {
      config[[i]] <- list()
    }
  }
  
  
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

convert_to_metaData <- function(obj, obj_type) {
  cc <- 1
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
                     createDHIS2_CategoryOption(as.character(obj[cc,]))
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
  return(upload)
}

scrapeDHIS2_configFile <- function(filename, usr, pwd, url, warn=T, upload=T) {
  # Take a meta data config file, scrape the data off 
  # for now assuming that the file has all of the necessary tabs a
  # add_ids will determine if the script should create new system ids 
  # for the uploaded objects
  if (warn) {
    cat('Scraping Excel config file\n
      IMPORTANT: After importing, you will need to update and edit \n
      existing metaData objects within DHIS2 or use more complex methods\n
      for changes.  It is not possible to reimport a configuration file\n
      with changes after it has already been imported the first time!\n\n')
    ans <- confirmAction('Proceed with the process? (Y/N) ')
    if (ans == "N") stop('Import canceled.')
  }

  # Ex.
  # > scrapeDHIS2_configFile('meta-data-config.xlsx')
  # Config scraped in 1.3 seconds
  # dataElements  categories  categoryOptions ...
  # 45            12          50
  
  startTime <- Sys.time()
  
  # Load all of the options we're working with
  catOptions <- readWorkbook(filename, 'Category Options')
  
  # CREATE OPTIONS -----------------------------------------------------------
  # take just the options listed, remove na values, and duplicates to just
  # return the unique options we're working with
  if (nrow(catOptions) > 1) {
    options <- catOptions[,-1] %>% .[!is.na(.)] %>% .[!duplicated(.)]
    cOptions <- all_character(data.frame('name' = options))
    cOptions <- check_ids(cOptions, 'categoryOptions', usr, pwd, url)
    
    categoryOptions <- list()
    for (i in 1:nrow(cOptions)) {
      obj <- createDHIS2_CategoryOption(cOptions$id[i], cOptions$name[i])
      if (cOptions$existing[i]) obj %<>% add_href('categoryOptions', url)
      categoryOptions %<>% append(list(obj))
    }
    
    # CREATE CATEGORIES --------------------------------------------------------
    # still contained in catOptions (confusing).  Revalue all names with ids
    opts <- make_revalue_map(cOptions$name, cOptions$id)
    catOptions[,-1] %<>% apply(2, function(x) plyr::revalue(x, opts, warn_missing=F))
    
    # get new ids
    names(catOptions)[1] <- 'name' 
    catOptions <- check_ids(catOptions, 'categories', usr, pwd, url)
    
    
    # convert to lists
    categories <- list()
    opts <- greps(c('id', 'name', 'existing'), names(catOptions))
    for (i in 1:nrow(catOptions)) {
      obj <- createDHIS2_Category(catOptions[i, 'id'], catOptions[i, 'name'], catOptions[i, -opts])
      if (catOptions$existing[i]) obj %<>% add_href('categories', url)
      categories %<>% append(list(obj))
    }
    
  }
  else {
    categoryOptions <- list()
    categories <- list()
  }
  
  
  # Now do the more complicated stuff
  dataElements <- readWorkbook(filename,  "Data Elements")
  columns <- c(findColumn_index('Data.Element.Name', dataElements, "Can't determine column with DATA ELEMENT name!", 'Please specify data element column number: '),
               findColumn_index('Short.Name', dataElements, "Can't determine column with SHORT NAME!", 'Please specify short name column number: '),
               findColumn_index('Code', dataElements, "Can't determine column with CODE!", 'Please specify short name column number: '),
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
  
  default_categoryCombo <- getDHIS2_Resource('categoryCombos', usr, pwd, url) %>% .[grep('default', .$displayName), 'id']
  catCombos[,1] <- plyr::revalue(catCombos[,1], c('None'='default'), warn_missing = F)
  catCombos[,1][is.na(catCombos[,1])] <- 'default'
  names(catCombos)[1] <- 'name'
  catCombos <- check_ids(catCombos, 'categoryCombos', usr, pwd, url)
  
  
  catCombos$id[grepl('default', catCombos[,1])] <- default_categoryCombo
  categoryCombo_ids <- catCombos$id # will use this with dataElement creation

  catCombos <- catCombos[-grep(default_categoryCombo, catCombos$id),,drop=F] %>% .[!duplicated(.$name),]
  
  categoryCombos <- list()
  
  
  if (nrow(catCombos) > 0) {
    cats <- make_revalue_map(catOptions$name, catOptions$id)
    catCombos[,-c(1, ncol(catCombos))] %<>% apply(2, function(x) plyr::revalue(x, cats, warn_missing = F))
    for(i in 1:nrow(catCombos)) {
      # apply has some weird issues with this, so explicit for loop
      obj <- createDHIS2_CategoryCombo(catCombos$id[i], catCombos$name[i], catCombos[i,-c(1, ncol(catCombos))])
      if (catCombos$existing[i]) obj %<>% add_href('categoryCombos', url)
      categoryCombos %<>% append(list(obj))
    }
  }
  

  
  
  # DATA ELEMENTS -----------------------------------------------------------------
  dataElements <- dataElements[,columns] 
  names(dataElements) <- c('name','shortName','code', 'description', 'valueType', 'aggregationType', 'categoryCombo', 'formName', 'dataSet', 'dataElementGroup')
  dataElements$categoryCombo <- categoryCombo_ids
  dataElements %<>% check_ids('dataElements', usr, pwd, url)
  
  # DATA ELEMENT GROUP
  # import the dataElementGroup names
  deGroups <- readWorkbook(filename,  'Data Element Groups')
  names(deGroups) <- c('name', 'shortName', 'aggregationType')
  deGroups %<>% check_ids('dataElementGroups', usr, pwd, url)

  # now find the matching dataElements from that page
  dataElementGroups <- list()
  for (deg in 1:nrow(deGroups)) {
    de <- dataElements$id[grep(deGroups$name[deg], dataElements$dataElementGroup)]
    obj <- createDHIS2_DataElementGroup(deGroups$id[deg], deGroups$name[deg], dataElements = de)
    if (deGroups$existing[deg]) obj %<>% add_href('dataElementGroups', url)
    dataElementGroups %<>% append(list(obj))
  }
  
  dataElements$dataElementGroup %<>% plyr::revalue(make_revalue_map(deGroups$name, deGroups$id), warn_missing=F)
  
  # DATA SETS ---------------------------------------------------------------------
  # this one is going to be slightly different as we need information on two pages
  dSets <- readWorkbook(filename,  'Dataset')
  names(dSets) <- c('name','frequency', 'ou_level', 'attribute', 'catCombo', 'description')[1:ncol(dSets)]
  dSets %<>% check_ids('dataSets', usr, pwd, url)
  
  dataSets <- list()
  for (ds in 1:nrow(dSets)) {
    de <- dataElements$id[grep(dSets$name[ds], dataElements$dataSet)]
    obj <- createDHIS2_DataSet(dSets$id[ds], dSets$name[ds], periodType = dSets$frequency, description= dSets$description, dataElements = de)
    if (dSets$existing[ds]) obj %<>% add_href('dataSets', url)
    dataSets %<>% append(list(obj))
  }
  
  dataElements$dataSet %<>% plyr::revalue(make_revalue_map(dSets$name, dSets$id), warn_missing=F)
  
  de <- list()
  for (i in 1:nrow(dataElements)) {
    obj <- createDHIS2_DataElement(dataElements$id[i], dataElements$name[i], dataElements$shortName[i],
                                                dataElements$code[i], dataElements$description[i], valueType=dataElements$valueType[i],
                                                aggregationType = dataElements$aggregationType[i], categoryCombo= dataElements$categoryCombo[i],
                                                formName= dataElements$formName[i])
    if (dataElements$existing[i]) obj %<>% add_href('dataElements', url)
    de %<>% append(list(obj))
  }
  dataElements <- de
  
  # USER ROLES -------------------------------------------------------------------
  # userRoles <- readWorkbook(filename,  'User Roles')

  # USER INVITATION --------------------------------------------------------------
  
  
  config <- list('categoryOptions' = categoryOptions, 'categories'= categories, 'categoryCombos' = categoryCombos,
                 'dataElements' = dataElements, 'dataElementGroups' = dataElementGroups, 'dataSets' = dataSets)
  
  cat("----- Scrape Completed -----\n")
  print(Sys.time() - startTime)
  cat('Summary:\n')
  summary <- as.data.frame.list(lapply(config, function(x) {length(x)}))
  print(summary)
  cat('\nUse the following to post to the server:\n\n')
  cat('\tresponses <- uploadDHIS2_metaData(object_you_just_imported, usr, pwd, url) \n\n')
  return(config)
  
  
}

check_ids <- function(scraped_obj, obj_type, usr, pwd, url) {
    # requires a name and id field in scraped_obj
  # if (!any(grepl('id', names(scraped_obj)))) scraped_obj$id <- NA
  
  obj_resource <- getDHIS2_Resource(obj_type, usr, pwd, url)
  
  scraped_obj <- merge(scraped_obj, obj_resource[,c('displayName', 'id')], by.y='displayName', by.x='name', all.x=T)
  
  # scraped_obj$id <- plyr::revalue(scraped_obj$name, make_revalue_map(obj_resource$displayName, obj_resource$id), warn_missing = F,)
  scraped_obj$existing <- !is.na(scraped_obj$id)
  if (any(is.na(scraped_obj$id))) {
      scraped_obj$id[is.na(scraped_obj$id)] <- getDHIS2_systemIds(table(is.na(scraped_obj$id))["TRUE"], usr, pwd, url)
  }
  return(scraped_obj)
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
uploadDHIS2_metaData <- function(config, usr, pwd, url) {
  # take objects from scrapeDHIS2_configFile() and create the appropriate configuration in the system.  
  # this performs no sanity checks and relies on dhis2 to handle the import process
  
  req <- lapply(names(config), function(x) {
    # if (length(config[[x]]) > 1) {
      # lapply(config[[x]], function(y) {
      #   cat('\r', y$name, rep(' ', 50)) 
      #   # if (any(grepl('href', names(y)))) putDHIS2_metaData(y, usr, pwd, y$href)
      #    x <- postDHIS2_metaData(y, x, usr, pwd, url) 
      #    print(x$status_code)
      #    x
      # })
  
    r <- postDHIS2_metaData(config[x], 'metadata', usr, pwd, url)
    cat(x, r$status_code)
    r
    # }
    # else if (length(config[[x]]) == 1) {
    #   cat('\r', config[[x]][[1]]$name, rep(' ', 50)) 
    #   # if (any(grepl('href', names(config[[x]][[1]])))) putDHIS2_metaData(config[[x]][[1]], usr, pwd, config[[x]][[1]]$href)
    #    x <- postDHIS2_metaData(config[[x]][[1]], x, usr, pwd, url)
    #    print(x$status_code)
    #    x
    # }
  })
  
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
  new_orgUnits$parent <- plyr::revalue(new_orgUnits$parent, current_ids, warn_missing = F)
  
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










