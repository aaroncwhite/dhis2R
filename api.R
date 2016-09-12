library(httr)
library(parallel)
library(foreach)

# API INTERACTIONS -----------------------------------------------------------------------
# Contains a series of wrapper functions to ease interaction with the dhis2 API.  These functions
# should be used in conjunction with those found in payload_creation.R to properly format
# upload objects when attempting to create new configurations or upload data.
# this is the foundation for the rest of the configuration files. 


## GET ------------------------------------------------------------------------------------
getDHIS2_dataSet <- function(dataSet, orgUnit, start, end, usr, pwd, children="true", url, type='json') {
  # download the requested data set
  # lookup the data set and org unit
  dataSet_table <- getDHIS2_Resource('dataSets', usr, pwd, url)
  dataSet <- dataSet_table$id[dataSet_table$displayName == dataSet]
  if (length(dataSet) == 0) {stop('Check dataSet name')}
  
  orgUnit_table <- getDHIS2_Resource('organisationUnits', usr, pwd, url)
  orgUnit <- orgUnit_table$id[orgUnit_table$displayName == orgUnit]
  if (length(orgUnit) ==0) {stop('Check orgUnit name')}
  
  # build the url
  url <- paste0(url, 'dataValueSets.',type,'?dataSet=',dataSet,'&orgUnit=',orgUnit,'&startDate=',start, '&endDate=',end,'&children=',children)
  
  resp <- GET(url, authenticate(usr, pwd, 'basic'))
  
  cat('Status:', resp$status_code, '\n')
  
  if (type=='csv') {
    resp <- content(resp,'parsed', 'text/csv')
    names(resp) <- c('dataElement', 'period', 'orgUnit', 'categoryOptionCombo', 'attributeOptionCombo', 'value', 'storedBy', 'lastUpdated', 'comment', 'followup')
    return(resp)
  } 
  else {
    return(content(resp))
    
  }

}

getDHIS2_Resource <- function(resourceName, usr, pwd, url, add_props=c()) {
  # creates the appropriate url from which to fetch information
  
  resource <- getDHIS2_Request(usr, pwd, url= paste0(url, resourceName), add_props = add_props)
  
  
  return(resource)
}



getDHIS2_Request <- function(usr, pwd, url, add_props=c()) {
  ## This will take a specific api url for dataElements or categoryOptions, etc
  ## and return a table with all information available.
  ## This only works well for the main resource tables.  Specific data element
  ## or category information is nested, so it's not as well suited. 
  
  if (length(add_props)>0) {add_props <- paste0(",",add_props, collapse='')}
  
  url <- paste0(url, '.json?fields=displayName,id,shortName,code',add_props, '&paging=false') # we dont' want to page the file
  req <- GET(url, authenticate(usr, pwd, type='basic')) # hit the server
  req_content <- content(req)[[1]] # take the response data from the info we got back from the server
  
  # build the table that as as many rows as there are options and has as many columns as each option contains
  # this is confusing.  for example: if the response returns 110 data elements, the table will have 110 rows
  # then, each data element has 5 components, so we'll make 5 columns and populate the table accordingly
  output <- data.frame(matrix(nrow=length(req_content), ncol=0)) 
  if (length(req_content) == 0) {
    output <- data.frame(matrix(nrow=0, ncol=2))
    names(output) <- c('displayName', 'id')
  }
  else {
    for (i in 1:length(req_content)) { # look at each resource element in the response
      # some elements have codes, some don't. this causes problems.
      row <- unlist(req_content[[i]])
      for (e in 1:length(row)) {
        output[i,names(row)[e]] <-  row[e] # 'unlist' it, so it's just a vector of values and stick it into that row
      }
    }  
    # names(output) <- names(req_content[[1]]) # give it the names from the server
    
  }

  return(output)
}

getDHIS2_ResourceTable <- function(usr, pwd, url) {
  # search for a specific resource type (data element, category option, etc)
  # return url for further parsing
#   url <- paste0(url, 'api/?paging=false')
  req <- GET(url,
             authenticate(usr,pwd, type='basic')
  )
  #stop_for_status(req)
  resources <- content(req)$resources
  # this list is unnamed
  # run through and find the actual names
  rsrcs <- data.frame(matrix(nrow=length(resources), ncol=4))
  for (i in 1:length(resources)) {
    rsrcs[i,] <- unlist(resources[i])
  }
  
  names(rsrcs) <- names(resources[[1]])
  return(rsrcs[order(rsrcs$plural),])
}

getDHIS2_elementInfo <- function(id, type, usr, pwd, url) {
  # uses the urls from get Resource table and resource to pull further information
  # use to pull data element info on combo options, etc
  # type and id must be exact
  
  req <- GET(paste0(url,"/", type,"/", id), authenticate(usr, pwd, type='basic'))
  return(req)
}

getDHIS2_objectChildren <- function(obj_id, obj_type, usr, pwd, url) {
  # Download element info for a specific object and return the relevant
  # child objects for use elsewhere. Returns a named list with 'parent' and 'children' 
  # elements. 
  # Ex.
  # > getDHIS2_objectChildren('n6sRE49hGrQ', 'categoryCombos', usr, pwd, url)
  # $parent
  # [1] 'Category Combination Name'
  # $children
  # [1] 'Category1' 'Category2'
  
  map <- list('dataElements' = 'categoryCombo',
              'categoryCombos' = 'categories',
              'categories' = 'categoryOptions',
              'optionSets' = 'options',
              'trackedEntityAttributes' = 'optionSet',
              'programs' = 'trackedEntityAttributes',
              'users' = 'userRoles')
  # look up the parent data
  parent_data <- content(getDHIS2_elementInfo(obj_id, obj_type, usr, pwd, url))
  child_type <- map[[obj_type]]
  child_id <- unlist(parent_data[[child_type]])
  
  # This is kind of funky, but the data element uses a singular, we need plural to look up children
  if (stri_sub(child_type, -1) != 's') {child_type <- paste0(child_type,'s')}
  
  # look up the child name
  child_names <- lapply(child_id, function(x) content(getDHIS2_elementInfo(x, child_type, usr, pwd, url))[c('displayName', 'id')])
  child_names <- list_to_df(lapply(child_names, function(x) as.data.frame(t(unlist(x))))) %>% all_character()
#   if (!is.null(child_names)) {
#     names(child_names) <- rep(child_type, length(child_names))
#   }
  parent_name <- parent_data$displayName
  names(parent_name) <- obj_type
  
  return(list('parent' = parent_name, 'children' = child_names))
}

getDHIS2_translationValues <- function(obj_id, target_lang, usr, pwd, url) {
  # given a specific object id, return all of the associated translations with that object for the target language
  response <- queryDHIS2(paste0(obj_id,"&filter=locale:eq:",target_lang), 'objectId', 'translations', usr, pwd, url)
  # convert the values into a table
  response <- list_to_df(lapply(response, function(x) as.data.frame.list(x[names(x) %in% c('href', 'id', 'className', 'locale', 'externalAccess', 'property', 'value', 'objectId')])))
  
  return(response)
}

# PUT ----------------------------------------------------------------------------------------
putDHIS2_metaData <- function(upload, usr, pwd, url, verbose=T) {
  # put meta data to DHIS2 instance.  this is for updating existing objects
  # you must already have the appropriate href. this will update the object in 
  # it's entirety.

  if (verbose==T) cat('Updating')
  req <- PUT(url, authenticate(usr, pwd), body= upload, encode='json')
  
  flush.console()
  return(req)
  
}

# PUT ----------------------------------------------------------------------------------------
patchDHIS2_metaData <- function(upload, usr, pwd, url, verbose=T) {
  # patch meta data to DHIS2 instance.  this is for updating existing objects
  # you must already have the appropriate href.  this will update only the named
  # elements sent in the payload. 
  
  if (verbose==T) cat('Updating')
  req <- PATCH(url, authenticate(usr, pwd), body= upload, encode='json')
  
  flush.console()
  return(req)
  
}

# POST ---------------------------------------------------------------------------------------
postDHIS2_metaData <- function(obj, usr, pwd, url, type='dataElements', verbose=T) {
  # post meta data to DHIS2 instance
  # find the correct resource link

  req <- POST(paste0(url,type), authenticate(usr, pwd), body= obj, encode='json')
    
  flush.console()
  return(req)
    
}

postDHIS2_Values <- function(df, splitBy, usr, pwd, url, type='dataValueSets') {
  if (nrow(df) < splitBy) {
    split <- cbind(1, nrow(df))
  }
  else {
    split <- calcSplits(df, splitBy)
    
  }
  results <- data.frame(matrix(nrow=0, ncol=5))
  url <- paste0(url, 'dataValueSets')
  chunks <- list()
  for (i in 1:nrow(split)) {
    upload <- df[split[i,1]:split[i,2],]
    upload <- upload[!is.na(upload$dataElement),]
    chunk <- list(upload)
    conflicts <- list()
    cat('\rSegment:', i, 'of', nrow(split))
    req <- POST(url,authenticate(usr, pwd), body= list("dataValues" = upload), encode='json')
    results <- rbind(results, unlist(content(req)$importCount))
    cat('\tStatus:', content(req)$description)
    if ("conflicts" %in% names(content(req))) {
      conflicts <- list(content(req))
    }
    flush.console()
    # Sys.sleep(runif(1, 0, .2))
    chunks <- append(chunks, list(list('data' = chunk, 'conflicts' = conflicts)))
  }
  cat('\n')
  names(results) <- names(unlist(content(req)$importCount))
  print(colSums(results))
  return(list('results' = results, 'chunks' = chunks))
}

# DELETE --------------------------------------------------------------------------------------
deleteDHIS2_objects <- function(obj_names=NA, ids=NA, obj_types, usr, pwd, url, prompt=T, prune=F) {
  # takes a vector of named objects to search for and then posts delete api calls
  # first download the appropriate table.  If you pass a vector of strs in obj_names
  # function will look for matches.  if you pass ids, it will just find the matches without
  # prompting which to select.  Both confirm deletion.
  results <- list()
  final_table <- data.frame()
  for (obj_type in obj_types) {
    cat(obj_type, '\n')
    obj_table <- getDHIS2_Resource(obj_type, usr, pwd, url)
    # now search for the name
    if (!is.na(obj_names[1]) & is.na(ids[1])) {
      obj_table <- obj_table[obj_table$displayName != "default",]
      obj_table <- obj_table[greps(obj_names, obj_table$displayName),,drop=FALSE]
      
      if (nrow(obj_table) == 0) {
        cat('Nothing found!\n')
      }
      else if (nrow(obj_table) != length(obj_names)) {
        rownames(obj_table) <- c(1:nrow(obj_table))
        print( obj_table[,c('displayName','id')])
        row <- readline('Multiple matches!  Please specify row number(s) in comma separated format (enter 0 to skip): ')
        row <- eval(parse(text=paste0('c(',row,')')))
        obj_table <- obj_table[row,,drop=FALSE]
      }

    }
    else if (is.na(obj_names[1]) & !is.na(ids[1])) {
      obj_table <- obj_table[obj_table$id %in% ids,, drop=FALSE]
      # the way this is set up conflicts with some things like enrollments
      # tracked entity instances etc
      if (nrow(obj_table) == 0) {
        obj_table <- as.data.frame(cbind('displayName' = ids, 'id' = ids))
        obj_table$displayName %<>% as.character(.)
        obj_table$id %<>% as.character(.)
      }
    }
    else {
      stop("Something went wrong.  You can only pass obj_names or ids NOT both.\n")
    }
    
    
    

    if (nrow(obj_table) == 0) {resp <- "N"}
    else if (prompt == T) {
      cat('About to delete the following', obj_type, 'values:\n')
      print(obj_table[,names(obj_table) %in% c('displayName', 'id')])
      resp <- readline('Is this OK? Please answer Y or N:')
      while (resp != "Y" & resp != "N") {
        resp <- readline('Invalid response. Please answer Y or N: ')
      }
    }
    else {resp <- "Y"}
    
  
    
    
    if (resp == "Y") {

      
      
      obj_table$status <- NA
      obj_table$message <- NA
      resp <- NULL
      reqs <- list()
      pruned <- list()
      # send the delete request for each stated object
      for (i in 1:nrow(obj_table)) {
        cat('\r Deleting:', obj_table$displayName[i], 'Status:')
        
        # if prune == T, remove any value audits or anything that might get in the way 
        # of deleting the object
        
#         if (prune == T) {
# #             resp <- POST(paste0(url, 'maintenance/dataPruning/', obj_type,"/", obj_table$id[i]), authenticate(usr, pwd, type='basic'))
#             POST(paste0(url, 'maintenance/dataElements/DtyGiExGSgj'), authenticate(usr, pwd, type='basic'))
#             
#             pruned <- append(pruned, resp)
#         }
        
        req <- DELETE(as.character(paste0(url,obj_type,"/",obj_table$id[i])), authenticate(usr, pwd, type = 'basic'))
        obj_table$status[i] <- req$status_code
        cat(req$status_code) # print the status code and then clear the line
        req <- list(req)
        names(req) <- obj_table$displayName[i]
        reqs <- append(reqs, req)
        cat(rep('\r ', 150))
        flush.console()
      }
      cat('\n')
      obj_table$obj_type <- obj_type
      results <- append(results, reqs)
      final_table <- rbind.fill(final_table, obj_table)
    }
    
    else {
      cat('\rDelete skipped\n')
      flush.console()
    }
  }
  
  if (nrow(final_table) > 0) {
    return(list('summary'=final_table[,names(final_table) %in% c('displayName', 'id', 'status', 'obj_type'), drop=F], 'responses' = results))
    
  }
  
  
}

deleteDHIS2_Values <- function(df, splitBy, usr, pwd, url, type='dataValues') {
  # With the upgrade to 2.22, we can now use a delete strategy using a POST call to the system.  this makes 
  # it much easier to delete data.  Column names should match the json style (camel case).
  # must at least have dataElement, period, orgUnit, attributeOptionCombo, categoryOptionCombo, and value
  if (nrow(df) < splitBy) {
    split <- cbind(1, nrow(df))
  }
  else {
    split <- calcSplits(df, splitBy)
    
  }
  results <- data.frame(matrix(nrow=0, ncol=4))
  conflicts <- list()
  
  url <- paste0(url, 'dataValueSets')
  for (i in 1:nrow(split)) {
    cat('\rSegment:', i, 'of', nrow(split))
    payload <- list( df[split[i,1]:split[i,2],])
    names(payload) <- type
    req <- POST(paste0(url,'?importStrategy=DELETE'),authenticate(usr, pwd), body= payload, encode='json')
    results <- rbind(results, unlist(content(req)$importCount))
    cat('\tStatus:', content(req)$description)
    if ("conflicts" %in% names(content(req))) {
      conflicts <- append(conflicts, list(content(req)$conflicts))
    }
    cat('\tSegments with conflicts:', length(conflicts), '\t\t')
    flush.console()
    Sys.sleep(runif(1, 0, .2))
  }
  cat('\n')
  names(results) <- names(unlist(content(req)$importCount))
  print(colSums(results))
  return(list('results' = results, 'conflicts' = conflicts))
}

# QUERY ----------------------------------------------------------------------------

queryDHIS2 <- function(term, term_type, obj_type, usr, pwd, url) {
  # Query the DHIS2 system with a 'term' that matches a certain attribute of the obj_type that you are looking
  # for.  An example would be term='SF-ANC- Abortions', term_type= 'name', obj_type='dataElements'
  
  term <- gsub(" ", "%20", term)
  # Build the query
  url <- paste0(url, obj_type, '.json?fields=*&filter=',term_type,":eq:", term, '&paging=false')
  
  # this is a get request
  req <- content(GET(url, authenticate(usr, pwd, type='basic')))
  
  if (obj_type %in% names(req)) {
    return(req[[obj_type]])
  }
  else {
    warning("Something didn't seem to go right.  Here's the full response.")
    return(req)
    
  }
  
}

# DEPRECATED -----------------------------------------------------------------------
# 
# createBatch_dataElements <- function(new_elements_df, domainType= 'AGGREGATE', 
#                                      valType = 'INTEGER_POSITIVE', aggType= 'SUM',
#                                      catCombo = '', url = "", zeros='false', usr=NA, pwd=NA, api_url='https://zl-dsp.pih.org/') {
#   # take a df of new data element names and short names
#   # make a df appropriate for upload to DHIS2
#   # need to define several things 
#   # this is sufficient for a CSV upload
#   
#   out <- cbind.data.frame('name' = new_elements_df$dataElement, 'id' = "",
#                           'code' = '', 'shortName' = new_elements_df$shortName)
#   # does a description exist?
#   out$description <- unlist(ifelse('description' %in% names(new_elements_df), new_elements_df$description, ""))
#   out$formName <- unlist(ifelse('formName' %in% names(new_elements_df), new_elements_df$formName, ""))
#   out$domainType <- domainType
#   out$valueType <- valType
#   out$aggregationOperator <- aggType
#   # get the list of cat combos if we're searching for a name
#   # otherwise you can pass the id directly
#   if (catCombo != '') {
#     catCombos <- getDHIS2_Resource('categoryCombos', usr, pwd, api_url)
#     catCombo <- catCombos$id[catCombos$displayName == catCombo]
#     if (length(catCombo) > 1) {stop('Multiple Category Combinations found')}
#   }
#   
#   out$categoryCombo <- catCombo
#   out$url <- url
#   out$zero <- zeros
#   return(out)
# }
# 
# postDHIS2_dataElements <- function(dataElements, usr, pwd, url='http://zl-dsp.pih.org/api/',overwrite=F, prompt=T, verbose=F) {
#   # the final step in configuration for data elements, this requires the name, definition, aggregation type, etc 
#   # to post along with the appropriate category combination. see createDHIS2_dataElement() for more details
#   # this is no way complete.  assumes a data element name, short name, description, value type, aggregation type, and catcombo 
#   # columns
#   cat('----------------- Starting Data Element Upload -------------------------\n')
#   
#   # double check column names
#   if (!all(names(dataElements) %in% c('dataElement','shortName', 'description', 'valueType', 'aggregationType', 'categoryCombo', 'formName'))) {
#     stop("Column names don't match: 'dataElement','shortName', 'description', 'valueType', 'aggregationType', 'categoryCombo', 'formName' \n
#          Check the input data and try again.")
#   }
#   
#   existingDataElements <- getDHIS2_Resource('dataElements', usr, pwd, url)
#   existingCatCombos <- getDHIS2_Resource('categoryCombos', usr, pwd, url)
#   
#   # set up some empty counts and objects to return
#   skip_count <- 0
#   req <- list('updated' = list(), 'uploaded' = list(), 'skipped' = list())
#   
#   for (cc in 1:nrow(dataElements)) {
#     dataElement <- dataElements[cc,,drop=F]
#     de_name <- dataElement[1,1] 
#     cat('\r', rep('\t',60))
#     cat('\rUploaded:', length(req$uploaded), 'Updated:', length(req$updated), 'Skipped:', length(req$skipped),'\t\tCURRENT:', cc, "|", nrow(dataElements), '\tObject:',de_name)
#     
#     upload <- createDHIS2_DataElement(de_name, shortName = dataElement$shortName, aggregationType = dataElement$aggregationType,
#                                       valueType = dataElement$valueType, categoryCombo = dataElement$categoryCombo, 
#                                       description = dataElement$description, formName= dataElement$formName)
#     
#     
#     if (de_name %in% existingDataElements$displayName == TRUE & overwrite == T) {
#       # does this name already exist in the system? if it does, 
#       # and overwrite == T, prompt for permission on each data element that already
#       # exists
#       
#       # get the id
#       id <- existingDataElements$id[existingDataElements$displayName == de_name] 
#       # download the detailed information on the category combination
#       existing_dataElement <- content(getDHIS2_elementInfo(id, 'dataElements', usr, pwd, url))
#       
#       # get the ids of the attached cat combo
#       existing_catCombo <- unlist(existing_dataElement$categoryCombo)
#       
#       comparison <- unlist(existing_dataElement[names(upload)])
#       comparison['categoryCombo.id'] <- existingCatCombos$displayName[existingCatCombos$id == comparison['categoryCombo.id']]
#       if (verbose == T | prompt == T) {
#         cat("\nData Element with that name already exists and has the following configuration:\n")
#         print(comparison)
#         cat('Attempted data element to upload:\n')
#         print(unlist(upload$name))
#         ifelse(prompt==T, resp <- confirmAction('Overwrite existing configuration? Y/N: '), resp <- "Y")
#       }
#       else {
#         resp <- "Y"
#       }
#       
#       if (resp == "Y") {
#         resp <- content(putDHIS2_metaData(upload, usr, pwd, existing_dataElement$href))
#         req$updated %<>% append(., list(list('name'=de_name, 'response'=list(resp))))
#       }
#       
#     }
#     else if (de_name %in% existingDataElements$displayName == FALSE ) {
#       # any data elements that DO NOT exist at all will always be uploaded
#       resp <- content(postDHIS2_metaData(upload, usr, pwd, url, type='dataElements'))
#       req$uploaded %<>% append(., list(list('name'=de_name, 'response'=list(resp))))
#     }
#     else {
#       if( verbose==T) cat("Skipping!")
#       req$skipped %<>% append(., list(list('name'= de_name, 'attempted' = list(upload))))
#       
#     }
#   }
#   cat('\n----------------- Completed Data Element Upload ---------------------------\n')
#   cat("Uploaded", length(req$uploaded), 'data elements\n')
#   cat("Updated", length(req$updated), 'already existing data elements\n')
#   
#   cat("Skipped", length(req$skipped), 'already existing data elements\n')
#   
#   return(req)
#   
#   }
# 
# postDHIS2_categoryCombos <- function(catCombos, usr, pwd, url='http://zl-dsp.pih.org/api/',overwrite=F, prompt=T, verbose=F) {
#   # take the category combinations listed on the Data Elements tab in the the config file and create the appropriate
#   # combinations in the system.  similar to postDHIS2_categories, this will check if the settings already exist in 
#   # the system and will skip those unless overwrite=T.  set prompt=T to confirm any overwrites, otherwise, they 
#   # will be overwritten automatically.  THIS COULD BE DANGEROUS IF NOT CERTAIN THERE ARE NO OVERLAPPING CONFIGURATIONS
#   cat('----------------- Starting Category Combo Upload -------------------------\n')
#   # check category combinations that exist in the system
#   existingCatCombos <- getDHIS2_Resource('categoryCombos', usr, pwd, url)
#   existingCategories <- getDHIS2_Resource('categories', usr, pwd, url)
#   
#   # set up some empty counts and objects to return
#   req <- list('updated' = list(), 'uploaded' = list(), 'skipped' = list())
#   
#   # check categories per combination are the same
#   
#   for (cc in 1:nrow(catCombos)) {
#     catCombo <- catCombos[cc,]
#     cat_name <- catCombo[1,1] 
#     # print(cat_name)
#     cats <- catCombo[-1] %>% .[!is.na(.)] %>% .[!duplicated(.)]
#     upload <- createDHIS2_CategoryCombo(cat_name, cats, shortName = cat_name)
#     cat('\r', rep('\t',40))
#     cat('\rUploaded:', length(req$uploaded), 'Updated:', length(req$updated), 'Skipped:', length(req$skipped),'\t\tCURRENT:', cc, "|", nrow(catCombos), '\tObject:',cat_name,"\tStrategy: ")
#     
#     if (cat_name %in% existingCatCombos$displayName == TRUE & overwrite == T & cat_name != "default") {
#       # does this name already exist in the system? if it does, 
#       # and overwrite == T, prompt for permission on each catCombo that already
#       # exists
#       # get the id
#       id <- existingCatCombos$id[existingCatCombos$displayName %in% cat_name] 
#       # download the detailed information on the category combination
#       existing_catCombo <- content(getDHIS2_elementInfo(id, 'categoryCombos', usr, pwd, url))
#       # get the ids of the attached categories
#       if (verbose == T | prompt == T) {
#         existing_cats <- unlist(existing_catCombo$categories)
#         # do the ids match? if so, answer will be TRUE
#         already_exists <- all(existingCategories$displayName[existingCategories$id %in% existing_cats] %in% cats)
#         cat("\nCategory Combination with that name already exists and has the following options:\n")
#         print(existingCategories$displayName[existingCategories$id %in% existing_cats])
#         cat('Attempted categories to upload:\n')
#         print(cats)
#         ifelse(prompt==T, resp <- confirmAction('Overwrite existing configuration? Y/N: '), resp <- "Y")
#       }
#       else {
#         resp <- "Y"
#       }
#       if (resp == "Y") {
#         resp <- content(putDHIS2_metaData(upload, usr, pwd, existing_catCombo$href))
#         req$updated %<>% append(., list(resp))
#       }
#       
#     }
#     else if (cat_name %in% existingCatCombos$displayName == FALSE ) {
#       # any categories that DO NOT exist at all will always be uploaded
#       resp <- content(postDHIS2_metaData(upload, usr, pwd, url, type='categoryCombos'))
#       req$uploaded %<>% append(., list(resp))
#     }
#     else {
#       cat("Skipping!")
#       req$skipped %<>% append(., list(upload))
#     }
#     flush.console()
#     
#     # cat(c('\r',rep('\t',35),'\r'))
#   }
#   cat('\n----------------- Completed Category Combo Upload ------------------------\n')
#   
#   cat("Uploaded", length(req$uploaded), 'category combinations\n')
#   cat("Updated", length(req$updated), 'already existing category combinations\n')
#   cat("Skipped", length(req$skipped), 'already existing category combinations\n')
#   
#   return(req)
#   
# }
# 
# postDHIS2_categories <- function(catOptions, usr, pwd, url="http://zl-dsp.pih.org/api/", overwrite=F, prompt=T) {
#   # data a data frame with the first column naming the categories
#   # and the other columns in the rows indicating the options
#   # get list of categories
#   existingCategories <- getDHIS2_Resource('categories', usr, pwd, url)
#   existingOptions <- getDHIS2_Resource('categoryOptions', usr, pwd, url)
#   skip_count <- 0
#   update_count <- 0
#   req <- list('updated' = list(), 'uploaded' = list())
#   
#   for (cat in 1:nrow(catOptions)) {
#     category <- catOptions[cat,]
#     cat_name <- category[1,1]
#     opts <- category[,-1] %>% .[!is.na(.)] %>% .[!duplicated(.)]
#     upload <- createDHIS2_Category(cat_name, opts, shortName = cat_name)
#     
#     
#     if (cat_name %in% existingCategories$displayName == TRUE & overwrite == T) {
#       # does this name already exist in the system? if it does, 
#       # and overwrite == T, prompt for permission on each category that already
#       # exists
#       id <- existingCategories$id[existingCategories$displayName %in% cat_name] 
#       existing_cat <- content(getDHIS2_elementInfo(id, 'categories', usr, pwd, url))
#       existing_cat_options <- unlist(existing_cat$categoryOptions)
#       already_exists <- all(existingOptions$displayName[existingOptions$id %in% existing_cat_options] %in% opts)
#       cat("Category",cat_name,"already exists and has the following options:\n")
#       print(existingOptions$displayName[existingOptions$id %in% existing_cat_options])
#       cat('Attempted options to upload:\n')
#       print(opts)
#       ifelse(prompt==T, resp <- confirmAction('Overwrite existing configuration? Y/N: '), resp <- "Y")
#       if (resp == "Y") {
#         resp <- content(putDHIS2_metaData(upload, usr, pwd, existing_cat$href))
#         req$updated %<>% append(., list(resp))
#       }
#       else {
#         cat("Skipping!\n")
#         skip_count <- skip_count + 1
#       }
#     }
#     else if (cat_name %in% existingCategories$displayName == FALSE ) {
#       # any categories that DO NOT exist at all will always be uploaded
#       resp <- content(postDHIS2_metaData(upload, usr, pwd, url, type='categories'))
#       req$uploaded %<>% append(., list(resp))
#     }
#     else {
#       skip_count <- skip_count + 1
#     }
#   }
#   cat("Uploaded", length(req$uploaded), 'new categories\n')
#   cat("Updated", length(req$updated), 'already existing categories\n')
#   cat("Skipped", skip_count, 'already existing categories\n')
#   
#   return(req)
# }
# 
# postDHIS2_categoryOptions <- function(options, usr, pwd, url='http://zl-dsp.pih.org/api/') {
#   # Upload any options from a config doc that do not already exist in the system, otherwise skip
#   
#   # get the current options from the system
#   existingOptions <- getDHIS2_Resource('categoryOptions', usr, pwd)
#   req <- list()
#   skip_count <- 0
#   for (o in options) {
#     # check if each option already exists, if it does, then don't do anything, if it doesn't 
#     # then upload the new option
#     if (!(o %in% existingOptions$displayName)) {
#       req %<>% append(., list(content(postDHIS2_metaData(createDHIS2_CategoryOption(o), usr, pwd, url, type = 'categoryOptions'))))
#     }
#     else {
#       skip_count <- skip_count + 1
#     }
#   }
#   cat("Uploaded", length(req), 'options\n')
#   cat("Skipped", skip_count,"already existing options\n")
#   return(req)
# }
# 
# parallel.postDHIS2_Values <- function(df, splitBy, usr, pwd, url='https://zl-dsp.pih.org/api/dataValueSets', nc=8) {
#   # parallelized version of postDHIS2_values
#   # addition of splitting data frame first by nc*2
#   df_split <- calcSplits(df, floor(nrow(df)/(nc)))
#   
#   cat('Registering', nc,'processes for uploading.\n')
#   cl <- makeCluster(nc)
#   registerDoParallel(cl)
#   
#   cat('Starting upload.\n')
#   results <- list()
#   results <- foreach(i=1:nrow(df_split), .combine=append,.packages='httr', .export = c('calcSplits', 'postDHIS2_Values')) %dopar% {
#     list(postDHIS2_Values(df[df_split[i,1]:df_split[i,2],], splitBy, usr, pwd, url))
#   } 
#   
#   stopCluster(cl)
#   
#   r_table <- data.frame(matrix(nrow=0, ncol=4))
#   conflicts <- list()
#   for (i in results) {
#     r_table <- rbind.data.frame(r_table, i$results)
#     conflicts <- append(conflicts, i$conflicts)
#   }
#   
#   print(colSums(r_table))
#   return(list('results'=r_table, 'conflicts'=conflicts))
#   
# }
