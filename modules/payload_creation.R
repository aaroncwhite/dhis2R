# AGGREGATE METADATA CREATION ------------------------------------------------------------------------------
# These functions can be used to c use the 'name' tag for related objects.  For example, 
# to create a data element with a catereate various the various meta data configurations necessary. 
# Each has a standard default set.  Allgory combination you would specify categoryCombo = 'combo name'
# this is easier than trying to find the id of each value. 
# These functions are NOT for actual uploading, but help create the proper payload format for upload. 

createDHIS2_DataSet <- function(id, name, shortName = NA, description = "", periodType='Monthly', 
                                dataElements=NA, timely=15, other_properties=list()) {
  # Make a list object for upload to DHIS2.  at least a name is required.  
  # dataElements should be a character vector with the names of each data element
  # that should be assigned to the dataSet
  
  if (is.na(shortName) | is.null(shortName) ) {shortName <- name} # make sure we have something for short name to post
  
  if (suppressWarnings(is.character(dataElements) & length(dataElements) > 0)) { # convert the named data elements
    # to a list object with a parent called 'dataElements' and each child containing the name of 
    # each dataElement passed originally
    dataElements <- lapply(dataElements, function(x) list('dataElement' = list('id' = x)))
  }
  else {
    dataElements <- list()
  }
  
  upload <- list('name'= name,'shortName'= shortName, 'periodType' = periodType, 'description' = description,
                 'dataSetElements' = dataElements, 'timelyDays'= timely)
  if (!is.null(id)) upload <- append(list('id' = id), upload)
  
  
  # incase we passed any other properties, add them now.
  upload <- append(upload, other_properties)
  
  return(upload)
}

createDHIS2_DataElementGroup <- function(id, name, shortName = NA, aggregationType='SUM', description='', 
                                         dataElements= list(), other_properties=list()) {
  # make a list object for upload to DHIS2.  requires a name at least
  # other properties can be passed using other_properties in named list f
  # returns list for upload using postDHIS2_metaData()
  # dataElements must be a list of object ids in the system. 
  
  if (is.na(shortName) | is.null(shortName) ) {shortName <- name} # make sure we have something for short name to post
  
  dataElements <- lapply(dataElements, function(a) list('id' = a))
  
  # these are all necessary elements
  upload <- list('name' = name, 'shortName'=stri_sub(shortName, length=50),
                 'aggregationType' = aggregationType, 'dataElements'=dataElements, 'description' = description
                 )
  if (!is.null(id)) upload <- append(list('id' = id), upload)
  
  
  # these are optional
  upload <- append(upload, other_properties)
  return(upload)
  
}


createDHIS2_DataElement <- function(id=NULL, name, shortName = NA, code="", description="", domainType='AGGREGATE', valueType= 'INTEGER', 
                                    aggregationType='SUM', categoryCombo='default', formName="", other_properties=list()
                                    ) {
  # make a list object for upload to DHIS2.  requires a name at least
  # other properties can be passed using other_properties in named list f
  # returns list for upload using postDHIS2_metaData()
  
  if (is.na(shortName) | is.null(shortName) | nchar(shortName) > 50) {shortName <- name} # make sure we have something for short name to post
  
  # these are all necessary elements
  upload <- list('name' = name, 'shortName'=stri_sub(shortName, length=50),'code' = code, 'description'=description, 'domainType' = domainType, 'formName'= formName, 'valueType' = valueType, 
                 'aggregationType' = aggregationType, 'categoryCombo'=list('id' = categoryCombo))
  if (!is.null(id)) upload <- append(list('id' = id), upload)
  
  # these are optional
  upload <- append(upload, other_properties)
  return(upload)
}

createDHIS2_CategoryCombo <- function(id, name, categories, shortName = NA, dataDimension= 'DISAGGREGATION',other_properties=list()) {
  # make a list object for upload to DHIS2.  requires a name at least plus options to be combined
  # other properties can be passed using other_properties in named list f
  # returns list for upload using postDHIS2_metaData()
  name <- unname(name, force=T)
  if (is.na(shortName) | is.null(shortName) ) {shortName <- name} # make sure we have something for short name to post
  

  
  upload <- list('name' = name, 'shortName'=shortName, 'dataDimensionType' = dataDimension)
  if (!is.null(id)) upload <- append(list('id' = id), upload)
  
  
  categories %<>% .[!is.na(.)]
  category_list <- list()
  for (cat in categories) {
    category_list <- append(category_list, list(list('id'= cat)))
  }
  upload <- append(upload, list('categories' = category_list))
  upload <- append(upload, other_properties)
  return(upload)
}

createDHIS2_Category <- function(id, name, options=c(), shortName = NA, dataDimension='DISAGGREGATION', other_properties=list()) {
  # make a list object for upload to DHIS2.  requires a name at least plus options to be combined
  # other properties can be passed using other_properties in named list f
  # returns list for upload using postDHIS2_metaData()
  
  if (is.na(shortName) | is.null(shortName) ) {shortName <- name} # make sure we have something for short name to post
  categoryOptions <- list()
  for (o in options[!is.na(options)]) {
    categoryOptions <- append(categoryOptions, list(list('id'= o)))
  }
  
  upload <- list('name' = name, 'shortName'=shortName, 'categoryOptions'= categoryOptions, 'dataDimensionType' = dataDimension)
  
  if (!is.null(id)) upload <- append(list('id' = id), upload)
  
  
  upload <- append(upload, other_properties)
  return(upload)
}

createDHIS2_CategoryOption <- function(id, name, add_props=list()) {
  # returns list for upload using postDHIS2_metaData()
  # this one is pretty simple
  upload <- list('name'=name)
  if (!is.null(id)) upload <- append(list('id' = id), upload)
  
  upload <- append(upload, add_props)
  return(upload)
}

# TRACKER SPECIFIC ------------------------------------------------------------------------
createDHIS2_program <- function(id, name, shortName = NA, description = '', trackedEntity, 
                                trackedEntityAttributes=NA, programType = 'WITH_REGISTRATION',
                                other_properties= list()) {
  if (is.na(shortName) | is.null(shortName) ) {shortName <- name} # make sure we have something for short name to post
  
  payload <- list('name' = name, 'shortName' = shortName, 'description'= description,
                  'trackedEntity' = list('name' = trackedEntity),
                  'programType' = programType)
  
  if (!is.na(trackedEntityAttributes)) {payload <- list('programTrackedEntityAttributes' = list(trackedEntityAttributes))}
  if (!is.null(id)) payload <- append(list('id' = id), payload)
  
  payload <- append(payload, other_properties)
  
  return(payload)
}

createDHIS2_programStage <- function(id =NULL, name, programId, dataElements = c(), other_properties=list()) {
  payload <- list('name' = name, 'program' = list('id' = programId)) 
  if (!is_empty(dataElements)) {
    payload <- append(payload, list('programStageDataElements' = lapply(dataElements, function(a) list('id' = a))))
  }
  if (!is.null(id)) payload <- append(list('id' = id), payload)
  
  payload <- append(payload, other_properties)
  return(payload)
}
                                     

createDHIS2_trackedEntityAttribute <- function(id, name, shortName=NA, aggregationType = 'SUM', 
                                               valueType = 'INTEGER', optionSetId = NA, other_properties=list()) {
  # create a trackedEntityAttribute for use with Tracker programs
  if (is.na(shortName) | is.null(shortName) ) {shortName <- name} # make sure we have something for short name to post
  
  payload <- list('name' = name, 'shortName' = shortName, 'valueType' = valueType)
  
  if (!is.na(optionSetId)) {payload <- append(payload, list('optionSetValue' = 'TRUE', 'optionSet' = list('id' = optionSetId)))}
  else {payload <- append(payload, list('optionSetValue' = 'FALSE'))}
  if (!is.null(id)) payload <- append(list('id' = id), payload)
  
  payload <- append(payload, other_properties)
  
  return(payload)
  
}

createDHIS2_optionSet <- function(id =NULL, name, option_ids = list(), add_props=list()) {
  # Make an option set for use with Tracker programs
  payload <- list('name' = name, 'options' = option_ids)
  if (!is.null(id)) payload <- append(list('id' = id), payload)
  if (!is_empty(add_props)) payload %<>% append(add_props)
  
  return(payload)
}

createDHIS2_option <- function(id = NULL, name, code) {
  # make an option for an option set
  payload <- list('name' = name, 'code' = code)
  if (!is.null(id)) payload <- append(list('id' = id), payload)
  
  return(payload)
}

createDHIS2_programRuleVariable <- function(id = NULL, name, 
                                            programRuleVariableSourceType = 'DATAELEMENT_NEWEST_EVENT_PROGRAM_STAGE',
                                            program,
                                            programStage,
                                            dataElement,
                                            useCodeForOptionSet = TRUE
                                            ) {
  # Make a program rule variable for program entry form logic
  payload <- list(name = name, 
                  useCodeForOptionSet = tolower(as.character(useCodeForOptionSet)),
                  programRuleVariableSourceType = programRuleVariableSourceType,
                  program = list('id' = program),
                  programStage = list('id' = programStage),
                  dataElement = list('id' = dataElement))
  if (!is.null(id)) payload <- append(list('id' = id), payload)
  return(payload)
}
  
createDHIS2_programRule <- function(id = NULL, 
                                    name, 
                                    description = '',
                                    condition = '',
                                    program) {
  # Make a program rule
  payload <- list(name = name,
                  description = description, 
                  condition = condition, 
                  program = list('id' = program))
  if (!is.null(id)) payload <- append(list('id' = id), payload)
  return(payload)
}

createDHIS2_programRuleAction <- function(id = NULL,
                                          programRuleActionType = 'HIDEFIELD',
                                          programRule,
                                          ...
                                          ) {
  # Make a program action rule
  payload <- list(programRuleActionType = programRuleActionType,
                  programRule = list('id' = programRule), 
                  ...)
  if (!is.null(id)) payload <- append(list('id' = id), payload)
  return(payload)
}

add_href <- function(obj, obj_type, url) {
  # add an href element to a metadata object for "putting"
  obj %<>% append(list('href' = sprintf('%s%s/%s', url, obj_type, obj$id)))
}

# USERS -----------------------------------------------------------------------------------
createDHIS2_user <- function(id = NULL, firstName, surname, username, password, userRoles, organisationUnits, dataViewOrganisationUnits, add_props=list()) {
  # Create a dhis2 user object for upload, required properties must be stated, additional properties/attributes
  # can be declared with add_props
  # https://lists.launchpad.net/dhis2-devs/msg48140.html

  user <- list('id' = id,
               'name' = paste(firstName, surname),
               'firstName' = firstName,
               'surname' = surname,
               'userCredentials' = list('userInfo' = list('id' = id),
                                        'username' = username, 
                                        'password' = password,
                                        'userRoles' = lapply(userRoles, function(a) list('id' = a))),
               'organisationUnits' = lapply(organisationUnits, function(a) list('id' = a)),
               'dataViewOrganisationUnits' = lapply(dataViewOrganisationUnits, function(a) list('id' = a))
  )
  # if (!is.null(id)) user <- append(list('id' = id), user)
  
  user <- append(user, add_props)
  
  return(user)
               
}

# ORANISATION UNITS -----------------------------------------------------------------------
createDHIS2_OrgUnit <- function(id = NULL, name, shortName=NA, description='', 
                                openingDate = Sys.Date(), parentId=NA, add_props=list()) {
  # Create an organisation unit in dhis2
  if (is.na(shortName) | is.null(shortName) ) {shortName <- substr(name, 1, 49)} # make sure we have something for short name to post
  
  up <- list('name' = name, 'shortName' = shortName, 
             'description' = description, 'openingDate' = openingDate)
  if (!is.na(parentId)) {up <- append(up, list('parent' = list('id' = parentId)))}
  if (!is.null(id)) up <- append(list('id' = id), up)
  
  up <- append(up, add_props)
  
  return(up)
}

# FORMS -----------------------------------------------------------------------------------
createDHIS2_report <- function(id, name, content, type='HTML',
                               reportParams = list('paramReportingPeriod' = "true", 
                                                   'paramOrganisationUnit' = "true"),
                               relativePeriods = list('lastMonth' = 'true', 'thisMonth' = "true")) {
  # return a payload ready to send to dhis2 for a report. lazy loading most of the defaults
  # since our reports will most likely be monthly
  payload <- list('name' = name, 'designContent' = content,'type' = type, 'reportParams' = reportParams,
                  'relativePeriods' = relativePeriods)
  if (!is.null(id)) payload <- append(list('id' = id), payload)
  
  return(payload)
}


# TRANSLATIONS ----------------------------------------------------------------------------
createDHIS2_translation <- function(id, value, property, locale, obj_id, className) {
  upload <- list('className'=className, 
                 'locale'=locale,
                 'property'=property, 
                 'value'=value,  
                 'objectId'= obj_id,
                 'access' = list('read'='true', 
                                 'update'='true',
                                 'externalize'='false', 
                                 'delete'='false', 
                                 'write'='false',
                                 'manage' = 'false')
                  )
  if (!is.null(id)) upload <- append(list('id' = id), upload)
  
  return(upload)
}


# DATASET TRANSFORMATION -------------------------------------------------------------------
prepareDHIS2_dataValues <- function(df, usr, pwd, url) {
  # Take a data frame with columns dataElement, period,  orgUnit, categoryOptionCombo, attributeOptionCombo, value
  # and look up each dataElement, find appropriate id and assign appropriate categoryOptionCombo and attributeOptionCombo
  # ids to the dataframe.  Returns and object ready for upload using postDHIS2_dataValues()
  # REMEMBER:
  # the data elements must be assigned to a data set with the appropriate frequency
  # and that data set must be assigned to the appropriate org units 
  

  cols <- names(df) %in% c('dataElement', 'orgUnit', 'categoryOptionCombo', 'attributeOptionCombo')
  df[,cols] <- apply(df[,cols], 2, as.character)
  
  
  dataElements <- getDHIS2_Resource('dataElements', usr, pwd, url)
  catOptionCombos <- getDHIS2_Resource('categoryOptionCombos', usr, pwd, url)

  # output <- replaceNames(df,'dataElement', dataElements)
  
  output <- data.frame(matrix(nrow=0, ncol=6))
  unique_dataElements <- unique(df$dataElement)
  unique_dataElements <- unique_dataElements[!is.na(unique_dataElements)]
  for (de in unique_dataElements) {
    print(de)
    if (de %in% dataElements$displayName) { # make sure the data element exists
      de_info <- content(getDHIS2_elementInfo(dataElements$id[dataElements$displayName == de], 'dataElements', usr, pwd, url, content=F))

      # find the related category combination information
      catCombo_info <- content(getDHIS2_elementInfo(de_info$categoryCombo$id, 'categoryCombos', usr, pwd, url, content = F))
      catOptionCombo_ids <- unlist(catCombo_info$categoryOptionCombos) # this will return just the ids we want to try to find
      de_optionCombos <- catOptionCombos[catOptionCombos$id %in% catOptionCombo_ids,]

      sub <- df[df$dataElement == de,]
      sub <- sub[!is.na(sub$dataElement),]
      # SET THE OPTION COMBOS
      # so we have the right ids to work with, now let's look at the optionCombos
      if (is.null(catOptionCombo_ids)) { # if there is no disaggregation we can just assign the default
        sub$categoryOptionCombo[sub$dataElement == de & sub$categoryOptionCombo == "default"] <- catOptionCombos$id[catOptionCombos$displayName == 'default']
      }
      else {
        # if it's different, let's match the right category option combos
        stated_combos <- unique(sub$categoryOptionCombo[sub$dataElement == de])
        stated_combos <- stated_combos[!is.na(stated_combos)]
        for (opt_combo in stated_combos) {
          # since dhis2 might have created the combination of categories in a
          # different order than i typed it, this will attempt to match each individual
          # option and guess the right one, if not, it will prompt for a choice.
          opt_combo_index <- matchDHIS2_catOptions(opt_combo, de_optionCombos)

          sub$categoryOptionCombo[sub$categoryOptionCombo == opt_combo & sub$dataElement == de] <- de_optionCombos$id[opt_combo_index]

        }

      }

      # assign the data element id
      sub$dataElement[sub$dataElement == de] <- dataElements$id[dataElements$displayName == de]
      output <- rbind.data.frame(output, sub)
    }
    else {
      print("Skipping because does not exist in the system. Rows have been removed.")
      # df <- df[df$dataElement != de,]
    }
  }
  orgUnits <- getDHIS2_Resource('organisationUnits', usr, pwd, url)
  output <- replaceNames(output, 'orgUnit', orgUnits)
  
  output <- replaceNames(output, 'attributeOptionCombo', catOptionCombos)
  
  return(output)
  
}

convertDHIS2_IDs <- function(df, usr, pwd, url) {
  # Convert a dataSet downloaded from DHIS2 with object ids to a 
  # human readable output
  
  # Get the info we want
  de <- getDHIS2_Resource('dataElements', usr, pwd, url)
  ou <- getDHIS2_Resource('organisationUnits', usr, pwd, url)
  
  # attributeOptionCombos and categoryOptionCombos use the same categoryOptionCombos endpoint
  coc <- getDHIS2_Resource('categoryOptionCombos', usr, pwd, url)
  
  df$dataElement %<>% revalue(make_revalue_map(de$id, de$name), warn_missing=F)
  df$orgUnit %<>% revalue(make_revalue_map(ou$id, ou$name), warn_missing=F)
  df$categoryOptionCombo %<>% revalue(make_revalue_map(coc$id, coc$name), warn_missing=F)
  df$attributeOptionCombo %<>% revalue(make_revalue_map(coc$id, coc$name), warn_missing=F)
  
  return(df)
  
  
}



matchDHIS2_catOptions <- function(opt_combo, de_optionCombos) {

  # since some of the orders are different in the system than how we configure the 
  # excel file, we need to check for matches using each part of the cat option combo.
  # we'll use greps_and() (see utilities.R) to find the column that matches ALL of the 
  # parts that we give it
  print(opt_combo)
  options <- unlist(strsplit(opt_combo, ", ")) 
  opt_combo_index <- greps_and(options, de_optionCombos$displayName, ignoreCase = F)
  if (length(opt_combo_index) > 1) {
    # if we found more than one it's because there are multiple matches, we'll match 
    # based on string length
    opt_combo_index <- which(nchar(de_optionCombos$displayName[opt_combo_index]) == nchar(opt_combo))
    
  }
  
  if (length(opt_combo_index) == 0) {
    # prompt for correct value if nothing matches
    cat('Nothing seems to match!\n Available options:\n')
    current_opts <- de_optionCombos[,'displayName', drop=F]
    rownames(current_opts) <- 1:nrow(current_opts)
    print(current_opts)
    opt_combo_index <- promptNumber('Please select the proper value by entering the row number: ', 1, nrow(current_opts))
    
  }
  return(opt_combo_index)
}
