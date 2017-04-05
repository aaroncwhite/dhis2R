# Tracker Configuration Setup
# Tracker upload process
# 1- create trackedEntityInstances for each trackedEntity
# 2- create program enrollment for trackedEntityInstances in specific program
# 3- create program stage data

# These will handle the proper munging and uploading of data from the EMR data. Metadata configuration happens elsewhere (either with the tracker config file or in the UI)

uploadDHIS2_trackedEntityInstances <- function(df, program_name, usr, pwd, url, conf_map, date_map, parallel=T, n_clust=detectCores(), source_files='') {
  # upload a data frame with one row per TEI to stated program. 
  # conf_map can be a key/value vector where names are the columns from the df and values are the trackedentityattribute
  # in dhis2
  # also enrolls TEIs in the program based on date map
  
  if (!('orgUnit' %in% names(df))) {# data often have location instead of orgUnit label.  No error handling here yet. 
    cat('OrgUnit seems to be missing. Available options:\n')
    print(names(df))
    df$orgUnit <- df[,readline('Please specify the proper column name to use as orgUnit: ')]
  }
  
  
  # get program details
  program_data <- queryDHIS2(program_name, 'name', 'programs', usr, pwd, url)
  n_programs <- length(program)
  
  # If the name has been incorrectly typed or doesn't match exactly
  # let's have some debugging to handle that. 
  if (n_programs > 1) { # we found more than one
    cat("Multiple Programs with that name found:\n")
    print(data.frame('name' = sapply(program_data, function(x) x$name), 'number' = 1:length(program_data)))
    resp <- readline('Please enter the number for the program to use:') %>% as.numeric()
    while(!(resp >= 1 & resp <= n_programs)) {
      resp <- readline(sprintf('The answer should be an integer between %s and %s: ', 1, n_programs)) %>% as.numeric()
    }
    program_data <- program_data[[resp]]
  }
  else if (n_programs == 0) { # we didn't find anything
    stop('No programs found!\n')
  }
  else {
    program_data <- program_data[[1]]
  }
  
  cat("Using program:", program_data$name, '\n')
  
  # now let's see if there's a conf_map
  if (missing(conf_map)) {
    cat("No attribute mapping defined. Let's do that.\n")
    program_attributes <- map_trackedEntityAttributes(df, program_data, usr, pwd, url)

  }
  
  if (parallel) {
    cl <- spin_up_clusters(n_clust, source_files = source_files) # start the cluster
    # make the initial payloads

    # things can time out if trying to upload a lot of data at once
    resps <- foreach(i=550:nrow(df), .combine=append) %dopar% {
      cat(sprintf("-------- %s -----------\n", i))
      response <- create_TEI_and_Enrollment(df[i,], program_data, attribute_map, date_map, usr, pwd, url)
      return(response)
    
    }
    stopCluster(cl)
  }
  return(resps)
  
}

map_trackedEntityAttributes <- function(df, program_data, usr, pwd, url) {
  program_attributes <- sapply(program_data$programTrackedEntityAttributes, function(x) x$name) # pull just the names of the attributes
  program_ids <- sapply(program_data$programTrackedEntityAttributes, function(x) x$dimensionItem) # dhis2 maps in the following format PROGRAMID.TEI_ATTRIBUTEID as a dimension item
  program_ids %<>% gsub(paste0(program_data$id, '.'), '', .) # remove the program id so we just have the tracked entity attribute id
  
  n_attr <- length(program_attributes) # total number of attributes that need to be mapped.
  
  cat(sprintf("%s attributes found.  For each one, we'll display a list of headers in the dataframe provided.\nSelect the number that matches the attribute\n", n_attr))
  readline('Press enter when ready...')
  
  n_cols <- 1:ncol(df)
  available_columns <- 1:ncol(df) # vector to track which column names have already been used
  
  cols <- names(df)
  while (T) {
    for (i in 1:n_attr) {
      cat(sprintf("------ %s -------\n", program_attributes[i]))
      opts <- data.frame('name' = cols[available_columns])
      rownames(opts) <- available_columns
      print(opts)
      resp <- promptNumber(sprintf("%s maps to option #", program_attributes[i]), range=available_columns)
      names(program_attributes)[i] <- cols[resp]
      available_columns %<>% .[!(. == resp)]
    }
    cat('\n Attributes mapped!\n')
    map <- data.frame('dhis2_attribute' = names(program_attributes), 'dataset_name' = program_attributes)
    rownames(map) <- 1:nrow(map)
    print(map)
    resp <- readline('Everything look good? Enter Y to continue, N to redo, or C to cancel process:') %>% toupper()
    while (!(resp %in% c('Y', 'N', 'C'))) {
      resp <- readline('Try that answer again:') %>% toupper()
    }
    if (resp == 'Y') break
    if (resp == 'C') stop('Process canceled!')
    
  }
  # attribute names and ids are in the same order, so spit out id values since we'll use those in the upload
  names(program_ids) <- names(program_attributes)
  return(program_ids)
}

createDHIS2_trackedEntityInstances <- function(df, program_data, attribute_map, date_map, parallel=T, n_core=detectCores(), source_files='', cl) {
  # convert a whole dataframe with one row per trackedEntityInstance into payloads for dhis2
  if (parallel) {
    # allow for spinning cluster up once from a parent function
    if (missing(cl)) {
      cl <- spin_up_clusters(n_core,source_files = source_files)
      stop_clust <- T
    }
    
    payloads <- foreach(i=1:nrow(df), .combine = append) %dopar% {
      list(convert_row_to_tei(df[i,], te_id, attribute_map))
    }
    if (stop_clust) stopCluster(cl)
  }
  else {
    # not sure how to dynamically substitute special command %dopar%.  This is not DRY, but it works for now.
    payloads <- foreach(i=1:nrow(df), .combine = append) %do% {
      list(convert_row_to_tei(df[i,], te_id, attribute_map))
    }
  }
  return(ifelse(length(payloads)>1, list('trackedEntityInstances' = payloads), payloads))

}

createDHIS2_programEnrollments <- function(df, program_data, tei_response, date_map, parallel=T, n_core=detectCores(), source='', stop_clust=T, cl) {
  # take a response from posting tracked entity instances and enroll those instances in the appropriate program
  tei_ids <- sapply(tei_response$importSummaries, function(x) x$reference)
  df$trackedEntityInstance <- tei_ids # these will be in the same order
  
  if (parallel) {
    if (missing(cl)) {
      cl <- spin_up_clusters(n_core, source_files = source_files)
      stop_clust <- T
    }
    payload <- foreach(i=1:nrow(df), .combine = append) %dopar% {
      list(convert_row_to_enrollment(df[i,], program_data, date_map))
    }
    if (stop_clust) stopCluster(cl)
  }
  
  return(ifelse(length(payload) > 1, list('enrollments' = payload), payload))

  
}

create_TEI_and_Enrollment <- function(row, program_data, attribute_map, date_map, usr, pwd, url) {
  # Isolated instance for one specific row.  Creating this because of errors kicking when trying to 
  # batch all TrackedEntityInstance creation and Enrollments
  # this will create the trackedEntityInstance and then enroll it in the proper program in one go
  cat(sprintf('PtID: %s  Identifier: %s\n', row$patient_id, row$identifier))
  payload <- convert_row_to_tei(row, program_data$trackedEntity$id, attribute_map)
  tei_resp <- postDHIS2_metaData(payload, 'trackedEntityInstances', usr, pwd, url)
  if (check_response(tei_resp, 'TEI')) {
    # tracked entity creation was successfull, now enroll in the program
    tei_resp %<>% content()
    row$trackedEntityInstance <- tei_resp$response$reference
    payload <- convert_row_to_enrollment(row, program_data, date_map)
    enroll_resp <- postDHIS2_metaData(payload, 'enrollments', usr, pwd, url)
    if (check_response(enroll_resp, "Enrollment")) {
      enroll_resp %<>% content()
    }
    return(list('tei' = tei_resp, 'enrollment' = enroll_resp))
  }
  else {
    return(list('tei' = tei_resp, 'enrollment' = NULL))
  }
  
}

check_response <- function(resp, text) {
  # used to continue process, returns T/F
  if( resp$status_code %in% 200:204) {
    cat(sprintf('%s Success!\n', text))
    return(T)
    
  }
  else {
    cat('Something went wrong\n')
    return(F)
  }
}

convert_row_to_tei <- function(row, program_data, attribute_map, enrollment=T, date_map) {
  # requires at least a column called "orgUnit" for enrollment site
  # plus any attributes defined in conf
  # program_data is the api response with details from the api
  orgUnit <- row$orgUnit %>% as.character()
  # row %<>% as.character()
  attributes <-  data.frame('value' = row[names(attribute_map)] %>% as.character(), stringsAsFactors = F)
  attributes$attribute <- attribute_map
  
  attribute_list <- list()
  for (i in 1:nrow(attributes)) {
    attribute_list %<>% append(list(list('attribute' = attributes$attribute[i], 'value' = attributes$value[i])))
  }
  tei <- list('trackedEntity' = te_id, 'orgUnit' = orgUnit, 'attributes' = attribute_list)
  
  if (enrollment) {
    enrollment <- list('enrollments' = list('program' = program_data$id,
                                            'orgUnit' = row$orgUnit,
                                            'enrollmentDate' = row[date_map[date_map == 'enrollmentDate']],
                                            'incidentDate' = row[date_map[date_map == 'incidentDate']]))
    tei %<>% append(enrollment)
  }
  
  return(tei)
}

convert_row_to_enrollment <- function(row, program_data, date_map) {
  # convert row to enrollment
  # requires trackedEntity id (te_id) and trackedEntityInstance id (tei_id)
  # When a new trackedEntityInstance is posted to the API it returns the tei_id as response$reference
  # date_map should be a vector indicating enrollmentDate and incidentDate column values
  # Ex. date_map = c('enrollment_date' = 'enrollmentDate', 'enrollment_date' = 'incidentDate')
  # in this case, enrollment_date from the row data would be used for both enrollmentDate and incidentDate in the dhis2 payload
  
  payload <- list('program' = program_data$id,
       'orgUnit' = row$orgUnit,
       'trackedEntity' = program_data$trackedEntity$id, 
       'trackedEntityInstance' = row$trackedEntityInstance
       )
  dates <- row[names(date_map)] %>% lapply(function(x) x) 
  names(dates) <- date_map
  
  payload %<>% append(dates)
  
  return(payload)
  
}

