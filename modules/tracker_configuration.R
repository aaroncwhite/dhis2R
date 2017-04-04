# Tracker Configuration Setup
# Tracker upload process
# 1- create trackedEntityInstances for each trackedEntity
# 2- create program enrollment for trackedEntityInstances in specific program
# 3- create program stage data

# These will handle the proper munging and uploading of data from the EMR data. Metadata configuration happens elsewhere (either with the tracker config file or in the UI)

uploadDHIS2_trackedEntityInstances <- function(df, program_name, usr, pwd, url, conf_map) {
  # upload a data frame with one row per TEI to stated program. 
  # conf_map can be a key/value vector where names are the columns from the df and values are the trackedentityattribute
  # in dhis2
  
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
  if (missing) {
    cat("No attribute mapping defined. Let's do that.\n")
    
    program_attributes <- map_trackedEntityAttributes(df, program_data, usr, pwd, url)
    
    # everything should be mapped appropriately now, let's map things in the right way
    
  }
  
  
  
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

convert_row_to_tei <- function(row, te_id, attribute_map) {
  # requires at least a column called "orgUnit" for enrollment site
  # plus any attributes defined in conf
  orgUnit <- row$orgUnit %>% as.character()
  # row %<>% as.character()
  attributes <-  data.frame('value' = row[names(attribute_map)] %>% as.character(), stringsAsFactors = F)
  attributes$attribute <- attribute_map
  
  attribute_list <- list()
  for (i in 1:nrow(attributes)) {
    attribute_list %<>% append(list(list('attribute' = attributes$attribute[i], 'value' = attributes$value[i])))
  }
  
  return(list('trackedEntity' = te_id, 'orgUnit' = orgUnit, 'attributes' = attribute_list))
}

convert_row_to_enrollment <- function(row, program_data, te_id, tei_id, date_map) {
  # convert row to enrollment
  # requires trackedEntity id (te_id) and trackedEntityInstance id (tei_id)
  # When a new trackedEntityInstance is posted to the API it returns the tei_id as response$reference
  # date_map should be a vector indicating enrollmentDate and incidentDate column values
  # Ex. date_map = c('enrollment_date' = 'enrollmentDate', 'enrollment_date' = 'incidentDate')
  # in this case, enrollment_date from the row data would be used for both enrollmentDate and incidentDate in the dhis2 payload
  
  payload <- list('program' = program_data$id,
       'orgUnit' = row$orgUnit,
       'trackedEntity' = te_id, 
       'trackedEntityInstance' = tei_id
       )
  dates <- row[names(date_map)] %>% lapply(function(x) x) 
  names(dates) <- date_map
  
  payload %<>% append(dates)
  
  return(payload)
  
}

