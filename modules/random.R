generate_tei <- function(structure, orgUnit, enrollmentDate = Sys.Date()) {
  if (!structure$continue) stop('no more unique values for at least one attribute!')
  
  tei <- structure$structure
  for (i in 1:length(structure$structure$attributes)) {
    j <- structure$structure$attributes[[i]]
    
    if (length(j$value) > 0) {
      choice = sample(1:length(j$value), 1)
      j$value = j$value[choice]
      if (j$unique) {
        structure$structure$attributes[[i]]$value %<>% .[-choice]
        if (length(structure$structure$attributes[[i]]$value) == 0) {
          structure$continue = F
        }
      }
      tei$attributes[[i]] <- j[-grep('unique', names(j))]
    }
  }
  
  
  enrollmentDate %<>% as.Date() %>% as.character()
  tei$orgUnit <- orgUnit
  tei$enrollments[[1]]$orgUnit <- orgUnit
  tei$enrollments[[1]]$enrollmentDate <- enrollmentDate
  tei$enrollments[[1]]$incidentDate <- enrollmentDate
  
  structure[['generated']] = tei
  
  return(structure)
}

new_datetime <- function(d, h=NULL, m=NULL, flux=.2) {
  if (is.null(h)) {
    h <- sample(0:23,1)
  }
  if (is.null(m)) {
    m <- sample(0:59, 1)
  }
  date_val <- d + days(rpois(1, flux)) + hours(h) + minutes(m)
  return(date_val)

}

generate_events <- function(event_stages, orgUnit, enrollment, tei_id, event_date = Sys.Date()) {
  new_events <- list()
  # event_date %<>% ymd()
  date_val = new_datetime(event_date)
  event_date = event_date + days(rpois(1, 52)) # this is changed to data entry date
  for (i in 1:length(event_stages)) {
    evnt <- generate_event(event_stages[[i]], orgUnit, enrollment, tei_id, event_date=event_date, date_val=date_val)
    new_events %<>% append(list(evnt))
    
    if (event_stages[[i]]$programStage == 'lNmErUNhmC0') {
      check <- any(sapply(evnt$dataValues, function(x) x$dataElement == 'ijG1c7IqeZb' && x$value %in% c('0', '1', '2', '3', '7')))
      
      if (check) {print('EU Dispo ends record - breaking generation'); break}
    
    }
    
    date_val = new_datetime(date_val)
    
  }
  return(new_events)
}


generate_event <- function(event_stage, orgUnit, enrollment, tei_id, event_date = Sys.Date(), date_val=Sys.Date()) {
  new_event <- event_stage

  new_event$status='ACTIVE'
  new_event$orgUnit = orgUnit
  new_event$enrollment = enrollment
  new_event$eventDate = event_date %>% as.Date() %>% as.character()
  new_event$trackedEntityInstance <- tei_id
  new_dv <- list()
  for (x in new_event$dataValues) {
    if (x$valueType == 'DATE') {
      x$value = date_val %>% as.Date() %>% as.character()
    }
    else if (x$valueType == 'hour') {
      x$value <- as.character(hour(date_val))
    }
    else if (x$valueType == 'minute') {
      x$value <- as.character(minute(date_val))
    }
    else{
      x$value %<>% sample(1:length(x$value), 1)
    }
    x$valueType = NULL

    new_dv %<>% append(list(x))
    date_val %<>% new_datetime(flux=0)

  }
  new_event$dataValues <- new_dv
  new_event <- new_event[c('program', 'programStage', 'orgUnit', 'eventDate', 'status', 'enrollment', 'trackedEntityInstance', 'dataValues')]
  
  if (any(grepl('repeatable', names(new_event)))) {
    new_event$repeatable <- NULL
    
  }
  return(new_event)
}


determine_stage_structures <- function(program_info, nsamp=NULL) {
  stages <- getDHIS2_multiElementInfo(program_info$programStages, 'programStages', usr, pwd, url)
  stages <- lapply(stages, determine_stage_structure, nsamp=nsamp)
  return(list(structure = stages))
}


determine_stage_structure <- function(program_stage, nsamp=NULL) {

  de <- lapply(program_stage$programStageDataElements, function(z) {
    info <- getDHIS2_elementInfo(z$dataElement$id, 'dataElements', usr, pwd, url)
    list('dataElement' = z$dataElement$id, 
         'value' = determine_value_options(list('id' = info), nsamp=nsamp),
         'valueType' = if_else(grepl('hour', info$name, ignore.case = T), 'hour',
                                if_else(grepl('minute', info$name, ignore.case = T), 'minute',
                                info$valueType))
         )
  
  
  
  })
  de %<>% .[list.which(., !is.null(value))]
  stage <- list(program = program_stage$program$id,
       programStage = program_stage$id, 
       orgUnit = NULL, 
       programStage = program_stage$id, 
       dataValues = de,
       trackedEntityInstance = NULL,
       repeatable = program_stage$repeatable)
  write(toJSON(stage), sprintf("%s.json", stage$program))
  return(stage)
    
}



determine_tei_structure <- function(program_info, nsamp=NULL) {
  # determine the tei structure based on program information
  structure <- list('trackedEntity' = program_info$trackedEntity$id)
  
  get_tea <- . %>% .$programTrackedEntityAttribute %>% lapply(function(x) list('mandatory' = x$mandatory, 
                                                                               'id' = x$trackedEntityAttribute$id, 
                                                                               'pid' = x$id))
  teas <- get_tea(program_info) %>% lapply(function(x) {
    x$id %<>% getDHIS2_elementInfo('trackedEntityAttributes', usr, pwd,url)
    x
  })
  
  structure[['attributes']] <- lapply(teas, function(j) {list('attribute' = j$id$id, 
                                                             'value' = determine_value_options(j, nsamp=nsamp),
                                                             'unique' = j$id$unique)
    }) %>% .[sapply(., function(i) !is.null(i$value))]
  
  structure$enrollments = list(list('enrollmentDate' = NULL,
                      'incidentDate' = NULL,
                      'program' = program_info$id,
                      'orgUnit' = NULL))
  # structure$program <- program_info$id
  
  return(list(structure = structure, generated = NULL, continue = T))
}



determine_value_options <- function(j, nsamp=NULL) {
  x <- j$id
  print(x$name)
  if (grepl('Other|time', x$name, ignore.case = T)) {
    values <- NULL
  }
  else if (x$optionSetValue) {
    values <- find_option_values(x)
  }
  else if (grepl('TEXT', x$valueType)) {
    values <- select_text(x$name, nsamp=nsamp)
  }
  else if (grepl('NUMBER|INTEGER', x$valueType)) {
    values <- select_num(x$name, nsamp=nsamp)
  }
  else if (grepl('DATE', x$valueType)) {
    values <- select_date(x$name, nsamp=nsamp)
  }
  else if ('mandatory' %in% names(j)) {
    if (j$mandatory) {
      cat(sprintf('unable to determine for %s\n enter at least one value or values', toupper(x$name )))
      values <- prompt_str()
    }
    else values <- NULL
  }
  else {
    values <- NULL
  }
  return(values)

}

find_option_values <- function(x) {
  options <- x$optionSet$id %>% getDHIS2_elementInfo('optionSets', usr, pwd, url) %>% .['options']  %>% unlist()
  values <- sapply(options, function(j) getDHIS2_elementInfo(j, 'options', usr, pwd, url)$code) %>% unname() #%>% lapply(function(y) get_element(y$id, 'options'))# %>% unlist()
  return(values)
}

select_date <- function(y, nsamp=NULL, gen=F) {
  if (gen) {
    cat(sprintf('Select date range for %s:\n', toupper(y)))
    min <- prompt_str('Min date: ', validate = ymd)
    max <- prompt_str('Max date: ', validate = ymd)
    if (!is.null(nsamp)) nsamp <- prompt_num('N sample: ')
    vals <- sample(seq(ymd(min), ymd(max), by='day'), nsamp, replace=T)
  }
  else {
    vals <- c(Sys.Date())
  }

  return(vals)
}

select_text <- function(y, nsamp=NULL) {
  cat(sprintf('Select type of text for %s:\n', toupper(y)))
  funcs <- list('rand_name' =  select_text, 
                'rand_para' = partial(stri_rand_lipsum, nparagraphs=ifelse(is.null(nsamp), prompt_num(), nsamp), start_lipsum=F), 
                'rand_string' = partial(stri_rand_strings, n = ifelse(is.null(nsamp), prompt_num(), nsamp), length=20))
  resp <- prompt_option(names(funcs), nsamp = NULL)
  if (resp$selection == 'rand_name') {
    opts = list.files('random-name/', '.json', full.names = T)
    opt = prompt_option(opts=opts)
    vals = fromJSON(opt$selection)
    
    if (is.null(nsamp)) {
      nsamp <- prompt_num()
    }
    
    vals = vals[sample(1:length(vals), nsamp, replace=T)]
    
  } else {
    selector <- funcs[[resp$selection]]
    vals <- selector()
  }
  
  return(vals)
  
}

select_num <- function(y, nsamp=NULL) {
  cat(sprintf('Select type of text for %s:\n', toupper(y)))
  
  min <- prompt_num('Select min value: ')
  max <- prompt_num('Select max value: ')
  if (is.null(nsamp))  nsamp <- prompt_num('Select n sample: ')
  vals <- sample(min:max, nsamp, replace=T)
  return(vals)
}

prompt_option <- function( opts, info = 'Select option from the following:\n', text = 'Please select an option: ', nsamp=NULL) {
  cat('Select name source index:\n')
  print(data.frame('name' = opts))
  resp <- prompt_num('Option: ')
  out <- list('selection' = opts[resp])
  
  if (resp < 1) {
    out['selection'] <- prompt_num('Must be positive integer: ')
    
  }
  if (!is.null(nsamp)) {
    out[['n']] = nsamp
  }
  return(out)
}



prompt_num <- function(text='Number of observations to sample: '){
  n <- as.integer(readline(text))
  if (suppressWarnings(is.na(n))) {
    n <- prompt_num('must be integer: ')
  }
  return(n)
}

prompt_str <- function(text='Date or string: ', validate=NULL) {
  n <- readline(text)
  if (!is.null(validate)) {
    if (is.na(validate(n))) {
      n <- prompt_str('Invalid answer: ', validate=validate)
    }
  }
  return(n)
}

