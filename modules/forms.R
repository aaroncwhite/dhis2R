# Form Creation
# assumes other helper scripts have been loaded (api in particular)
library(openxlsx)
library(htmlTable)
# library(stringi)
library(stringr)

scrapeDHIS2_form <- function(form_file, usr, pwd, url, type='report', add_js='', add_date_filter=F) {
  # using scrape instead of create to avoid confusion with payload creation
  # scrape is same as other excel based tools
  # import an excel form with data elements and indicators defined
  # create relevant JavaScript at beginning to query analytics
  # for those data elements and then convert imported dataframe to
  # html table.  replace name values with id values queried from relevant
  # sources.  Each data reference should be structured identifying the 
  # entity type::name::categoryoptioncombination(if necessary).  Note the 
  # double :: dividers. 
  # form type:
  # the type parameter supports "dataEntryForm" or "report" which will 
  # change the html tags that call the element ids
  # add_date_filter is used for analytics objects like charts and tables
  # if T, it will specify to specific reporting month. otherwise, the 
  # default value will be returned.  if relative timeframes, the date is used as
  # the end montt
  # The data imported here will be passed to createDHIS2_form() to create a payload
  # for upload to the server. Returns formatted html code.
  #
  # Ex. 
  # > scrapeDHIS2_form('path/to/form.html', usrname, password, server_url)
  
  # Step 1: Import the excel html file
  html_raw <- readLines(form_file)
  html_raw <- gsub('  ', ' ', paste(html_raw, collapse="")) # there are extra line breaks that add spacing, get ride of those
  # find the break point where our content starts
#   html_raw <- strsplit(html_raw, '<!-----------------------------><!--START OF OUTPUT FROM EXCEL PUBLISH AS WEB PAGE WIZARD --><!----------------------------->')
#   html_head <- html_raw[[1]][1] # above the commented line
#   html_body <- html_raw[[1]][2] # below the commented line
  html_body <- html_raw
  
  # Step 2: Replace the references in the file
  objects <- c('report', 'dataElements', 'indicators', 'charts', 'reportTables')
  # report will pull special values from the report itself. needs to be
  # declared in the javascript
  parsed <- findDHIS2_elements(html_body, objects)
  parsed$addl_disagg_or_specification %<>% gsub('&lt;', "<", .) %>% gsub('&gt;', ">", .)
  
  prepared <- replaceFormValues(parsed, usr, pwd, url, type=type)
  

  # escape the brackets again, derp
  prepared$pattern %<>% gsub("\\[","\\\\[", .) %>% gsub("\\]","\\\\]", .)
  base <- ''
  
  for (i in 1:nrow(prepared)) {
    # replace the placeholders with the html spans generated
    print(prepared$html[i])
    html_body <- gsub(prepared$pattern[i], prepared$html[i], html_body)
  }
  
  # Step 3: Prepare the JavaScript header
  if (type == 'report') {
    base <- "
    
    <script src'//cdnjs.cloudflare.com/ajax/libs/numeral.js/1.4.5/numeral.min.js'></script>
  
    <script type='text/javascript'>
    $( document ).ready( function() {
    
    var date = dhis2.report.date;
    console.log(date); 
    
    var datestr=date.substring(0,4)+date.substring(5,7);
    var orgUnit = dhis2.report.organisationUnit;
    $('#orgUnit').html(orgUnit.name);
    $('#date').html(dhis2.report.date)
    console.log(orgUnit);
    
    --CHARTS--
  
    --REPORTTABLES--
  
    --MAPS--
  
    var DElements= [---OBJECT_IDS---];
    var DEValues = {};
    
    var lookup = DElements.join(';');
    $.get('../api/analytics?dimension=dx:'+lookup+'&dimension=pe:'+datestr+'&filter=ou:' + orgUnit.id)
      .fail(function(err){
        console.warn('Something bad happened',err);
      })
      .then(function (json) {
        for (var i = 0; i<json.rows.length; i++){
         console.log(json.rows[i]);
          DEValues[json.rows[i][0]]=json.rows[i][2];
        }
  
        for (i = 0; i < DElements.length; i++) {
          if (DEValues.hasOwnProperty(DElements[i])){
          var htmlElement=DElements[i].replace('.','');
            $('#'+htmlElement).html(DEValues[DElements[i]]);
          }
      
      }
    });
    }); 
      
    </script>
    "
    
    # the analytics query update
    ids <- prepared$ids[prepared$obj_type %in% c('dataElements', 'indicators')] #report allows for dhis.report objects or js objects, so exclude
    # collapse into a comma separated string to insert into js 
    ids %<>% paste0(., collapse="', '") %>% # condense each element down with internal single quotes and commas
      paste0("'",.,"'") # add the outside quotes on the string
    
    base <- gsub('---OBJECT_IDS---', ids, base)
    
    # update the analytics objects and make the JS scripts
    analytics_objects <- c('charts', 'reportTables', 'maps')
    analytics_strings <- lapply(analytics_objects, function(x) buildObjectQueries(prepared, x))
    
    for (i in 1:length(analytics_objects)) {
      base <- gsub(paste0('--', toupper(analytics_objects[i]), '--'), analytics_strings[i], base)
    }
  }
  
  
  base <- paste0(base, '\n', add_js) # any other scripts can be defined in add_js. make sure it has the <script></script> tags
  
  
  
  # Step 4: Create the final html code to upload
  
  html_code <- paste0(base, "\n", html_body) # concatenate the js and html together. 
  
  return(html_code)
}


findDHIS2_elements <- function(html_body, objects) {
  # search an html string for specific object (or any string)
  # return a list of indicies, object type, and values
  objects %<>% paste0('\\[\\[',., "::*")
  
  # find the beginning of each of the patterns
  direct <- str_locate_all(html_body, objects)
  # find the end
  indirect <- str_locate_all(html_body, '\\]\\]')
  # convert the list objects to data frames
  for (i in 1:length(objects)) {
    if (nrow(direct[[i]]) > 0) { # if we matched something for the object, convert the results
      direct[[i]] <- as.data.frame(cbind(direct[[i]], 'pattern' = objects[i]))
    }
  }
  
  direct <- direct[sapply(direct, function(x) nrow(x) > 0)]
  
  # collapse the list into one dataframe
  direct <- list_to_df(direct)
  # start and end points got converted to factors, fix that
  direct[,1:2] <- apply(direct[,1:2], 2, function(j) as.numeric(as.character(j)))
  
  # find the closest end point for each start point as defined by the location of the ]] minus the 
  # end of [[object:: 
  # Ex. if the end of [[report:: is at 204, it will try to find the closest positive number for all of
  # the end points (identified with ]]) - 204.  The smallest positive number is most likely the end point. 
  # create a distance matrix to find out
  dist <- sapply(direct$end, function(x) indirect[[1]][,'end'] - x)
  # for each end point on the matched beginning strings, find the smallest point
  # that is greater than 0 from the other string.  This is the end point. 
  order <- apply(dist, 2, function(x) which(x >= 0 & x == min(x[x>0])))
  
  # replace the intermediate end with the final endpoint we just found. 
  direct <- cbind.data.frame(direct[,c('start', 'pattern')], 'end' = indirect[[1]][,'end'][order])
  
  # pattern got converted to factor, fix. 
  direct$pattern %<>% as.character()
  
  
  # get the whole string and replace the intermediate pattern with the full one
  for (i in 1:nrow(direct)) {
    direct$pattern[i] <- substr(html_body, direct$start[i], direct$end[i])
  }
  
  split_strings <- direct$pattern %>% gsub('\\[\\[', '', .) %>% gsub('\\]', '', .) %>% 
    strsplit("::") %>% lapply(function(x) as.data.frame(t(unlist(x)))) %>% list_to_df()
  
  names(split_strings) <- c('obj_type', 'object', 'addl_disagg_or_specification')[1:ncol(split_strings)]
  
  direct <- cbind.data.frame(direct, split_strings)
  
  return(direct)
}

replaceFormValues <- function(parsed_form, usr, pwd, url, type='report') {
  # Read the parsed form values that need to be replaced,
  # look up each object type and find the matching id values
  if (type=='report') {
    wrap <- c("<span id=", '"></span>')
  }
  else if (type == "dataEntryForm") {
    wrap <- c('<input id=', '-val" name="entryfield" />')
  }
  else {
    stop('Unrecognized report type...')
  }

  
  ignore <- c('report')
  # just look up data objects for this part
  lookups <- all_character(parsed_form[!(parsed_form$obj_type %in% ignore),])
  parsed_form$ids <- parsed_form$object # we're going to loop over the values and replace these. 
  
  # look up each object with the system and get the id values
  for (obj in unique(lookups$obj_type)) {
    cat('\rLooking up', obj, rep(' ', 50))
    obj_info <- getDHIS2_Resource(obj, usr, pwd, url)
    ids <- obj_info$id[obj_info$displayName %in% lookups$object]
    if (length(ids) == 0) {stop('Nothing seemed to match.  Has the configuration been uploaded?')}
    # verify matched all of the data points
    all_matched <- length(ids) == nrow(parsed_form[parsed_form$obj_type == obj,])
    if (all_matched != T) {
      warning(paste('Unable to match all', obj, "ids!"))
    }
    names(ids) <-  obj_info$displayName[obj_info$displayName %in% lookups$object]
    parsed_form$ids <- revalue(parsed_form$ids, ids)
    
    # if data elements have additional specificity defined, look up those.
    # this is going to repeat some code, needs refactoring
    if (obj == 'dataElements' & nrow(parsed_form[!is.na(parsed_form$addl_disagg_or_specification) & parsed_form$obj_type == obj,]) > 0) {
      flush.console()
      more_detail <- all_character(parsed_form[!is.na(parsed_form$addl_disagg_or_specification) & 
                                                 parsed_form$obj_type == obj & nchar(as.character(parsed_form$ids)) == 11,])
      cat('\rLooking up categoryOptionCombinations', rep(' ', 50))
      for(de in unique(more_detail$ids)) {
        print(de)
        catCombo <- getDHIS2_elementInfo(de, obj, usr, pwd, url)$categoryCombo$id
        catOptions <- getDHIS2_elementInfo(catCombo, 'categoryCombos', usr, pwd, url)$categoryOptionCombos
        catOptions <- lapply(catOptions, function(x) 
          getDHIS2_elementInfo(x$id, 'categoryOptionCombos', usr, pwd, url)) %>% 
            lapply(., function(x) as.data.frame(t(unlist(x[c('id', 'displayName')])))) %>% 
          list_to_df() %>% all_character()
        
        catOptionCombo_ids <- catOptions$id
        names(catOptionCombo_ids) <- catOptions$displayName
        
        # can use matchDHIS2_catOptions() to find proper matched catOptionCombo
        for (coc in more_detail$addl_disagg_or_specification[more_detail$ids == de]) {
          catOption_index <- matchDHIS2_catOptions(coc, catOptions)
          
          more_detail$ids[more_detail$ids == de & more_detail$addl_disagg_or_specification == coc] %<>% 
            paste0(., ".", catOptions$id[catOption_index])
        }

      }
      # now let's stick it back with the other data
      parsed_form <- rbind.fill(parsed_form[(!is.na(parsed_form$addl_disagg_or_specification) & parsed_form$obj_type == obj) == F,], # reversing the logic
                                more_detail)
    }
    flush.console()
  }
  
  parsed_form$html <- sapply(parsed_form$ids, function(x) paste0(wrap[1],'"',x, wrap[2], collapse=""))
  
  if (type=='dataEntryForm') {
    parsed_form$html %<>% gsub('\\.', '-', .) # Replace the . with a dash for the input section ids.
  }

  return(parsed_form)
}


buildObjectQueries <- function(prepared_data, obj_type, add_date_filter=F) {
  # for use with charts, reportTables, and maps
  object_queries <- ""
  if (any(grepl(obj_type, prepared_data$obj_type)) ==T ) {
    prepared_data <- all_character(prepared_data)
    for (i in grep(obj_type, prepared_data$obj_type)) {
      id <- prepared_data$ids[i]
      object_str <- paste0('$("#',id,'").attr("src", "../api/', obj_type, '/',
                          id,'/data')
      # we only want to add query params if they exist, so test
      if (!is.na(prepared_data$addl_disagg_or_specification[i]) & 
          'addl_disagg_or_specification' %in% names(prepared_data)) {
        params <- gsub("&amp;", '\\&', prepared_data$addl_disagg_or_specification[i])
        object_str <- paste0(object_str, "?", params)
        
        
      }
      if (add_date_filter == T) {
        # if this is on, we will pull the specific date for the reporting period
        # otherwise, the period of the object will be determined in its settings
        object_str %<>% paste0(., '&date=" + date)')
      }
      object_queries <- paste(object_queries, object_str, ';', '\n\n')
    }
  }
  return(object_queries)
}


findImages <- function(html_body, resource_folder, usr, pwd, url) {
  # resource_folder = Folder that contains references to images
  # find the src references and change them to fileResource references
  # in dhis2
}


convert_form <- function(id, from, to, usr, pwd, url, post=T,
                         name=NA) {
  if (from == "dataEntryForm") {
    form_html <- getDHIS2_elementInfo('dataEntryForm', paste0('dataSets/',id,"/"),usr, pwd, url)$dataEntryForm$htmlCode
  }
  else {
    form_html <- getDHIS2_elementInfo(id, 'reports', usr, pwd, url)$designContent
  }
  
  
  
  html_raw <- gsub('  ', ' ', paste(form_html, collapse="")) # there are extra line breaks that add spacing, get ride of those
  # find the break point where our content starts
  #   html_raw <- strsplit(html_raw, '<!-----------------------------><!--START OF OUTPUT FROM EXCEL PUBLISH AS WEB PAGE WIZARD --><!----------------------------->')
  #   html_head <- html_raw[[1]][1] # above the commented line
  #   html_body <- html_raw[[1]][2] # below the commented line
  html_body <- html_raw
}

convertDHIS2_form <- function(form_name, usr, pwd, url, post=T, report_name=NA, add_js='', add_date_filter=F) {
  # using scrape instead of create to avoid confusion with payload creation
  # scrape is same as other excel based tools
  # import an excel form with data elements and indicators defined
  # create relevant JavaScript at beginning to query analytics
  # for those data elements and then convert imported dataframe to
  # html table.  replace name values with id values queried from relevant
  # sources.  Each data reference should be structured identifying the 
  # entity type::name::categoryoptioncombination(if necessary).  Note the 
  # double :: dividers. 
  # form type:
  # the type parameter supports "dataEntryForm" or "report" which will 
  # change the html tags that call the element ids
  # add_date_filter is used for analytics objects like charts and tables
  # if T, it will specify to specific reporting month. otherwise, the 
  # default value will be returned.  if relative timeframes, the date is used as
  # the end montt
  # The data imported here will be passed to createDHIS2_form() to create a payload
  # for upload to the server. Returns formatted html code.
  #
  # Ex. 
  # > scrapeDHIS2_form('path/to/form.html', usrname, password, server_url)
  

  # # Step 1: Get the data entry html
  ds <- getDHIS2_Resource('dataSets', usr, pwd, url, 'dataEntryForm')
  html_raw <- getDHIS2_elementInfo(ds$dataEntryForm.id[ds$displayName == form_name], 'dataEntryForms', usr, pwd, url)$htmlCode
  
  
  # Step 2: Replace the references in the file
  prepared <- findHTML_tags(html_raw)
  
  for (i in 1:nrow(prepared)) {
    # replace the placeholders with the html spans generated
    print(prepared$html[i])
    html_raw <- gsub(prepared$pattern[i], prepared$html[i], html_raw)
  }
  
  # Step 3: Prepare the JavaScript header
  base <- "
  
  <script src'//cdnjs.cloudflare.com/ajax/libs/numeral.js/1.4.5/numeral.min.js'></script>
  
  <script type='text/javascript'>
  $( document ).ready( function() {
  
  var date = dhis2.report.date;
  console.log(date); 
  
  var datestr=date.substring(0,4)+date.substring(5,7);
  var orgUnit = dhis2.report.organisationUnit;
  $('#orgUnit').html(orgUnit.name);
  $('#date').html(dhis2.report.date)
  console.log(orgUnit);
  
  --CHARTS--
  
  --REPORTTABLES--
  
  --MAPS--
  
  var DElements= [---OBJECT_IDS---];
  var DEValues = {};
  
  var lookup = DElements.join(';');
  $.get('../api/analytics?dimension=dx:'+lookup+'&dimension=pe:'+datestr+'&filter=ou:' + orgUnit.id)
  .fail(function(err){
  console.warn('Something bad happened',err);
  })
  .then(function (json) {
  for (var i = 0; i<json.rows.length; i++){
  console.log(json.rows[i]);
  DEValues[json.rows[i][0]]=json.rows[i][2];
  }
  
  for (i = 0; i < DElements.length; i++) {
  if (DEValues.hasOwnProperty(DElements[i])){
  var htmlElement=DElements[i].replace('.','');
  $('#'+htmlElement).html(DEValues[DElements[i]]);
  }
  
  }
  });
  }); 
  
  </script>
  "
  
  # the analytics query update
  ids <- prepared$ids[prepared$obj_type %in% c('dataElements', 'indicators')] #report allows for dhis.report objects or js objects, so exclude
  # collapse into a comma separated string to insert into js 
  ids %<>% paste0(., collapse="', '") %>% # condense each element down with internal single quotes and commas
    paste0("'",.,"'") # add the outside quotes on the string
  
  base <- gsub('---OBJECT_IDS---', ids, base)
  
  # update the analytics objects and make the JS scripts
  analytics_objects <- c('charts', 'reportTables', 'maps')
  analytics_strings <- lapply(analytics_objects, function(x) buildObjectQueries(prepared, x))
  
  for (i in 1:length(analytics_objects)) {
    base <- gsub(paste0('--', toupper(analytics_objects[i]), '--'), analytics_strings[i], base)
  }

  
  
  base <- paste0(base, '\n', add_js) # any other scripts can be defined in add_js. make sure it has the <script></script> tags
  
  
  
  # Step 4: Create the final html code to upload
  
  html_code <- paste0(base, "\n", html_raw) # concatenate the js and html together. 
  
  if (is.na(report_name)) report_name <- form_name
  
  if (post) {
    html_code <- postDHIS2_metaData(createDHIS2_report(report_name, html_code), usr, pwd, url, 'reports')
  }
  
  
  return(html_code)
}

findHTML_tags <- function(html_body, form_type='dataEntryForm', tag) {
  # search an html string for specific object (or any string)
  # return a list of indicies, object type, and values
  if (is.null(html_body)) stop("No html seems to be found.  Is the form name correct?")
  
  
  if (missing(tag)) {
    tag <- switch(form_type, 
                  'reports' = '<span id=(.*) />', 
                  'dataEntryForm' = '<input id=(.*) />')
  }

  
  
  # find the beginning of each of the patterns
  direct <- as.data.frame(str_locate_all(html_body, tag)[[1]])
  direct$pattern <- apply(direct, 1, function(i) substr(html_body, i[1], i[2]))
  direct$obj_type <- ifelse(grepl('indicatorid', direct$pattern), 'indicators', 'dataElements')
  
  new_direct <- data.frame()
  for (i in 1:nrow(direct)) {
    new_direct %<>% rbind.fill(convertHTML_tag(direct[i,], direct$obj_type[i], 'report'))
  }

  return(new_direct)
}

convertHTML_tag <- function(tag_df_row, type, convert_to='report') {
  # given a scraped html result from findHTML_tags, convert the 
  # scraped string into the alternative. Currently will account
  # for data elements and indicators in the page, but will need to be 
  # expanded for other data points potentially.  Returns a new data frame
  # with converted html tags, with the addition of an id column to be 
  # used in the JS insertion phase.
  tag_df_row %<>% as.data.frame()
  if (type == 'indicators') {
    srt <- 21
    end <- 31
  }
  else {
    srt <- 12
    end <- 34
  }
  tag_df_row$ids <- substr(tag_df_row$pattern, srt, end) %>% gsub('-', '.', .)
  tag_df_row$html <- paste0('<span id="',tag_df_row$id, '"></span>') %>% gsub('\\.', '', .)
  
  return(tag_df_row)
}













