# Form Creation
# assumes other helper scripts have been loaded (api in particular)
library(openxlsx)
library(htmlTable)

scrapeDHIS2_form <- function(form_file, usr, pwd, url, type='report', add_js='') {
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
  #
  # The data imported here will be passed to createDHIS2_form() to create a payload
  # for upload to the server. Returns formatted html code.
  #
  # Ex. 
  # > scrapeDHIS2_form('path/to/form.xlsx', usrname, password, server_url)
  
  # Step 1: Import the excel file
  form <- readWorkbook(form_file,startRow = 0) # file should only contain one worksheet
  
  # Step 2: Replace the references in the file
  objects <- c('report', 'dataElements', 'indicators')
  # report will pull special values from the report itself. needs to be
  # declared in the javascript
  parsed <- findDHIS2_elements(form, objects)
  prepared <- replaceFormValues(parsed, usr, pwd, url, type=type)
  
  for (i in 1:nrow(prepared)) {
    form[prepared$row[i], prepared$col[i]] <- prepared$value[i]
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
          $('#'+htmlElement).html(numeral(DEValues[DElements[i]]).format('0,0'));
        }
    
    }
  });
  }); 
    
  </script>
  "
  ids <- prepared$ids[prepared$obj_type != 'report'] #report allows for dhis.report objects or js objects, so exclude
  # collapse into a comma separated string to insert into js 
  ids %<>% paste0(., collapse="', '") %>% # condense each element down with internal single quotes and commas
    paste0("'",.,"'") # add the outside quotes on the string
  
  base <- gsub('---OBJECT_IDS---', ids, base)
  base <- paste0(base, '\n', add_js) # any other scripts can be defined in add_js. make sure it has the <script></script> tags
  
  # Step 4: Create the final html code to upload
  form[is.na(form)] <- '' # replace any NA values with an empty string
  form_html <- knitr::kable(form, 'html', escape=F) # format the table as html
  form_html <- gsub("> NA <", "", form_html) # if the headers have NA values, remove those
  
  html_code <- paste(base, "\n", form_html) # concatenate the js and html together. 
  
  return(html_code)
}


findDHIS2_elements <- function(df, objects) {
  # search a dataframe for specific object (or any string)
  # return a list of indicies, object type, and values
  indices <- data.frame()
  for (obj in objects) { # look at all the object types passed, report
    # is special, the rest should match with the dhis2 api endpoints
    for (i in 1:ncol(df)) {
      # look through each column of the df
      matched <- grep(obj, df[,i]) # identify matched rows
      if (length(matched) > 0) { # if there are any add to the list of indices 
        indices <- rbind.fill(indices, data.frame('row' = matched, 
                                                  'col' = i, 
                                                  'obj_type' = obj, 
                                                  'value' = df[matched,i]))
      }

    }
  }
  indices$value <- as.character(indices$value)
  return(indices)
}

replaceFormValues <- function(parsed_form, usr, pwd, url, type='report') {
  # Read the parsed form values that need to be replaced,
  # look up each object type and find the matching id values
  if (type=='report') {
    wrap <- c("<span id=", "></span>")
  }
  else if (type == "dataEntryForm") {
    wrap <- c('input ids=', '></span>')
  }
  else {
    stop('Unrecognized report type...')
  }
    
  
  
  values <- sapply(parsed_form$value, function(x) strsplit(x, '\\[')) 
  values <- lapply(values, function(x) strsplit(x, '\\]'))
  
  originals <- list()
  # build a new list by just finding the object::name::child_name section
  # this is necessary to make sure there is no text before or after 
  for (row in 1:length(values)) {
    originals[[row]] <- grep('::', unlist(values[[row]]), value=T)
  }
  
  originals <- list_to_df(lapply(sapply(originals, function(x) strsplit(x, "::")), function(a) as.data.frame(t(a))))
  originals <- all_character(originals) # as.data.frame() converts to factors
  
  lookups <- originals[originals$V1 != 'report',]
  originals$ids <- originals$V2 # we're going to loop over the values and replace these. 
  
  for (obj in unique(lookups$V1)) {
    print(obj)
    obj_info <- getDHIS2_Resource(obj, usr, pwd, url)
    print(nrow(obj_info))
    ids <- obj_info$id[obj_info$displayName %in% lookups$V2]
    print(length(ids))
    names(ids) <-  obj_info$displayName[obj_info$displayName %in% lookups$V2]
    originals$ids <- revalue(originals$ids, ids)
  }
  
  originals$html <- sapply( gsub('report::', '', originals$ids), function(x) paste0(wrap[1],"'", x,"'", wrap[2], collapse=""))
  
  originals$ref <- apply(originals[,names(originals) %in% c('V1', 'V2', 'V3')], 1, function(x) paste0(x, collapse="::"))
  
  # the nested list of values contains where we need to replace plus and text before or after. 
  # let's convert it to a dataframe so replacing is easier. 
  values <- all_character(list_to_df(lapply(values, function(x) as.data.frame(t(unlist(x))))))
  originals <- cbind(originals, list_to_df(lapply(originals$ref, function(a) as.data.frame(which(values == a, arr.ind = T)))))
  
  for (i in 1:nrow(originals)) {
    values[originals$row[i], originals$col[i]] <- originals$html[i]
           
  }
  values[is.na(values)] <- ''
  
  # we've put the values back together, now put them back into parsed_form
  parsed_form$value <- apply(values, 1, function(a) paste(a, collapse=''))
  parsed_form$ids <- originals$ids
  
  return(parsed_form)
}






























