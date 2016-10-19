library(RJSONIO)
library(translateR)


# TRANSLATIONS -------------------------------------------------------------------------------------
createDHIS2_translationFile <- function(config_file, output_name=NULL, base_lang='en', new_TR=list('fr')) {
  # Scrape a configuration file and create a new file to ease translation by local
  # stakeholders.  
  # - output_name defines the output file, if none given, it will create a file with 'translations' appended
  # to the original config_file given. 
  # - base_lang defines the original language
  # - new_TR can be a list of multiple options for new translations.  this should be character elements, but 
  # match standard 2 character ISO naming conventions for languages as found at:
  # https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
  
  # import the configuration file
  config <- scrapeDHIS2_configFile(config_file)
  
  # we'll create a new worksheet for each type of object we want to translate
  # this object will have the following parts as a nested list:
  # - worksheet name
  #   - configElement: relates to the parts returned from scrapeDHIS2_configFile()
  #   - columns: the columns to pull out from the configElement defined in a list
  #     - vector of column names
  translate <- list('Data Elements' = list('configElement'= 'dataElements', 
                                           'columns'= list(c("dataElement","shortName","description","formName"))
  ),
  'Category Combinations' = list('configElement' = 'categoryCombos', 
                                 'columns'= list('Category.Combo.Name')
  ),
  'Categories' = list('configElement' = 'categories', 
                      'columns' = list('Category.Name')
  ),
  'Category Options' = list('configElement' = 'options', 
                            'columns' = list('options')
  ),
  'Datasets' = list('configElement'= 'dataSets',
                    'columns' = list('Dataset.Name'))
  )
  
  # make the translation workbook
  # check the file name
  if (is.null(output_name)) {output_name <- paste0('DHIS2 Translation Doc created ',Sys.Date(),'.xlsx')}
  
  # create the workbook
  wb <- loadWorkbook(output_name, create=T)
  
  for (ws in 1:length(translate)) {
    # make the worksheet tab
    ws_name <- names(translate)[ws]
    # build the table for translation ------
    # first grab the original columns
    to_translate <- config[[translate[[ws]]$configElement]][unlist(translate[[ws]]$columns)]
    
    # now we'll take each column and create a new column next to it for translation using new_TR
    translations <- data.frame(matrix(nrow=nrow(to_translate), ncol=0))
    
    for (col in 1:ncol(to_translate)) {
      # take the column and create two new columns of the same length but with null values
      trans <- to_translate[,col,drop=F]
      col_name <- names(trans)
      trans[,sapply(new_TR, function(x) paste0(col_name,'_', x))] <- ""
      names(trans)[1] <- paste0(col_name, '_', base_lang)
      # stick it onto the object wel'l output
      translations <- cbind.data.frame(translations, trans)
    }
    
    # now write that to the worksheet. using table1xls for this 
    # since I find it works better than writeWorksheet
    XLgeneric(wb, ws_name, translations)
  }
  saveWorkbook(wb)
  
}



downloadDHIS2_translationFile <- function(usr, pwd, url='https://zl-dsp.pih.org/api/', 
                                          base_lang='en', new_TR=list('fr'), attempt_translation=F, 
                                          obj_types=NULL, output_name=NULL) {
  # Similar to createDHIS2_translationFile(), this will create a translation doc using the API to download specified
  # object types and output a file for further translation. If obj_types is not stated, it will default to 
  # 'dataElements', 'categoryCombos', 'categories', 'categoryOptions', 'dataSets'
  
  out <- list()
  
  # Check if there are specified types, if not, set to defaults
  if (is.null(obj_types)) obj_types <- c('dataElements', 'categoryCombos', 'categories', 'categoryOptions', 'dataSets', 
                                         'dataElementGroups', 'dataElementGroupSets', 'charts', 'dashboards', 
                                         'indicators', 'indicatorGroups', 'indicatorGroupSets')
  
  
  out <- lapply(obj_types, function(x) downloadDHIS2_translations(x, usr, pwd, attempt_translation = attempt_translation))
  
  final_out <- list()
  for (i in 1:length(out)) {
    final_out <- append(final_out, out[[i]])
  }
  
  
  # now make a workbook
  if (is.null(output_name)) output_name <- paste0('DHIS2 Translation file created ', Sys.Date(),'.xlsx')
  new_file <- !(output_name %in% list.files())
  wb <- XLConnect::loadWorkbook(output_name, create=new_file)
  
  for (tr in 1:length(final_out)) {
    XLConnect::createSheet(wb, names(final_out[tr]))
    XLConnect::writeWorksheet(wb, final_out[[tr]], names(final_out[tr]))
  }
  
  XLConnect::saveWorkbook(wb)
  
  cat('\nTranslation file', output_name, 'successfully created and saved to working directory. \n')
  return(NULL)
}


scrapeDHIS2_translationFile <- function(translation_file) {
  # Import the translation file that was created using createDHIS2_translationFile()
  # that now has translations available. 
  wb <- XLConnect::loadWorkbook(translation_file)
  
  # we're going to import them using the sheet names and assume that they are correct
  sheets <- XLConnect::getSheets(wb)
  
  translations <- list()
  
  for (s in sheets) {
    translations[[s]] <- XLConnect::readWorksheet(wb, s)
  }
  
  return(translations)
}

uploadDHIS2_translations <- function(translation_file, usr, pwd, url='https://zl-dsp.pih.org/api/', new_TR=list('fr'), overwrite=F, prompt=T, verbose=F) {
  # Scrape a translation file and uploade to the system
  translations <- scrapeDHIS2_translationFile(translation_file)
  
  upload_results <- list()
  for (i in 1:length(translations)) {
    print(names(translations[i]))
    # each object has a name that corresponds with it's object type
    sub <- uploadDHIS2_objectTranslations(translations[[i]], names(translations[i]), usr, pwd, url, new_TR, overwrite, prompt, verbose)
    upload_results[[names(translations[i])]] <- append(upload_results, sub)
  }
  
  return(upload_results)
  
}

uploadDHIS2_objectTranslations <- function(df, obj_type, usr, pwd, url='https://zl-dsp.pih.org/api/', new_TR=list('fr'), overwrite=F, prompt=T, verbose = F) {
  # take data imported from translation file created using downloadDHIS2_translationFile()
  # and scraped from scrapeDHIS2_translationFile().  
  
  class_name <- switch(obj_type,
                       'dataElements' = 'DataElement',
                       'dataSets' = 'DataSet', 
                       'categoryOptions' = 'DataElementCategoryOption',
                       'categories' = 'DataElementCategory',
                       'categoryCombos' = 'DataElementCategoryCombo',
                       'dataSets_sections' = 'Section',
                       'charts' = 'BaseChart',
                       'dashboards' = 'Dashboard')
  
  if (obj_type == 'dataSets_sections') {
    obj_type <- 'sections'
    names(df) <- gsub('sections', 'name',names(df))
  }
  
  resp <- list()
  obj_table <- getDHIS2_Resource(obj_type, usr, pwd, url)
  for (tr in new_TR) { # look at each translation, make an upload object
    # find the base languages
    to_translate <- df[,c('name', grep(paste0("_", tr), names(df), value=T)), drop=F]
    
    # ignore the 'auto' tag for auto translated values
    to_translate <- to_translate[,grepl('auto', names(to_translate)) == F]
    # get the ids for the object type we're using
    
    obj_info <- getDHIS2_Resource(obj_type, usr, pwd, url)
    to_translate <- merge(to_translate, obj_info, by.x='name', by.y='displayName')
    
    # now lets transform it to value, property, class, object id, and locale
    to_translate <- to_translate[,names(to_translate)[!(names(to_translate) %in% 'name')]] # first get rid of the lingering name column
    
    # create an empty data frame to store the data
    translate <- data.frame()
    cols <- names(to_translate)
    cols <- cols[cols != 'id'] # find just the columns minus the id column
    
    # now we'll start rearranging by grabbing different columns each time as defined in cols
    for (i in cols) {
      sub <- to_translate[,c('id', i)] # grab the id too since we need it
      names(sub) <- c('objectId', 'value') # rename them
      sub$property <- gsub(paste0("_",tr), "", i) # add the property name, which is in the column name
      translate <- rbind.fill(translate, sub) # stick it onto the main translation object
      
    }
    translate$class <- class_name # class name is based on object type
    translate$locale <- tr # locale is based on the translation language 
    
    # get the current list of translation values
    current_translations <- list_to_df(lapply(unique(translate$objectId), function(x) getDHIS2_translationValues(x, tr, usr, pwd, url)))
    
    # add the href for the ones that exist so we know where to put it
    if (nrow(current_translations) > 0) {
      translate <- merge(translate, current_translations[,c('href','property', 'objectId'),drop=F], by.x=c('property', 'objectId'), by.y=c('property', 'objectId'), all=T)
      translate$href <- as.character(translate$href) # R is trying to be too smart
      
    }
    else {
      translate$href <- NA
    }
    
    
    # now let's upload this stuff
    for (i in 1:nrow(translate)) {
      if (translate[i,]$value != "" & !is.na(translate[i,]$value)) { # just double check that there's something here
        print(translate[i,])
        payload <- createDHIS2_translation(translate[i,]$value, translate[i,]$property, translate[i,]$locale, translate[i,]$objectId, translate[i,]$class)
        if (!is.na(translate[i,]$href) & overwrite == T) {
          # if there is an href, it means that there is already a translation that exists for that
          # object, let's double check a few things and then replace it
          
          if (verbose == T | prompt == T) {
            cat("\nTranslation for that object already exists and has the following options:\n")
            print(current_translations$value[current_translations$href == translate[i,]$href & current_translations$className == translate[i,]$class])
            cat('Attempted children to upload:\n')
            print(translate[i,]$value)
            ifelse(prompt==T, resp <- confirmAction('Overwrite existing translation? Y/N: '), resp <- "Y")
          }
          else {
            resp <- "Y"
          }
          if (resp == "Y") {
            result <- content(putDHIS2_metaData(payload, usr, pwd, translate[i,]$href, verbose=verbose))
            results[[tr]]$updated %<>% append(., list(list('name' = payload$name, 'response' = list(result))))
          }
        }
        else {
          results[[tr]]$uploaded <- append(results[[tr]]$uploaded, list(content(postDHIS2_metaData(payload, usr, pwd, url, type='translations'))))
          
        }
        
        
      } # end of upload
    } # end of loop
    
    
    
  } # end of translation locale loop
  
  return(results)
}

old.uploadDHIS2_translations <- function(translation_file, usr, pwd, url='https://zl-dsp.pih.org/api/', base_lang='en', new_TR=list('fr')) {
  translations <- scrapeDHIS2_translationFile(translation_file)
  base_lang <- paste0("_", base_lang)
  
  # This is going to take some work.  Need to use the english name to get back the id
  
  # options first
  upload <- translations$categoryOptions
  base <- upload[,grep(base_lang, names(upload), ignore.case = T)]
  
  results <- list()
  # split the data up between our base language and upload languages
  # column 1 should be the main column, but we'll make sure
  for (tr in 1:length(new_TR)) {
    trans <- upload[,grep(paste0("_",new_TR[[tr]]), names(upload), ignore.case = T)]
    results <- append(results, run_translations(base, trans, new_TR[[tr]],trans_class = 'DataElementCategoryOption', trans_element = 'name', obj_type = 'categoryOptions'))
  }
  
  
  # now categories
  upload <- translations$categories
  base <- upload[,grep(base_lang, names(upload), ignore.case = T)]
  
  # split the data up between our base language and upload languages
  # column 1 should be the main column, but we'll make sure
  for (tr in 1:length(new_TR)) {
    trans <- upload[,grep(paste0("_",new_TR[[tr]]), names(upload), ignore.case = T)]
    results <- append(results, run_translations(base, trans, new_TR[[tr]],trans_class = 'DataElementCategory', trans_element = 'name', obj_type = 'categories'))
  }
  
  # now category combinations
  upload <- translations$categoryCombos
  base <- upload[,grep(base_lang, names(upload), ignore.case = T)]
  
  # split the data up between our base language and upload languages
  # column 1 should be the main column, but we'll make sure
  for (tr in 1:length(new_TR)) {
    trans <- upload[,grep(paste0("_",new_TR[[tr]]), names(upload), ignore.case = T)]
    results <- append(results, run_translations(base, trans, new_TR[[tr]],trans_class = 'DataElementCategoryCombo', trans_element = 'name', obj_type = 'categoryCombos'))
  }
  
  # data sets
  upload <- translations$dataSets
  base <- upload[,grep(base_lang, names(upload), ignore.case = T)]
  
  # split the data up between our base language and upload languages
  # column 1 should be the main column, but we'll make sure
  for (tr in 1:length(new_TR)) {
    trans <- upload[,grep(paste0("_",new_TR[[tr]]), names(upload), ignore.case = T)]
    results <- append(results, run_translations(base, trans, new_TR[[tr]],trans_class = 'DataSet', trans_element = 'name', obj_type = 'dataSets'))
  }
  
  
  results <- list()
  # now data elements, this will take a little more work
  upload <- translations$dataElements
  base_de <- upload[,grep(base_lang, names(upload), ignore.case = T)]
  
  base <- base_de[,grep(paste0('dataElement',base_lang), names(base_de), ignore.case = T)]
  
  # split the data up between our base language and upload languages
  # column 1 should be the main column, but we'll make sure
  for (tr in 1:length(new_TR)) {
    trans <- upload[,paste0('dataElement',"_",new_TR[[tr]])]
    results <- append(results, run_translations(base, trans, new_TR[[tr]],trans_class = 'DataElement', trans_element = 'name', obj_type = 'dataElements'))
  }
  
  # split the data up between our base language and upload languages
  # column 1 should be the main column, but we'll make sure
  for (tr in 1:length(new_TR)) {
    trans <- upload[,paste0('shortName',"_",new_TR[[tr]])]
    results <- append(results, run_translations(base, trans, new_TR[[tr]],trans_class = 'DataElement', trans_element = 'shortName', obj_type = 'dataElements'))
  }
  
  # split the data up between our base language and upload languages
  # column 1 should be the main column, but we'll make sure
  for (tr in 1:length(new_TR)) {
    trans <- upload[,paste0('description',"_",new_TR[[tr]])]
    results <- append(results, run_translations(base, trans, new_TR[[tr]],trans_class = 'DataElement', trans_element = 'description', obj_type = 'dataElements'))
  }
  
  
  
  return(results)
}




# Translation functions for downloading/uploading dhis2 translations
# uses api.R as a base and it MUST be sourced before these will work.
# also relies on utilities.R for some helper functions. 

downloadDHIS2_translations <- function(obj_type, usr, pwd, url='https://zl-dsp.pih.org/api/', base_lang='en', new_TR=list('fr'), attempt_translation=F) {
  # download all translations available for a specific object type, like dataElements or dataSets.
  # This will download the child types as well, specified by child_download (below) which is set
  # to switch based on the obj_type input.  Since this won't change much, I'm leaving it here 
  # instead of parameterizing it. 
  
  # this switch will follow up on any child values that we also want to incorporate into the
  # translation doc for that object type.  For example, we want to translate a data set and
  # also it's section values.
  child_download <- switch(obj_type,
                           'default' = NULL, # If obj doesn't match, this will be returned
                           'dataSets' = list('sections')
  )
  
  # get the list of objects
  rsrc <- getDHIS2_Resource(obj_type, usr, pwd, url) 
  
  # based on the params we passed in child_download
  # make another list object to build the child translations
  # this will take the obj_type and append whatever 
  # the child type is. Ex. - dataSets_sections
  child_list <- list()
  for (sect in child_download) {
    n <- paste0(obj_type,"_",sect)
    child_list[n] <- list(data.frame())
  }
  
  trans <- list('main' = list())
  # now download the translations for each obj_id
  for (i in rsrc$id) {
    
    # download the main trainslation and child translations
    # for the individual object
    tr <- downloadDHIS2_translation(i, obj_type, usr, pwd, url, new_TR, child_types= child_download)
    trans$main <- append(trans$main, list(tr$main))
    
    # now for each of the child types we have
    # append it to the larger child list
    # Ex. dataSets_section <- append(dataSets_section, child-translations-for-ANC-Register)
    # we don't need the specifically associated together any more
    if (length(tr$children) > 0) { # first just check if there are any child sections to return
      for (j in 1:length(tr$children)) {
        n <- paste0(obj_type,"_",child_download[j]) # this is the child section name (dataSets_sections)
        
        # grow the list by 1 object of child translations for the latest main object we have
        # for that type (sections)
        child_list[[n]] <- append(child_list[[n]],
                                  list(tr$children[[j]]) # downloadDHIS2_translation already organizes things for us. 
        )
      }
    }
  }
  
  # right now, child_list has N first level elements, where N is equal to the
  # number of child sections.  The second level matches the number of 
  # child translations available for that type, but they're still all in 
  # separate lists.  Use list_to_df() to convert into one df per
  # child type. 

  trans$main <- list_to_df(trans$main)
  names(trans) <- obj_type # rename the main section to be obj_type
  
  # convert the child lists as well
  child_list <- lapply(child_list, list_to_df)
  
  trans <- append(trans, child_list)
  # if attempt_translation is set to TRUE, the function will attempt to 
  # translate blank elements in the target language using 
  # microsoft's translate api. note that the max quota is 2m characters. 
  # (Google Translate is no longer free)
  if (attempt_translation == T) {
    cat('\nAttempting Translations for',obj_type, '\n')
    for (tr in new_TR) {
      trans <- lapply(trans, function(x) translateDHIS2_elements(x, base_lang, tr))
    }
  }
  
  # return one final list with the main object type and any child types
  # on one level.  this will be used for excel translation sheet
  # creation.
  return(trans)
  
}

downloadDHIS2_translation <- function(obj_id, obj_type, usr, pwd, url='https://zl-dsp.pih.org/api/', new_TR=list('fr'), child_types=NULL) {
  # for an object, download the translations for the main object and any children if designated
  obj_info <- content(getDHIS2_elementInfo(content=F, obj_id, obj_type, usr, pwd, url))
  main_translations <- switch(obj_type,
                              'charts' = c('name', 'rangeAxisLabel', 'domainAxisLabel', 'title')
                              
  )
  
  if (is.null(main_translations)) main_translations <- c('name', 'shortName', 'description')  # these are attributes that we always want to look for (default)

  
  
  base_lang_values <- obj_info[names(obj_info) %in% main_translations]
  
  cat('\rDownloading:',base_lang_values$name,"\t\t\t\t\t\t\t\t\t\t\t\t")
  translation <- downloadDHIS2_objectTranslation(obj_id, usr, pwd, url, new_TR)
  
  translated <- as.data.frame.list(append(base_lang_values, translation))
  
  
  # check to see which ones we have and which we don't. there might be a name translation but no
  # shortName translation for example. We expected to see each element from base_lang_values
  # have a translation object for each language we've requested. 
  expected_translations <- unlist(lapply(new_TR, function(x) paste0(names(base_lang_values),"_",x)))
  missing_translations <- expected_translations[!(expected_translations %in% names(translated))]
  if (length(missing_translations) > 0) {
    # for the ones that are missing, we'll add a column with that name and an NA value
    for (m in missing_translations) {
      translated[,m] <- ""
    }
  }
  
  translated <- translated[,order(names(translated))]
  translation <- list()
  translation[['main']] <- translated[,greps(main_translations, names(translated))]
  
  
  
  # CHILD DOWNLOADS
  if (!is.null(child_types) & is.list(child_types)) {
    children <- list()
    for (ch in child_types) {
      # this will most likely just be one extra type of value
      # but we'll do it for all of them
      ids <- unlist(obj_info[names(obj_info) == ch])
      child <- list()
      base_child <- list()
      for (i in ids) {
        child <- append(child, unlist(downloadDHIS2_objectTranslation(i, usr, pwd, url, new_TR)))
        base_child <- append(base_child, content(getDHIS2_elementInfo(content=F, i, 'identifiableObjects', usr, pwd, url))$name)
      }
      
      base_child <- unlist(base_child)
      child <- unlist(child)
      
      if (length(base_child) == length(child) & length(base_child) > 0) {
        child <- cbind.data.frame(base_child, child)
        names(child) <- c(ch, paste0(ch, "_",unlist(new_TR)))
        child$parent <- obj_info$name
        
        children[[ch]] <- child
        
      }
    }
    translation$children <- children
    
    # end of child scraping
  }
  flush.console()
  
  return(translation)
  
}

downloadDHIS2_objectTranslation <- function(obj_id, usr, pwd, url='https://zl-dsp.pih.org/api/', new_TR=list('fr')) {
  # given a certain id, lookup translation values available and return the translations that already exist
  ids <- lapply(unlist(new_TR), function(x) queryDHIS2(paste0(obj_id,"&filter=locale:eq:",x), 'objectId', 'translations', usr, pwd, url))[[1]]
  translations <- list()
  for (j in ids) {
    tr <- j[c('property', 'value', 'locale')]
    translated <- list(tr$value)
    names(translated) <- paste0(tr$property,"_", tr$locale)
    #     j <- list(tr$id)
    #     names(j) <- paste0('id_',tr$property,"_",tr$locale)
    #     translated <- append(translated, j)
    translations <- append(translations, translated)
  }
  if (length(translations) > 0 ) {
    return(list(translations))
  }
  else {
    empt <- list("")
    # names(empt) <- paste0(tr$property,"_", tr$locale)
    
    return(empt)
  }
}


translateDHIS2_elements <- function(df, from, to) {
  # Attempt tranlsations using MS Translator API
  # This will search for headers with a suffix of _lang (Ex. _en or _fr)
  # and send them to the api for translation.  this only
  # will search for blank rows.
  
  # identify the target columns by splitting the
  # names by underscore and looking for names
  # that repeat ('name' and 'name_fr', etc)
  # this is in case we picked up some weird extra
  # columns (it's been happening)
  cols <- unlist(strsplit(names(df), '_')) 
  cols <- cols[cols != to] # remove the target lang tag
  cols <- cols[duplicated(cols)]
  
  auto_trans <- data.frame(matrix(nrow=nrow(df), ncol=length(cols)))
  names(auto_trans) <- paste0(cols,"_",to,"_auto")
  df <- cbind.data.frame(df, auto_trans)
  
  to_cols <- which(names(df) %in% paste0(cols, "_", to))
  from_cols <- which(names(df) %in% cols)
  
  df <- apply(df, 2, as.character)
    
  for (tr in 1:length(from_cols)) {
    tr_col <- paste0(cols[tr],"_",to,"_auto")

    filter <- df[,to_cols[tr]] == "" | is.na(df[,to_cols[tr]])
    x <- unique(df[filter ,from_cols[tr]])
    x <- x[!is.na(x)]
    x_tr <- suppressWarnings(translate(content.vec=as.character(x), microsoft.client.id = ms_id, 
                      microsoft.client.secret = ms_secret, source.lang = from, target.lang = to))
    names(x_tr) <- as.character(x)
    
    df[filter , to_cols[tr]] <- as.character(revalue(as.character(df[filter, from_cols[tr]]), x_tr, warn_missing = F))
    df[, tr_col] <- filter
  }
  

  return(as.data.frame(df))
  
}



translateVector <- function(v, lang) {
  # found a free translate API. considering diagnoses and such are pretty standard, this should work fine. 
  key <- 'trnsl.1.1.20150812T161627Z.388ea0c94434a06c.4df651bdf79cd6c3a67e6122666dc0acebb39938'
  url <- 'https://translate.yandex.net/api/v1.5/tr.json/translate?'
  url <- paste0(url, "key=", key, "&lang=", lang, "&options=1")
  
  
  # convert vector to string
  v <- text <- gsub(" ", "%20", v)
  v <- as.data.frame(v)
  v$char <- nchar(as.character(v[,1]))
  
  v$group <- 1
  # find break points
  if (sum(v$char) > 7000) {
    cat('Breaking vector into chunks for translation\n')
    i <- 1
    b <- 1
    start <- 1
    ngroups <- ceiling(sum(v$char)/7000)
    cat(ngroups, 'created')
    
    for (g in 1:ngroups) {
      while (sum(v$char[start:i]) <= 7000) {
        i <- i + 1
        if (i >= nrow(v)) {
          break
        }
      }
      v$group[start:i] <- g
      start <- i + 1
      i <- i + 1
    }
  }
  
  translated <- vector()
  for (g in unique(v$group)) {
    cat('\rTranslating group: ', g, "\n")
    text <- paste(v[v$group == g, 1], collapse="%0A")
    response <- fromJSON(paste0(url,'&text=', text))
    translated <- c(translated, strsplit(response$text, '\n'))
    Sys.sleep(1)
    flush.console()
  }
  
  return(unlist(translated))
  
}