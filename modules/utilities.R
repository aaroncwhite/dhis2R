# UTILITIES --------------------------------------------------------------------
# Utility functions for finding multiple matches, splitting data into chunks for upload,
# dataframe scraping, or prompts.

# DATA MANIPULATION ----------------------------------------------------------
make_revalue_map <- function(keys, values) {
  values %<>% as.character()
  names(values) <- keys
  return(values)
}


replaceNames <- function(table, column, idTable) {
  # Use the idTable of x type (dataElement, categoryOption, orgUnit, etc)
  # to overwrite the text values we have with the corresponding id values
  # in the dhis2 system. 
  
  # filter the unique values we want out of that idTable
  subset <- idTable[idTable$displayName %in% unique(table[,column]),]
  subIds <- subset$id
  names(subIds) <- subset$displayName
  table[,column] <- revalue(table[,column], subIds)
  return(table)
}

applyAge_categories <- function(tbl) {
  # Requires a column named 'age'
  tbl$age <- tbl[,grep('age', names(tbl))[1]]
  cats <- c(paste(seq(0, 95, by = 5), "-", seq (4,99, by = 5), sep = "")
            , "100+")
  
  tbl$age_cat[!is.na(tbl$age) & tbl$age >= 0] <- cats[floor(tbl$age[!is.na(tbl$age) & tbl$age >= 0]/5)+1]
  
  return(tbl)
}

all_character <- function(df) {
  # take a data frame and convert all columns to character class
  n <- names(df)
  for (i in 1:ncol(df)) {
    df[,i] <- as.character(df[,i])
  }
  # df <- as.data.frame(apply(df, 2, function(x) as.character(x))) # just make sure each column is a character class instead of factor
  names(df) <- n
  return(df)
}

fill_columns <- function(df, columns) {
  # take a dataframe from an excel worksheet that had
  # merged cells and fill the NA values with the preceeding
  # value for the stated column names
  for (cn in columns) {
    for (i in 1:length(df[,cn])) {
      if (is.na(df[i, cn]) & i != 1) {
        df[i, cn] <- df[i - 1, cn]
      }
    }
  }
  
  return(df)
}



# SEARCHING --------------------------------------------------------------------
greps <- function(search_terms, search_vector, ignoreCase=TRUE) {
  # functions as grep with an OR statement
  grepped <- vector()
  for (s in search_terms) {
    grepped <- c(grepped, grep(s, search_vector, ignore.case=ignoreCase))
  }
  return(unique(grepped))
}

greps_and <- function(stringList, vector, ignoreCase=TRUE) {
  # grep multiple segments with an AND function
  grepped <- data.frame(matrix(nrow=length(vector), ncol=0))
  for (st in stringList) {
    grepped[,st] <- grepl(st, vector, ignore.case=ignoreCase, perl = T)
  }
  grepped[,'all'] <- apply(grepped, 1, all) # this is going to be our result column where all things matched 
  # i.e. all of the columns say "TRUE"
  
  
  return(which(grepped$all == T))
}


findColumn_index <- function(find, df, head_message, prompt) {
  # will find column index for a stated string input (find) in a 
  # dataframe (df).  head_message will print before printing the
  # column names and prompt will display after to ask for user 
  # input. returns index number
  
  index <- grep(find, names(df))
  # check to make sure we only have one name column
  if (length(index) != 1) {
    cat(paste0(head_message,"\n"))
    print(as.data.frame(names(df)))
    index <- promptNumber(prompt, 1, ncol(df))
  }
  return(index)
}

# RECURSIVE LIST OPERATIONS ------------------------------------------------
find_replace.old <- function(obj, find, replace, ignore = NA, replaced=0) {
  # recursive function to evaluate a list of lists containing dhis2 metadata
  # whatever value is passed in for find will be substituted with replace
  # Evaluates recursively through a list object until it finds locations that 
  # are not more nested lists, then it attemps to perform the replace operation
  # Ex. 
  # If dataElements[[1]]$id == '1', set find = 1, replace = 2
  # response will be dataElements[[1]]$id == '2'
  # the replace operation if this is not a list
  if (!is.list(obj)) { 
    obj <- gsub(find, replace, obj)
    return(obj)
  }
  # Recurse if this is a list
  else {
    # look at all the sub elements and perform the same operation
    # as long as the length > 0 and the property is not declared in 
    # ignore
    
    for (i in 1:length(obj)) {
      
      if (is.na(ignore)) check_ignore <- T
      else check_ignore <- any(sapply(ignore, function(x) !grepl(x, names(obj[i]))))
      
      if (length(obj[[i]]) > 0 & check_ignore) {
        # replace in place
        obj[[i]] <- find_replace.old(obj[[i]], find, replace, ignore=ignore, replaced=replaced)
      }
      
    }
    # return the final modified list
    return(obj)
  }
}

find_replace <- function(obj, value_pairs, obj_map, property, parallel=F) {
  # map the specific property to replace
  # run through list of value pairs to replace
  # return final object back
  
  if (missing(obj_map) & !missing(property)) {
    cat('Making property map for', property, '\n')
    obj_map <- map_property(obj, property)
  }
  else if (missing(obj_map) & missing(property)) {
    stop('Both obj_map and property are missing.  Please define at least one.')
  }
  
  if (parallel) {
    cl <- makeCluster(detectCores() + 1)
    registerDoParallel(cl)
    obj_map %<>% split_map_nodes()
    
    obj[sapply(obj_map, function(j) j$node)] <- foreach(i=1:length(obj_map),.packages = 'magrittr', .export = c('find_replace', 'replace_property_value', 'follow_map', 'match_property_value')) %dopar% {
      find_replace(obj[[obj_map[[i]]$node]], value_pairs, obj_map[[i]]$obj_map, parallel=F)
    }
    stopCluster(cl)
    
    
  }
  else {
    for (i in 1:nrow(value_pairs)) {
      print(value_pairs[i,])
      obj <- replace_property_value(obj, value_pairs$find[i], value_pairs$replace[i], obj_map)
    }
  }

  return(obj)
}

map_property <- function(obj, property, prior=c(), name="", ignore=c('user', 'organisationUnits', 'userGroupAccesses')) {
  # recursive function to evaluate a list of lists containing dhis2 metadata
  # will search for stated property value ('id', 'name', etc) and return
  # the indices of the nested list and the property_value where that 
  # named property was found.  This does not search for specific values
  # but the named list elements that contain them.
  # Ex.
  # > obj <- getDHIS2_metadata(usr, pwd, url)
  # > x <- map_property(obj, 'id')
  # > x[[1]]
  # $indices
  # [1] 1 3
  # 
  # $property_value
  # [1] "SQX6LhW3eQi"
  if (is.null(name)) name <- ""
  
  
  if (!is.list(obj) & name %in% property) { 
    found <- list('indices' = prior,'property_value' = obj, 'property' = name)
    return(list(found))
  }
  # Recurse if this is a list
  else if (is.list(obj) & length(obj) > 0) {
    # look at all the sub elements and perform the same operation
    # as long as the length > 0 and the property is not declared in 
    # ignore
    n <- names(obj)
    skip <- (n %in% ignore)
    if (any(skip)) {
      n <- n[!skip]
      obj <- obj[!skip]
    }

    if (length(obj) > 0) {
      # using lapply is much faster since it uses C calls underneath the 
      # the hood.  based on tests it is 4x as fast for this operation when there
      # are multiple levels of nested properties (like ids which can be at the root level 
      # for a specific object and also nested deeper for the dependent relationships)
      recursed <- lapply(1:length(obj), function(i) {
        if (length(obj[[i]]) > 0) {
          # cat(n[i], i, '\n')
          prior <- c(prior, i)
          result <- map_property(obj[[i]], property=property, ignore=ignore, prior=prior, name=n[i])
          result
          
        }
      })
      
      # return the final modified list
      # unlist so we don't have the same nested structure at the end. 
      # this ensures all our results are at the same level
      
      return(unlist(recursed, recursive = F)) 
    }
  }
}

match_property_value <- function(obj_map, find_value) {
  # Find a value using the obj_map created by map_property and 
  # return a filtered obj_map for use with replace_property_value
  return(which(sapply(obj_map, function(x) grepl(find_value, x$property_value))))
}

follow_map <- function(obj, indices, find_value, replace_value) {
  # Recurse into a nested list using a vector of indices
  # when there are no more indices, replace the value and
  # return the nested list intact
  if (length(indices) == 1) {
    obj[[indices]] %<>% gsub(find_value, replace_value, .)
    return(obj)
  }
  else if (length(indices) > 1) {
    obj[[indices[1]]] <- follow_map(obj[[indices[1]]], indices[-1], find_value, replace_value)
    return(obj)
  }
  
}

replace_property_value <- function(obj, find_value, replace_value, obj_map) {
  # Find a specific property_value and replace with another in the main object
  # uses an obj_map created by map_property to define a set of indices and 
  # property values to run through.  This allows for faster replacement
  # since we don't need to recurse through the object multiple times
  # looking for different things
  
  matched_obj <- obj_map[match_property_value(obj_map, find_value)]
  
  for (i in matched_obj) {
    obj <- follow_map(obj, i$indices, find_value, replace_value)
  }
  return(obj)
}

split_map_nodes <- function(obj_map) {
  # split object map from map_property into nodes based on the 
  # first index value returned
  nodes <- sapply(obj_map, function(x) x$indices[1])
  obj_map_nodes <- lapply(unique(nodes), function(x) list('node' = x, 'obj_map' = lapply(obj_map[which(nodes %in% x)], function(y) {y$indices <- y$indices[-1]; y})))
  return(obj_map_nodes)
} 
# SPLITTING --------------------------------------------------------------------
calcSplits <- function(df, splitBy) {
  # Used by postDHIS2_Values(), this will split a data frame into 
  # equal parts for upload to the system.  splitBy determines how
  # many rows to attempt to include in each chunk. The function
  # also divides the remainder of nrow(df)/splitBy across each 
  # segment.  
  
  # Ex.-
  # > nrow(df)
  # [1] 1404
  # > calcSplits(df, 700)
  #   start end
  # 1 1     702
  # 2 703   1404
  
  # returns a dataframe with start and end points for each segment
  if (!is.data.frame(df)) df <- data.frame('col' = 1:length(df)) # this should handle passing it other classes of object
  # calculate how many splits we think we need
  
  nsplit <- floor(nrow(df)/splitBy) 
  split <- rep(splitBy, nsplit)
  remainder <- nrow(df) %% splitBy
  divide_across_all <- floor(remainder/nsplit)
  split <- split + divide_across_all
  divide_across_some <- remainder %% nsplit
  split[1:divide_across_some] <- split[1:divide_across_some] 
  
  cumulative_split <- cumsum(split)
  
  start <- cumulative_split - (split) + 1
  end <- cumulative_split
  end[length(end)] <- nrow(df)
  
  splits <- cbind('start' = start, 'end' = end)
  
  
  return(splits)
}

# LIST TO DATA FRAME -------------------------------------------------------------
list_to_df <- function(list_obj) {
  # take a single level list object with all of the same type (not nested)
  # and convert to a data frame.
  list_obj <- lapply(list_obj, as.data.frame)
  new_df <- as.data.frame(rbind.fill.matrix(list_obj))
  
  return(new_df)
}


# PROMPTS ------------------------------------------------------------------------
confirmAction <- function(message) {
  # Confirm action for given message. Will stop function from executing 
  # if answer is "N".
  
  resp <- toupper(readline(message))
  while (resp != "Y" & resp != "N") {
    resp <- toupper(readline('Invalid response. Please answer Y or N: '))
  }
  return(resp)
}

promptResponse <- function(message, valid_responses) {
  # prompt for any type of response and check that answer is acceptable
  resp <- readline(message)
  while (!(resp %in% valid_responses)){
    resp <- readline('Invalid response. Please re-enter response.')
  }
  return(resp)
}

promptNumber <- function(message, min, max, range=NA) {
  # prompt for a response with a value of a number
  # check that number fits within bounds of min and max
  
  if (!missing(min) & !missing(max)) {
    range <- min:max
  }
  
  resp <- readline(message) %>% as.numeric()
  while (!(resp %in% range)) {
    resp <- readline(paste0(resp,' is not a valid option. Please enter a valid number: ')) %>% as.numeric()
  }
  return(as.numeric(resp))
}
