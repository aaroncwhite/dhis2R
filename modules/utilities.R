# UTILITIES --------------------------------------------------------------------
# Utility functions for finding multiple matches, splitting data into chunks for upload,
# dataframe scraping, or prompts.

check_responses <- function(response_results) {
  # Utility to check status codes from API operations on 
  # multiple objects
  return(table(sapply(response_results, function(x) x$status_code)))
}

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


find_replace <- function(obj, obj_map, property, parallel=F, nc=ceiling(detectCores()/2)) {
  if (missing(obj_map) & !missing(property)) {
    cat('Making property map for', property, '\n')
    obj_map <- map_property(obj, property)
  }
  else if (missing(obj_map) & missing(property)) {
    stop('Both obj_map and property are missing.  Please define at least one.')
  }
  if (!parallel) nc <- 1
  cl <- makeCluster(nc)
  registerDoParallel(cl)
  
  split_nodes <- split_map_nodes(obj_map)
  
  k <- foreach(j=split_nodes, 
              .packages='magrittr',
              .export='follow_map') %dopar% {
    for (i in j$obj_map) {
      obj[[j$node]] %<>% follow_map(i$indices, i$property_value)
    }
    obj[[j$node]]
  }
  stopCluster(cl)
  names(k) <- names(obj)[sapply(split_nodes, function(x) x$node)] 
  return(k)
}

replace_pairs <- function(value_pairs, obj_map, parallel=F, nc=ceiling(detectCores()/2)) {
  # map the specific property to replace
  # run through list of value pairs to replace
  # return final object back
  
  property_values <- sapply(obj_map, function(x) x$property_value)
  if (!parallel) nc <- 1
  
  cl <- makeCluster(nc)
  registerDoParallel(cl)

  updated_obj_map <- foreach(i=1:nrow(value_pairs),
                      .packages = 'magrittr',
                      .export = c('replace_property_value')
                      ) %dopar% {
    replace_property_value(value_pairs$find[i], value_pairs$replace[i], obj_map, property_values)
                                                        
  }
  stopCluster(cl)
    
  return(updated_obj_map)
}

map_property <- function(obj, property, prior=c(), name="", ignore=c('user','users', 'organisationUnits', 'userGroupAccesses')) {
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
      follow <- which(!skip)
    } 
    else {
      follow <- 1:length(obj)
    }

    if (length(obj) > 0) {
      # using lapply is much faster since it uses C calls underneath the 
      # the hood.  based on tests it is 4x as fast for this operation when there
      # are multiple levels of nested properties (like ids which can be at the root level 
      # for a specific object and also nested deeper for the dependent relationships)
      recursed <- lapply(follow, function(i) {
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

follow_map <- function(obj, indices, replace_value) {
  # Recurse into a nested list using a vector of indices
  # when there are no more indices, replace the value and
  # return the nested list intact
  if (length(indices) == 1) {
    obj[[indices]] = replace_value
    return(obj)
  }
  else if (length(indices) > 1) {
    obj[[indices[1]]] <- follow_map(obj[[indices[1]]], indices[-1], replace_value)
    return(obj)
  }
  
}

replace_property_value <- function(find_value, replace_value, obj_map, property_values) {
  # Find a specific property_value and replace with another in the main object
  # uses an obj_map created by map_property to define a set of indices and 
  # property values to run through.  This allows for faster replacement
  # since we don't need to recurse through the object multiple times
  # looking for different things
  if (missing(property_values)) {
    property_values <- sapply(obj_map, function(x) x$property_value)
    matched <- match_property_value(obj_map, find_value)
  }
  else {
    matched <- which(grepl(find_value, property_values))
  }

  for (i in matched) {
    obj_map[[i]]$property_value %<>% gsub(find_value, replace_value, .)
  }
  return(obj_map[matched])
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
  new_df <- as.data.frame(bind_rows(list_obj))
  
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
