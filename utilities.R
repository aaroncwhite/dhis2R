# UTILITIES --------------------------------------------------------------------
# Utility functions for finding multiple matches, splitting data into chunks for upload,
# dataframe scraping, or prompts.

# DATA MANIPULATION ----------------------------------------------------------
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
list_to_df <- function(list_obj, verb=F) {
  # take a single level list object with all of the same type (not nested)
  # and convert to a data frame.
  new_df <- data.frame()
  for (m in 1:length(list_obj)) {
    if (verb==T) {
      print(m)
    }
    new_df <- rbind.fill(new_df, list_obj[[m]])
  }
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

promptNumber <- function(message, min, max) {
  # prompt for a response with a value of a number
  # check that number fits within bounds of min and max
  resp <- readline(message)
  while (!(resp %in% min:max)) {
    resp <- readline(paste0(resp,' is outside of valid range(',min,', ',max,'). Please enter a valid number: '))
  }
  return(as.numeric(resp))
}
