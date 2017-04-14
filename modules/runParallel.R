library(doParallel)
library(foreach)

runParallel <- function(obj, apply_fun, result_combine=rbind, source_files=NA, n_cl=2, sink_out=T, sink_location='monitor') {
  # attempt at a wrapper function that will run a desired
  # chunk of code in parallel and sink results into an 
  # output file
  # depends on calcSplits from utilities.R
  
  # apply_fun can only take one parameter, and that is the sub_object
  # that it will process. 
  
  # combine determines how to stick the pieces back itno a larger whole
  
  # source_files is a list object of file paths to source 
  
  
  # make an output file for logging/debugging if things fail
  if (sink_out==T) {
    if (!dir.exists(sink_location)) {dir.create(path=sink_location,showWarnings = F)}
  } 

  # find the number of items we're dealing with
  s <- ifelse(is.data.frame(obj) | is.matrix(obj) , nrow(obj), length(obj))  
  splits <- calcSplits(as.data.frame(obj), floor(s/n_cl)) # split that into chunks
  # result is split based on number of processes desired with start and end indicies 
  # within the object
  cat(s, 'total objects to handle with', nrow(splits), 'processes.\n')
  
  cat('Make process cluster.\n')
  # spin up the R instances
  cl <- makeCluster(n_cl)
  registerDoParallel(cl)
  
  output <- foreach(j=1:nrow(splits), .combine= result_combine) %dopar% { # determine combination
    # function with result_combine.  rbind most likely for data frames and matrices
    # use append for lists
    # c to concatenate vectors. 
    
    # make the output files if desired
    if (sink_out == T) {sink(paste0(sink_location,"/", j, ".txt"))}
    # source any files necessary
    if (any(!is.na(source_files))) {sapply(source_files, function(a) source(a))}
    
    # make the smaller split of the object for this specific instance
    ifelse(any(is.data.frame(obj) | is.matrix(obj)), 
           sub <- obj[splits[j,'start']:splits[j,'end'],], # dataframes and matricies
           sub <- obj[splits[j,'start']:splits[j,'end']]) # lists and vectors
    # run the processing function
    apply_fun(sub) # this should be specified before running the process
    
  }
  
  # spin down the slave processes
  stopCluster(cl)
  
  return(output)
  
}

spin_up_clusters <- function(n_clust, sink='sink', source_files=NA) {
  # spin up clusters and sink their outputs to files in 'sink' dir 
  # tail -f each file to monitor output
  cl <- makeCluster(n_clust)
  registerDoParallel(cl)
  if (!dir.exists(sink)) dir.create(sink)
  foreach(i=1:n_clust) %dopar% {
    sink(paste0(sink,'/',i,'.txt'))
    sapply(source_files, source)
  }
  return(cl)
}
