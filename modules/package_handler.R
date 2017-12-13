build_requirements_file <- function(base_directory='./', out_dir = './', out_name = 'r-requirements.txt') {
  # Read lines from R or r files and find any time "library(.)" occurs. 
  # strip the library lines out and make a requirements file commented
  # by file name
  files <- grep('\\.R$|\\.r$|\\.Rmd', list.files(base_directory, recursive = T), value=T)
  f <- file(file.path(out_dir, out_name))
  
  libraries <- lapply(files, function(x) {
    x_lines <- readLines(x)
    x_lines <- unlist(strsplit(x_lines, ' '))
    libs <- grep('library\\(.*\\)', x_lines, value=T, perl=T)
    libs <- gsub('library\\(|\\)', '', libs, perl = T)
    libs <- libs[libs != "\".\""]
    # cat(sprintf("# %s", x), libs, file= f, sep='\n', append=T)
    paste(c(sprintf("# %s", x), libs), collapse="\n")
  })
  
  libraries <- paste(libraries, collapse="\n\n")
  cat('# R Requirements File',
      sprintf('# Generated %s', Sys.Date()),
      "# Use install_requirements() to install",
      "# _____________________________________",
      "", 
      libraries,
      file= f, sep='\n', append=T)
  close(f)
  return(libraries)
}

install_requirements <- function(requirements_file, only_missing=T) {
  # Install requirements listed in an file of libraries
  # one per line
  reqs <- readLines(requirements_file)
  reqs <- unique(reqs[!grepl('#', reqs) & reqs != ''])
  
  if (only_missing) {
    reqs = reqs[!(reqs %in% installed.packages()) & !grepl('\\.', reqs)]
  }
  if (length(reqs) > 0) {
    print(sprintf('Installing: %s', paste(reqs, collapse=', ')))
    sapply(reqs, function(x) install.packages(x, repos='https://cloud.r-project.org/'))
  }
  else warning('No packages to install!')
  
}