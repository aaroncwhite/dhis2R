# Prepare a new R install to have the necessary packages to run the scripts
necessary <- c('httr', 'magrittr', 'RJSONIO', 'XLConnect','rJava', 'rlist', 'stringr', 'stringi',
               'doParallel', 'foreach', 'translateR', 'openxlsx', 'htmlTable','plyr', 'RCurl')

install <- necessary[!(necessary %in% installed.packages()[,'Package'])]

if(length(install) == 0) {warning('It seems all the packages you need are installed.');stop()}


install <- necessary[!(necessary %in% installed.packages()[,'Package'])]

install.packages(install)


print('If you have not installed Java, please do so now.  XLConnect depends on it.')