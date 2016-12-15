# Prepare a new R install to have the necessary packages to run the scripts
necessary <- c('httr', 'magrittr', 'RJSONIO', 'XLConnect', 'rlist', 'stringr', 
               'doParallel', 'foreach', 'translateR', 'openxlsx', 'htmlTable','plyr')

install <- necessary[!(necessary %in% installed.packages()[,'Package'])]

install.packages(install)