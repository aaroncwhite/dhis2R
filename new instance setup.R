# Prepare a new R install to have the necessary packages to run the scripts
necessary <- c('httr', 'magrittr', 'RJSONIO', 'XLConnect', 'rlist', 'stringr', 
               'parallel', 'foreach', 'translateR', 'openxlsx', 'htmlTable')

install <- necessary[!(necessary %in% installed.packages()[,'Package'])]

install.packages(install)