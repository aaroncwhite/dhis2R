# Settings file to load all of the module files. 
options(java.parameters = "-Xmx4g" )

# Load the functions
sapply(list.files('../modules/'), function(x) source(paste0('../modules/',x)))

# Set DHIS2 API credentials
url = 'https://play.dhis2.org/demo/api/' # include the /api/ at the end of the address Ex. https://play.dhis2.org/demo/api/
usr = 'admin'
pwd = 'district'

# Translation service settings
# Will need an API id and Key from MS Translation service
ms_id = ''
ms_secret= ''


