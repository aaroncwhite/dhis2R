# Basic settings
sapply(list.files('modules/', full.names = T), source)


# Define credentials here.  Format:
# 'name of server' = list(child list with elements)
# Each named element (key/value pair) will be assigned into the global environment
# Ex. 
# Selecting 'demo' will create an object called 'usr' with a value of 'admin', etc.

credentials <- list('demo' = list('usr' = 'admin', 'pwd' = 'district', 'url' = 'https://play.dhis2.org/demo/api/',
                                     'dev.usr' = 'admin', 'dev.pwd' = 'district', 'dev.url' = 'http://play.dhis2.org/dev/dhis/api/'),
                    'another_server' = list('usr' = 'your_username', 'pwd' = 'your_password', 'url' = 'https://some.dhis2.url/api/')
                    
)

instance <- readline(sprintf('Select server credentials (%s):', paste(names(credentials), collapse = ", ")))

for (i in 1:length(credentials[[instance]])) {
  assign(names(credentials[[instance]][i]), unlist(credentials[[instance]][i]))
}
