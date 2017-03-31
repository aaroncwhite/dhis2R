# dhis2R
This suite of functions has developed over time as I have worked more with the backend API of the DHIS2 data management platform.  
The foundation can be found in api.R which has the main functions to talk back and forth with the API.  configuration.R and payload_creation.R 
build on the foundation and facilitate faster interactions for configuration uploading/downloading.

Because the DHIS2 API keeps changing, there's no guarantee this will continue to work in the long term.  I originally developed this on the 
v2.22 API and have updated most of api.R to work with v2.26 (current stable version as of January 2017). 

More information about DHIS2 can be found at www.dhis2.org. 
