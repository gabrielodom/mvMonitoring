
## Front Matter
This is my very first package submission. I hope to make your job reviewing this package as easy as possible.

Gabriel, 2017-10-18

## Test environments
* local macOS Sierra install, R 3.4.0
* Ubuntu 14.04.05 (on Travis-CI), R 3.4.1
* local Windows 7 Enterprise install, R 3.4.2

## R CMD check results
There were 0 ERRORs, 0 WARNINGs, and 0 NOTEs.

#### Request from Uwe Ligges (2017-10-18 3:54PM Eastern):
After preliminary checks, we changed the version number to 0.1.0 (something less ridiculous) and included linked papers and authorship information for the package. 

#### Request from Uwe Ligges (2017-10-18 4:21PM Eastern):
We changed the hyperlink in the description to the DOI (digital object identifier) link.

#### Request from Swetlana Herbrandt (2017-10-19 1:58PM Eastern):
Change

    @docType package
    @name mvMonitoring
    
to

    \doctype{package}
    \name{mvMonitoring}

in the mvMonitoring.Rd file.

In order to complete this request, we edited the mvMonitoring.R file (because the .Rd file was created by Roxygen), and removed excess whitespace in front of the doctype and name tags (they had four spaces instead of 1).


## Downstream dependencies
There are currently no downstream dependencies for this package.
