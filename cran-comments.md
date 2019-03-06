## Test environments
* local OS X install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
I have also run R CMD check on downstream dependencies of httr 
(https://github.com/wch/checkresults/blob/master/httr/r-release). 
All packages that I could install passed except:
  
  * Ecoengine: this appears to be a failure related to config on 
that machine. I couldn't reproduce it locally, and it doesn't 
seem to be related to changes in httr (the same problem exists 
                                       with httr 0.4).