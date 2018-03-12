## Comments

It's a re-submission due to one of URLs is broken in the DESCRIPTION file, and now it's fixed. Thanks.

My apologies for submitting a new version since only 3 days after last update. I'm aware of CRAN polices, but I introduced an error to last version that isn't compatible with `matrix` class. It's likely to break users' code. I fix this issue along with others for this version. Sorry for the inconvenience.

## Test environments
* local OS X install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

There was 1 NOTE:

Maintainer: ‘Earo Wang <earo.wang@gmail.com>’

Days since last update: 3

## Reverse dependencies

I have run R CMD check on the 3 downstream dependencies. All packages that I could install passed except:

* corset: this appears to be a failure related to unit tests the author did for the package, as the class order has been slightly changed in the new release of hts.

* The corset maintainer was contacted and would make changes as soon as possible when the new version of hts is available.
