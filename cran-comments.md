## Test environments
* local OS X install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

There was 1 NOTE:

New maintainer:
  Earo Wang <earo.wang@gmail.com>
Old maintainer(s):
  Rob J Hyndman <Rob.Hyndman@monash.edu>

## Reverse dependencies

I have run R CMD check on the 3 downstream dependencies. All packages that I could install passed except:

* corset: this appears to be a failure related to unit tests the author did for the package, as the class order has been slightly changed in the new release of hts.

* The corset maintainers were notified of the release on RELEASE DATE.
