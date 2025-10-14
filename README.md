# rmnist

<!-- badges: start -->
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/mrc-ide/rmnist/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mrc-ide/rmnist/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/mrc-ide/rmnist/graph/badge.svg)](https://app.codecov.io/gh/mrc-ide/rmnist)
<!-- badges: end -->

R interface to the [MNIST database of handwritten digits](https://en.wikipedia.org/wiki/MNIST_database)

At present this package is a very basic interface that covers loading the data and caching the (fairly large) files that the database contains.  Note that the images are not directly included in the package as they are about 11MB.

See also [this gist](https://gist.github.com/brendano/39760) which also loads the images.

## Usage

```r
d <- load_mnist(download_if_missing = TRUE)
d
## <mnist object of 10000 entries>
##   label: 7 2 1 0 4 1 4 9 5 9 0 6 9 0 1 5 9 7 3 4 9 6 6 5 4 0 7 4 0 1 3 1...
print(d[[1]], TRUE)
## <mnist digit (7)>
##
##
##
##
##
##
##
##        :*++:.
##        #%%%%%*********.
##        :=:=+%%#%%%%%%%=
##              : :::: %%-
##                    :%#
##                    %@:
##                   =%%.
##                  :%%:
##                  =%*
##                  #%:
##                 =%*
##                :%%:
##                #%+
##               #%#.
##              .%%:
##             .#%=
##             =%%.
##            :%%%.
##            =%%#.
##            =%#
## plot(d[[1]])
```
