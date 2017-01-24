# rmnist

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/richfitz/rmnist.svg?branch=master)](https://travis-ci.org/richfitz/rmnist)

R interface to the [MNIST database of handwritten digits](http://yann.lecun.com/exdb/mnist/)

At present this package is a very basic interface that covers loading the data and caching the (fairly large) files that the database contains.  Note that the images are not directly included in the package as they are about 11MB.

See also [this gist](https://gist.github.com/brendano/39760) which also loads the images.
