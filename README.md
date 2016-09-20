# Simple Features for R

[![Build Status](https://travis-ci.org/edzer/sfr.png?branch=master)](https://travis-ci.org/edzer/sfr)
[![Coverage Status](https://img.shields.io/codecov/c/github/edzer/sfr/master.svg)](https://codecov.io/github/edzer/sfr?branch=master)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) 
[![CRAN](http://www.r-pkg.org/badges/version/sf)](http://cran.rstudio.com/package=sf)[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/edzer/sfr?branch=master&svg=true)](https://ci.appveyor.com/project/edzer/sfr)


A package that provides [simple features access](https://en.wikipedia.org/wiki/Simple_Features) for R. It currently features:

* native representation of simple features in R
* interfaces to [GEOS](https://trac.osgeo.org/geos) and [GDAL](http://www.gdal.org/)
* direct reading from [PostGIS](http://postgis.net/) using [DBI](https://cran.r-project.org/web/packages/DBI/index.html)
* fast I/O using reading and writing of [well-known-binary](https://en.wikipedia.org/wiki/Well-known_text#Well-known_binary), written in C++ and Rcpp
* support for all 17 simple feature types, for all dimensions (XY, XYZ, XYM, XYZM)

Install by
```
library(devtools)
install_github("edzer/sfr")
```

To compile, the current package needs a C++ compiler, and binary development versions of GDAL 2.x and GEOS. For e.g. Ubuntu or debian operating systems, one could either

* install from source; see the [travis](https://github.com/edzer/sfr/blob/master/.travis.yml) config file for hints, or
* add [ubuntugis-unstable](http://ppa.launchpad.net/ubuntugis/ubuntugis-unstable/ubuntu/) to the package repositories, and then `sudo apt-get install libgdal-dev libgeos++-dev`.

See also:

* the original R Consortium ISC [proposal](PROPOSAL.md),
* UseR! 2016 [slides presentations](http://pebesma.staff.ifgi.de/pebesma_sfr.pdf),
* Blog posts: [first](http://r-spatial.org/r/2016/02/15/simple-features-for-r.html), [second](http://r-spatial.org/r/2016/07/18/sf2.html).

### Acknowledgment

This project is being realized with financial [support](https://www.r-consortium.org/projects) from the

[![](https://www.r-consortium.org/sites/cpstandard/files/rconsort_logo_sml.png)](https://www.r-consortium.org/)
