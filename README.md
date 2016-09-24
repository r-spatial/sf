# Simple Features for R

[![Build Status](https://travis-ci.org/edzer/sfr.png?branch=master)](https://travis-ci.org/edzer/sfr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/edzer/sfr?branch=master&svg=true)](https://ci.appveyor.com/project/edzer/sfr)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) 
[![Coverage Status](https://img.shields.io/codecov/c/github/edzer/sfr/master.svg)](https://codecov.io/github/edzer/sfr?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/sf)](http://cran.rstudio.com/package=sf) 

A package that provides [simple features access](https://en.wikipedia.org/wiki/Simple_Features) for R. It currently features:

Install by
```
library(devtools)
install_github("edzer/sfr")
```

To compile, the current package needs a C++ compiler, and binary development versions of GDAL 2.x and GEOS. For e.g. Ubuntu or debian operating systems, one could either

* install from source; see the [travis](https://github.com/edzer/sfr/blob/master/.travis.yml) config file for hints, or
* add [ubuntugis-unstable](http://ppa.launchpad.net/ubuntugis/ubuntugis-unstable/ubuntu/) to the package repositories, and then `sudo apt-get install libgdal-dev libgeos++-dev`.

With [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed, this currently works under Windows.

See also:

* the original R Consortium ISC [proposal](PROPOSAL.md),
* UseR! 2016 [slides presentations](http://pebesma.staff.ifgi.de/pebesma_sfr.pdf),
* Blog posts: [first](http://r-spatial.org/r/2016/02/15/simple-features-for-r.html), [second](http://r-spatial.org/r/2016/07/18/sf2.html).

### Features

* native R representation of all 17 simple feature types for all dimensions (XY, XYZ, XYM, XYZM)
* use S3 classes, simple feature sets are a list column in a `data.frame`
* full support for coordinate reference systems and transformations through PROJ.4
* interfaces to [GEOS](https://trac.osgeo.org/geos) and [GDAL](http://www.gdal.org/)
* fast I/O using [well-known-binary](https://en.wikipedia.org/wiki/Well-known_text#Well-known_binary), written in C++ and Rcpp; up to a factor 10 speedup compared to `rgdal` 
* direct reading and writing from and to spatial databases, such as [PostGIS](http://postgis.net/) using [DBI](https://cran.r-project.org/web/packages/DBI/index.html)
* GDAL reading supports driver dependent layer creation options
* GDAL reading and writing support for Date and DateTime (`POSIXct`) columns

### Acknowledgment

This project is being realized with financial [support](https://www.r-consortium.org/projects) from the

[![](https://www.r-consortium.org/sites/cpstandard/files/rconsort_logo_sml.png)](https://www.r-consortium.org/)
