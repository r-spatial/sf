# Simple Features for R

[![Build Status](https://travis-ci.org/edzer/sfr.png?branch=master)](https://travis-ci.org/edzer/sfr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/edzer/sfr?branch=master&svg=true)](https://ci.appveyor.com/project/edzer/sfr)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) 
[![Coverage Status](https://img.shields.io/codecov/c/github/edzer/sfr/master.svg)](https://codecov.io/github/edzer/sfr?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/sf)](http://cran.rstudio.com/package=sf) 

A package that provides [simple features access](https://en.wikipedia.org/wiki/Simple_Features) for R.

Install with:

```r
library(devtools)
install_github("edzer/sfr")
```

This currently works directly under windows with R 3.3 or newer when [Rtools](https://cran.r-project.org/bin/windows/Rtools/) is installed. For Mac, see [here](http://www.karambelkar.info/2016/10/gdal-2-on-mac-with-homebrew/). For Unix-alikes, a recent C++ compiler (c++11) and binary development versions of GDAL 2.x and GEOS are needed.

To install these libraries on Ubuntu, for example, either:

* add [ubuntugis-unstable](http://ppa.launchpad.net/ubuntugis/ubuntugis-unstable/ubuntu/) to the package repositories (e.g. with `sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable` and then `sudo apt-get install libgdal-dev libgeos++-dev`), or
* install from source; see the [travis](https://github.com/edzer/sfr/blob/master/.travis.yml) config file for hints.

See also:

* the original R Consortium ISC [proposal](PROPOSAL.md),
* UseR! 2016 [slides presentations](http://pebesma.staff.ifgi.de/pebesma_sfr.pdf),
* Blog posts: [first](http://r-spatial.org/r/2016/02/15/simple-features-for-r.html), [second](http://r-spatial.org/r/2016/07/18/sf2.html).
* The first [package vignette](https://edzer.github.io/sfr/articles/sfr.html)

### What it does

The sf package:

* represents natively in R all 17 simple feature types for all dimensions (XY, XYZ, XYM, XYZM)
* uses S3 classes: simple features are `data.frame` objects (or similar) with a geometry list column
* interfaces to [GEOS](https://trac.osgeo.org/geos) to support the [DE9-IM](https://en.wikipedia.org/wiki/DE-9IM)
* interfaces to [GDAL](http://www.gdal.org/) with driver dependent layer creation options, Date and DateTime (`POSIXct`) columns, and coordinate reference system transformations through [PROJ.4](http://proj4.org/)
* provides fast I/O with GDAL and GEOS using [well-known-binary](https://en.wikipedia.org/wiki/Well-known_text#Well-known_binary) written in C++ and Rcpp
* reads and writes from and to spatial databases such as [PostGIS](http://postgis.net/), using [DBI](https://cran.r-project.org/web/packages/DBI/index.html)

### Acknowledgment

This project is being realized with financial [support](https://www.r-consortium.org/projects) from the

<a href="https://www.r-consortium.org/projects/awarded-projects">
<img src="https://www.r-consortium.org/wp-content/uploads/2016/09/RConsortium_Horizontal_Pantone.png" width="400">
</a>
