# Simple Features for R

[![Build Status](https://travis-ci.org/r-spatial/sf.png?branch=master)](https://travis-ci.org/r-spatial/sf)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-spatial/sf?branch=master&svg=true)](https://ci.appveyor.com/project/edzerpebesma/sf)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-spatial/sf/master.svg)](https://codecov.io/github/r-spatial/sf?branch=master)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![CRAN](http://www.r-pkg.org/badges/version/sf)](https://cran.r-project.org/package=sf)
[![Downloads](http://cranlogs.r-pkg.org/badges/sf?color=brightgreen)](http://www.r-pkg.org/pkg/sf)

A package that provides [simple features access](https://en.wikipedia.org/wiki/Simple_Features) for R. Package sf:

* represents natively in R all 17 simple feature types for all dimensions (XY, XYZ, XYM, XYZM)
* uses S3 classes: simple features are `data.frame` objects (or `tibbles`) that have a geometry list-column
* interfaces to [GEOS](https://trac.osgeo.org/geos) to support the [DE9-IM](https://en.wikipedia.org/wiki/DE-9IM)
* interfaces to [GDAL](http://www.gdal.org/) with driver dependent dataset or layer creation options, Date and DateTime (`POSIXct`) columns, and coordinate reference system transformations through [PROJ.4](http://proj4.org/)
* provides fast I/O with GDAL and GEOS using [well-known-binary](https://en.wikipedia.org/wiki/Well-known_text#Well-known_binary) written in C++/Rcpp
* directly reads from and writes to spatial databases such as [PostGIS](http://postgis.net/) using [DBI](https://cran.r-project.org/web/packages/DBI/index.html)

### Blogs, presentations, vignettes

See also:

* package vignettes: [first](https://r-spatial.github.io/sf/articles/sf1.html), [second](https://r-spatial.github.io/sf/articles/sf2.html), [third](https://r-spatial.github.io/sf/articles/sf3.html), [forth](https://r-spatial.github.io/sf/articles/sf4.html), [fifth](https://r-spatial.github.io/sf/articles/sf5.html)
* blog posts: [first](http://r-spatial.org/r/2016/02/15/simple-features-for-r.html), [second](http://r-spatial.org/r/2016/07/18/sf2.html), [third](http://r-spatial.org/r/2016/11/02/sfcran.html), [fourth](http://r-spatial.org/r/2017/01/12/newssf.html)
* the original R Consortium ISC [proposal](PROPOSAL.md), the R Consortium [blog post](https://www.r-consortium.org/blog/2017/01/03/simple-features-now-on-cran)
* UseR! 2016 [presentation](http://pebesma.staff.ifgi.de/pebesma_sfr.pdf)


## Installling

Install either from CRAN with:
```r
install.packages("sf")
```
this will install binary packages on Windows and MacOS, unless you configured R such that it tries to install source packages; in that case, see below.

Install development versions from github with
```r
library(devtools)
install_github("r-spatial/sf")
```

### Windows

Installing sf from source works under windows when [Rtools](https://cran.r-project.org/bin/windows/Rtools/) is installed. This downloads the system requirements from [rwinlib](https://github.com/rwinlib/). (This does not include `liblwgeom`; it will need to be installed separately for functions that require it, such as `st_make_valid`, to work.)

### MacOS

One way to install the dependencies is using `sudo`; the other is using homebrew. For the latter,
see e.g. [here](http://www.karambelkar.info/2016/10/gdal-2-on-mac-with-homebrew/). Homebrew commands might be:

```
brew unlink gdal
brew tap osgeo/osgeo4mac && brew tap --repair
brew install proj
brew install geos
brew install udunits
brew install gdal2 --with-armadillo --with-complete --with-libkml --with-unsupported
brew link --force gdal2
```
after that, you should be able to install `sf` as a source package.

According to https://github.com/r-spatial/sf/issues/349, `brew install postgis` installs a working `liblwgeom`. In case of problems, search the issues for `brew` before opening a new one.

For MacOS Sierra, see
[these](https://stat.ethz.ch/pipermail/r-sig-mac/2017-June/012429.html)
instruction, using kyngchaos frameworks.

### Linux

For Unix-alikes, GDAL (>= 2.0.0), GEOS (>= 3.3.0) and Proj.4 (>= 4.8.0) are required; `liblwgeom` is optional.

#### Ubuntu
To install the dependencies on Ubuntu, either add [ubuntugis-unstable](http://ppa.launchpad.net/ubuntugis/ubuntugis-unstable/ubuntu/) to the package repositories:
```sh
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt-get update
sudo apt-get install libgdal-dev libgeos-dev libproj-dev libudunits2-dev liblwgeom-dev
```
or install dependencies from source; see e.g. an older [travis](https://github.com/r-spatial/sf/blob/593ee48b34001fe3b383ea73ea57063ecf690732/.travis.yml) config file for hints.

#### Fedora
The following command installs all required dependencies:
```sh
sudo dnf install gdal-devel proj-devel proj-epsg proj-nad geos-devel udunits2-devel
```

#### Other
To install on Debian, the [rocker geospatial](https://github.com/rocker-org/geospatial) Dockerfiles may be helpful. Ubuntu Dockerfiles are found [here](https://github.com/r-spatial/sf/tree/master/inst/docker).

### Contributing

* Contributions of all sorts are most welcome, issues and pull requests are the preferred ways of sharing them.
* When contributing pull requests, please adhere to the package style (in package code use `=` rather than `<-`; don't change indentation; tab stops of 4 spaces are preferred)
* This project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

### Acknowledgment

This project has been realized with financial [support](https://www.r-consortium.org/projects) from the

<a href="https://www.r-consortium.org/projects/awarded-projects">
<img src="http://pebesma.staff.ifgi.de/RConsortium_Horizontal_Pantone.png" width="400">
</a>
