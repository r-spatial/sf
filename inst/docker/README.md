# Build and check sf against r-release and r-devel

See [here](https://github.com/rocker-org/geospatial) for general docker files building R with a significant number of spatial extension packages and external dependencies, based on [rocker](https://github.com/rocker-org/rocker) and debian.

To allows building `sf` in alternative environments with all
external system requirements (udunits, proj, gdal, geos, lwgeom),
this directory has subdirectories with Docker files:

* base: for installing R-release with all system dependencies required by `sf`, 
* devel: for building R-devel from source (downloaded from svn, without X11) on top of that. 
* gdal22: for testing with newer gdal versions
* custom: for testing with libraries (gdal, geos, proj.4) installed in custom, non-standard directories

The images are built on ubuntu:16.04 (xenial). They use [ubuntugis-unstable](https://launchpad.net/~ubuntugis/+archive/ubuntu/ubuntugis-unstable) for GIS package system dependencies.

## run check under R release

In directory `base`, type

    docker build . -t sf

this will build a docker image of approx. 3.8 Gb size, called `sf`. It will also install package `sf` with its dependencies from CRAN, and run a check on the github sources.

Run a container from this image interactively with 

	docker run -ti sf

If you want to run it while mounting the current working directory (`pwd`) to `/pkg` inside the container, and want to remove the container after you exit, use:

	docker run -v `pwd`:/pkg --rm -ti sf

## build R-devel, check with R-devel

_After_ you built docker image `sf` above, on top of that you can install r-devel; it counts up to 4 Gb. Build it by executing the following command in the `devel` directory: 

    docker build . -t sf_devel

Run a container from this image interactively with

	docker run -ti sf_devel

to start R-devel in the container, use the `Rdevel` command. Building the image checks sf from github.

## with custom gdal, geos and proj installs
