FROM ubuntu:22.04
# minimal docker file to get sp and sf running on ubunty 16.04 image,
# using gdal/geos/proj from ppa:ubuntugis/ubuntugis-unstable

MAINTAINER "edzerpebesma" edzer.pebesma@uni-muenster.de

RUN apt-get update && apt-get install -y software-properties-common dirmngr
RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

# add the R 4.0 repo from CRAN -- adjust 'focal' to 'groovy' or 'bionic' as needed
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

RUN apt-key  adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

RUN apt-get update
RUN apt-get upgrade -y

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y \
	devscripts \
	gdb \
	git \
	libcairo2-dev \
	libcurl4-openssl-dev \
	libexpat1-dev \
	libpq-dev \
	libsqlite3-dev \
	libudunits2-dev \
	make \
	pandoc \
	qpdf \
	r-base-dev \
  	sqlite3 \
	subversion \
	valgrind \
	vim \
	tk-dev \
	wget

#libv8-3.14-dev \
RUN apt-get update && apt-get install -y --fix-missing \
	libjq-dev \
	libprotobuf-dev \
	libxml2-dev \
	libprotobuf-dev \
	protobuf-compiler \
	unixodbc-dev \
	libssh2-1-dev \
	libgit2-dev \
	libnetcdf-dev \
	locales \
	libssl-dev \
	libtiff-dev \
	cmake \
	libtiff5-dev \
	libopenjp2-7-dev

RUN locale-gen en_US.UTF-8

#ENV PROJ_VERSION=7.1.0
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

# GEOS:
ENV GEOS_VERSION 3.13.0

RUN wget -q http://download.osgeo.org/geos/geos-${GEOS_VERSION}.tar.bz2 \
  && bzip2 -d geos-*bz2 \
  && tar xf geos*tar \
  && cd geos* \
  && mkdir build \
  && cd build \
  && cmake .. \
  && make \
  && make install \
  && cd ../.. \
  && ldconfig

#RUN git clone --depth 1 https://github.com/OSGeo/PROJ.git
# https://download.osgeo.org/proj/proj-9.0.0RC1.tar.gz
ENV PROJ_VERSION 9.4.1
RUN wget -q http://download.osgeo.org/proj/proj-${PROJ_VERSION}.tar.gz
RUN tar zxvf proj-${PROJ_VERSION}.tar.gz
RUN cd proj* \
  && ls -l \
  && mkdir build \
  && cd build \
  && cmake .. \
  && make \
  && make install \
  && cd ../.. \
  && ldconfig

# install proj-data:
#RUN cd /usr/local/share/proj \
#  && wget http://download.osgeo.org/proj/proj-data-1.1RC1.zip \
#  && unzip -o proj-data*zip \
#  && rm proj-data*zip \
#  && cd -

# GDAL:
ENV GDAL_VERSION 3.9.2
ENV GDAL_VERSION_NAME 3.9.2
#https://download.osgeo.org/gdal/3.9.0/gdal-3.9.0beta1.tar.gz

RUN wget -q http://download.osgeo.org/gdal/${GDAL_VERSION}/gdal-${GDAL_VERSION_NAME}.tar.gz \
  && tar -xf gdal-${GDAL_VERSION_NAME}.tar.gz \
  && cd gdal* \
  && mkdir build \
  && cd ./build \
  && cmake -DCMAKE_BUILD_TYPE=Release .. \
  && make \
  && make install \
  && ldconfig

RUN Rscript -e 'install.packages(c("sp", "RPostgreSQL", "RSQLite", "testthat", "knitr", "tidyr", "geosphere", "maps", "microbenchmark", "raster", "dplyr", "tibble", "units", "DBI",  "covr", "protolite", "tmap", "mapview", "odbc", "pool", "rmarkdown", "RPostgres","spatstat", "stars", "XML", "tmaptools", "tmap", "terra", "PCICt", "RNetCDF", "future.apply", "ggforce", "ggthemes", "gstat", "ncmeta", "pbapply", "plm", "spacetime", "xts", "zoo", "tidync", "ncdfgeom", "cubelyr", "clue", "classInt", "mapdata", "ncdfgeom", "exactextractr", "randomForest", "nanoarrow", "mapview", "tmap", "spatstat", "spatstat.linnet", "viridis"))'

RUN git clone --depth 10 https://github.com/r-spatial/sf.git
RUN git clone --depth 10 https://github.com/r-spatial/lwgeom.git
RUN git clone --depth 10 https://github.com/r-spatial/stars.git
#RUN git config --global user.email "edzer.pebesma@uni-muenster.de"

RUN R CMD build --no-build-vignettes --no-manual lwgeom
RUN R CMD build --no-build-vignettes --no-manual sf
RUN R CMD build --no-manual stars
# RUN pkg-config proj --modversion
RUN R CMD INSTALL sf
RUN R CMD INSTALL lwgeom
RUN R CMD INSTALL stars
#
#RUN R CMD check --no-build-vignettes --no-manual --as-cran sf_*.tar.gz 
#RUN R CMD check --no-build-vignettes --no-manual --as-cran lwgeom_*.tar.gz 
#
#RUN Rscript -e 'options(timeout=1200); install.packages("starsdata", repos="http://cran.uni-muenster.de/pebesma/")'
#
#RUN _R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-build-vignettes --no-manual --as-cran stars_*.tar.gz 
