FROM ubuntu:16.04
# minimal docker file to get sp and sf running on ubunty 16.04 image,
# using gdal/geos/proj from ppa:ubuntugis/ubuntugis-unstable

MAINTAINER "edzerpebesma" edzer.pebesma@uni-muenster.de

RUN apt-get update && apt-get install -y software-properties-common
RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable

RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/  " >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

RUN apt-get update
RUN apt-get upgrade -y

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y \
	libcurl4-openssl-dev \
	qpdf \
	pandoc \
	make \
	wget \
	git \
	subversion \
	libudunits2-dev \
	libexpat1-dev \
	libpq-dev \
	libsqlite3-dev \
	postgis \
	r-base-dev \
	gdb \
	valgrind \
	vim

# PROJ:
ENV PROJ_VERSION 4.9.3
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

RUN wget http://download.osgeo.org/proj/proj-${PROJ_VERSION}.tar.gz \
  && tar zxf proj-*tar.gz \
  && cd proj* \
  && ./configure \
  && make \
  && make install \
  && cd .. \
  && ldconfig

# install proj-datumgrid:
RUN cd /usr/local/share/proj \
  && wget http://download.osgeo.org/proj/proj-datumgrid-1.6.zip \
  && unzip proj-datumgrid*zip \
  && rm proj-datumgrid*zip \
  && cd -

# GEOS:
ENV GEOS_VERSION 3.6.2

RUN wget http://download.osgeo.org/geos/geos-${GEOS_VERSION}.tar.bz2 \
  && bzip2 -d geos-*bz2 \
  && tar xf geos*tar \
  && cd geos* \
  && ./configure \
  && make \
  && make install \
  && cd .. \
  && ldconfig

# GDAL:

ENV GDAL_VERSION 2.2.3
ENV GDAL_VERSION_NAME 2.2.3

RUN wget http://download.osgeo.org/gdal/${GDAL_VERSION}/gdal-${GDAL_VERSION_NAME}.tar.gz \
  && tar -xf gdal-${GDAL_VERSION_NAME}.tar.gz \
  && cd gdal* \
  && ./configure \
  && make \
  && make install \
  && cd .. \
  && ldconfig

RUN Rscript -e 'install.packages("sf", dependencies = TRUE, repos = "https://cloud.r-project.org")'

RUN Rscript -e 'install.packages(c("sp","rgeos","rgdal","RPostgreSQL","RSQLite","testthat","knitr","tidyr","geosphere","maptools","maps","microbenchmark", "raster", "dplyr", "tibble", "units", "DBI",  "covr"), repos = "https://cloud.r-project.org")'

RUN svn checkout svn://scm.r-forge.r-project.org/svnroot/rgdal/
# or:
# svn checkout svn+ssh://edzer@scm.r-forge.r-project.org/svnroot/rgdal/

# tmap:
RUN add-apt-repository -y ppa:opencpu/jq
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y \
	libv8-3.14-dev \
	libjq-dev \
	libprotobuf-dev \
	libxml2-dev \
	libprotobuf-dev \
	protobuf-compiler
RUN Rscript -e 'install.packages(c("protolite", "tmap"), repos = "https://cloud.r-project.org")'

RUN R CMD build rgdal/pkg --no-build-vignettes
RUN R CMD INSTALL rgdal_*.tar.gz 


RUN git  clone  https://github.com/r-spatial/sf.git

RUN R CMD build --no-build-vignettes --no-manual sf

RUN R CMD check --no-build-vignettes --no-manual --as-cran --run-dontrun sf_*tar.gz

CMD ["/bin/bash"]
