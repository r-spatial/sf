FROM ubuntu:16.04
# minimal docker file to get sf running on an ubunty 16.04 image,
# installing gdal, geos and proj.4 from source in a non-standard location

MAINTAINER "edzerpebesma" edzer.pebesma@uni-muenster.de

RUN apt-get update && apt-get install -y software-properties-common
#RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable

RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/  " >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

RUN apt-get update
RUN apt-get upgrade -y

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y \
	libcurl4-openssl-dev \
	qpdf \
	pandoc \
	pandoc-citeproc \
	make \
	wget \
	git \
	subversion \
	libudunits2-dev \
	libsqlite3-dev \
	libexpat1-dev \
    libprotobuf-dev \
    libv8-3.14-dev \
    libcairo2-dev \
	protobuf-compiler \
	libxml2-dev \
	libpq-dev \
	libssh2-1-dev \
	r-base-dev


RUN export DEBIAN_FRONTEND=noninteractive; \
    add-apt-repository -y ppa:opencpu/jq; \
    apt-get update; \
    apt-get install -y \
    libjq-dev

RUN cd \
	&& wget http://download.osgeo.org/proj/proj-4.8.0.tar.gz \
	&& tar zxvf proj-4.8.0.tar.gz  \
	&& cd proj-4.8.0/ \
	&& ./configure \
	&& make \
	&& make install

# for now, rgdal needs:
#RUN cp /root/proj-4.8.0/src/projects.h /usr/include

RUN	cd \
	&& wget http://download.osgeo.org/gdal/2.0.1/gdal-2.0.1.tar.gz \
	&& tar zxvf gdal-2.0.1.tar.gz  \
	&& cd gdal-2.0.1 \
	&& ./configure \
	&& make \
	&& make install
	
RUN	cd \
	&& wget http://download.osgeo.org/geos/geos-3.4.0.tar.bz2 \
	&& bunzip2  geos-3.4.0.tar.bz2  \
	&& tar xvf geos-3.4.0.tar  \
	&& cd geos-3.4.0 \
	&& ./configure \
	&& make \
	&& make install
	
RUN ldconfig

RUN svn checkout svn://scm.r-forge.r-project.org/svnroot/rgdal/
RUN R CMD build rgdal/pkg --no-build-vignettes
RUN R -e 'install.packages(c("sp") , repos = "https://cran.uni-muenster.de")'
RUN R CMD INSTALL rgdal_*.tar.gz

RUN R -e 'install.packages("devtools", , repos = "https://cran.uni-muenster.de")'

RUN R -e 'devtools::install_github("r-spatial/sf", repos = "https://cran.uni-muenster.de")'

# no rgdal:
RUN R -e 'install.packages(c("Rcpp", "DBI", "units", "magrittr", "lwgeom", "maps", "rgeos", "sp", "raster", "spatstat", "tmap", "maptools", "RSQLite", "tibble", "pillar", "rlang", "dplyr", "tidyr", "RPostgres", "tidyselect", "ggplot2", "mapview", "testthat", "knitr", "covr", "microbenchmark", "rmarkdown", "RPostgreSQL", "devtools") , repos = "https://cran.uni-muenster.de")'

RUN R -e 'library(sf)'

RUN git clone https://github.com/r-spatial/sf.git

RUN	R CMD build sf

RUN	R CMD check --no-manual sf_*.tar.gz

CMD ["/bin/bash"]
