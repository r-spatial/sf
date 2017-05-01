FROM ubuntu:16.04
# minimal docker file to get sp and sf running on ubunty 16.04 image,
# using gdal/geos/proj from ppa:ubuntugis/ubuntugis-unstable

ENV GDAL_VERSION 2.2.0
ENV GDAL_VERSION_NAME 2.2.0RC1
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

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
	libgdal-dev \
	libgeos-dev \
	libproj-dev \
	liblwgeom-dev \
	libudunits2-dev \
	postgis \
	r-base-dev

RUN wget http://download.osgeo.org/gdal/${GDAL_VERSION}/gdal-${GDAL_VERSION_NAME}.tar.gz \
  && tar -xf gdal-${GDAL_VERSION_NAME}.tar.gz \
  && cd gdal* \
  && ./configure \
  && make \
  && make install \
  && cd .. \

RUN Rscript -e 'install.packages("sf", dependencies = TRUE, repos = "https://cloud.r-project.org")'
RUN Rscript -e 'install.packages(c("dplyr", "tibble"), repos = "https://cloud.r-project.org")'
RUN git clone https://github.com/edzer/sfr.git
RUN R CMD build --no-build-vignettes --no-manual sfr
RUN Rscript -e 'install.packages(c("units"), repos = "https://cloud.r-project.org")'
RUN R CMD INSTALL sf_*tar.gz
RUN Rscript -e 'install.packages(c("sp","rgeos","rgdal","RPostgreSQL","RSQLite","testthat","knitr","tidyr","geosphere","maptools","maps","microbenchmark"), repos = "https://cloud.r-project.org")'

RUN apt-get install -y subversion
RUN svn checkout svn://scm.r-forge.r-project.org/svnroot/rgdal/
# or:
# svn checkout svn+ssh://edzer@scm.r-forge.r-project.org/svnroot/rgdal/

RUN R CMD build rgdal/pkg --no-build-vignettes
RUN R CMD INSTALL rgdal_1.2-7.tar.gz 
RUN R CMD check --no-build-vignettes --no-manual --as-cran sf_*tar.gz

CMD ["/bin/bash"]

