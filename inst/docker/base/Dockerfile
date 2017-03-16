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
 	gdal-bin \
	libcurl4-openssl-dev \
	libgdal-dev \
	libgeos-dev \
	libproj-dev \
	# liblwgeom-dev \
	liblwgeom-2.2-5 \
	libudunits2-dev \
	postgis \
	make \
	wget \
	r-base-dev

RUN Rscript -e 'install.packages(c("devtools"), repos = "https://cloud.r-project.org")'
RUN Rscript -e 'devtools::install_github("edzer/sfr")'

CMD ["/bin/bash"]
