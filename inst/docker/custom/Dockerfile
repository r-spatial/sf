FROM ubuntu:16.04
# minimal docker file to get sf running on an ubunty 16.04 image,
# installing gdal, geos and proj.4 from source in a non-standard location

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
	cmake \
	libudunits2-dev \
	r-base-dev

RUN git clone https://github.com/r-spatial/sf.git

RUN cd \
	&& mkdir /opt/proj \
	&& wget http://download.osgeo.org/proj/proj-4.9.3.tar.gz \
	&& tar zxvf proj-4.9.3.tar.gz  \
	&& cd proj-4.9.3/ \
	&& ./configure --prefix=/opt/proj \
	&& make \
	&& make install

RUN	cd \
	&& wget http://download.osgeo.org/gdal/2.2.0/gdal-2.2.0.tar.gz \
	&& tar zxvf gdal-2.2.0.tar.gz  \
	&& cd gdal-2.2.0 \
	&& mkdir /opt/gdal \
	&& ./configure --prefix=/opt/gdal \
	&& make \
	&& make install
	
RUN	mkdir /opt/geos \
	&& cd \
	&& wget http://download.osgeo.org/geos/geos-3.7.0.tar.bz2 \
	&& bunzip2 -c geos-3.7.0.tar.bz2 | tar xvf - \
	&& cd geos-3.7.0 \
	&& mkdir build \
	&& cd build \
	&& cmake -DCMAKE_INSTALL_PREFIX:PATH=/opt/geos .. \
	&& make \
	&& make install
	
RUN R -e 'install.packages(c("Rcpp", "DBI", "units", "magrittr", "classInt"), repos = "https://cran.uni-muenster.de")'

#RUN	cd /usr/share \
#	&& mkdir proj \
#	&& cd proj \
#	&& wget https://download.osgeo.org/proj/proj-datumgrid-1.7.zip \
#	&& unzip proj*zip

RUN	cd / \
	&& (cd sf; git pull; autoconf) \
	&& R CMD build sf --no-build-vignettes \
	&& GDAL_DATA=/opt/gdal/share/gdal/ LD_LIBRARY_PATH=/opt/gdal/lib:/opt/geos/lib:/opt/proj/lib R CMD INSTALL --configure-args='--with-gdal-config=/opt/gdal/bin/gdal-config --with-proj-include=/opt/proj/include --with-proj-lib=/opt/proj/lib --with-geos-config=/opt/geos/bin/geos-config' sf_*.tar.gz  \
	&& LD_LIBRARY_PATH=/opt/gdal/lib:/opt/geos/lib:/opt/proj/lib R -e 'library(sf)'

CMD ["/bin/bash"]
