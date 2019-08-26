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
	libsqlite3-dev \
	sqlite3 \
	qpdf \
	pandoc \
	make \
	wget \
	git \
	cmake \
	libudunits2-dev \
	r-base-dev

RUN git clone https://github.com/r-spatial/sf.git

RUN apt-get install -y  pkg-config

RUN cd \
	&& mkdir /opt/proj \
	&& wget http://download.osgeo.org/proj/proj-6.1.1.tar.gz \
	&& tar zxvf proj-6.1.1.tar.gz  \
	&& cd proj-6.1.1/ \
	&& ./configure --prefix=/opt/proj \
	&& make \
	&& make install install-am

RUN export PKG_CONFIG_PATH=/opt/proj/lib/pkgconfig \
	&& ls -la /opt/proj/lib/pkgconfig \
	&& pkg-config proj --modversion

RUN	cd \
	&& wget http://download.osgeo.org/gdal/3.0.1/gdal-3.0.1.tar.gz \
	&& tar zxvf gdal-3.0.1.tar.gz

	#&& export LD_LIBRARY_PATH=/opt/gdal/lib:/opt/geos/lib:/opt/proj/lib \

RUN cd \
	&& cd gdal-3.0.1 \
	&& mkdir /opt/gdal \
	&& ./configure --prefix=/opt/gdal --with-proj=/opt/proj \
	&& make \
	&& make install
	
RUN	mkdir /opt/geos \
	&& cd \
	&& wget http://download.osgeo.org/geos/geos-3.7.2.tar.bz2 \
	&& bunzip2 -c geos-3.7.2.tar.bz2 | tar xvf - \
	&& cd geos-3.7.2 \
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
	&& R CMD build sf --no-build-vignettes

# RUN PKG_CONFIG_PATH=/opt/proj/lib/pkgconfig PROJ_DATA=/opt/proj/share/proj GDAL_DATA=/opt/gdal/share/gdal/ LD_LIBRARY_PATH=/opt/gdal/lib:/opt/geos/lib:/opt/proj/lib R CMD INSTALL --configure-args='--with-gdal-config=/opt/gdal/bin/gdal-config --with-proj-include=/opt/proj/include --with-proj-lib=/opt/proj/lib --with-proj-share=/opt/proj/share/proj --with-proj-api=no --with-geos-config=/opt/geos/bin/geos-config' sf_*.tar.gz  \
# LD_LIBRARY_PATH=/opt/gdal/lib:/opt/geos/lib:/opt/proj/lib 

RUN PKG_CONFIG_PATH=/opt/proj/lib/pkgconfig \
	LD_LIBRARY_PATH=/opt/gdal/lib:/opt/geos/lib:/opt/proj/lib \
	PROJ_DATA=/opt/proj/share/proj \
	GDAL_DATA=/opt/gdal/share/gdal/ \
	R CMD INSTALL --configure-args='--with-gdal-config=/opt/gdal/bin/gdal-config --with-proj-include=/opt/proj/include --with-proj-lib=/opt/proj/lib --with-geos-config=/opt/geos/bin/geos-config' sf_*.tar.gz  \
	&& LD_LIBRARY_PATH=/opt/gdal/lib:/opt/geos/lib:/opt/proj/lib R -e 'library(sf)'

CMD ["/bin/bash"]
