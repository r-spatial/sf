FROM ubuntu:16.04
# minimal docker file to get sp and sf running on ubunty 16.04 image,
# using gdal/geos/proj from ppa:ubuntugis/ubuntugis-unstable

MAINTAINER "edzerpebesma"

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
	liblwgeom-dev \
	libudunits2-dev \
	make \
	wget \
	r-base-dev

RUN Rscript -e 'install.packages(c("devtools"), repos = "https://cloud.r-project.org")'
RUN Rscript -e 'devtools::install_github("edzer/sfr")'

## Check out & build R-devel:
RUN apt-get install -y subversion

RUN cd /tmp \
&& svn co https://svn.r-project.org/R/trunk R-devel

RUN apt-get install -y texinfo \
       texlive-base \
       texlive-extra-utils \
       texlive-fonts-extra \
       texlive-fonts-recommended \
       texlive-generic-recommended \
       texlive-latex-base \
       texlive-latex-extra \
       texlive-latex-recommended

## Build and install according the standard 'recipe' I emailed/posted years ago
RUN cd /tmp/R-devel \
	&& R_PAPERSIZE=letter \
		R_BATCHSAVE="--no-save --no-restore" \
		R_BROWSER=xdg-open \
		PAGER=/usr/bin/pager \
		PERL=/usr/bin/perl \
		R_UNZIPCMD=/usr/bin/unzip \
		R_ZIPCMD=/usr/bin/zip \
		R_PRINTCMD=/usr/bin/lpr \
		LIBnn=lib \
		AWK=/usr/bin/awk \
		CFLAGS=$(R CMD config CFLAGS) \
		CXXFLAGS=$(R CMD config CXXFLAGS) \
	./configure --enable-R-shlib \
               --without-blas \
               --without-lapack \
               --with-readline \
               --without-recommended-packages \
               --program-suffix=dev \
			   --with-x=no \
	&& make \
	&& make install \
	&& rm -rf /tmp/R-devel

## Set Renviron to get libs from base R install
RUN echo "R_LIBS=\${R_LIBS-'/usr/local/lib/R/site-library:/usr/local/lib/R/library:/usr/lib/R/library'}" >> /usr/local/lib/R/etc/Renviron

## Set default CRAN repo
RUN echo 'options(repos = c(CRAN = "https://cran.rstudio.com/"), download.file.method = "libcurl")' >> /usr/local/lib/R/etc/Rprofile.site

RUN cd /usr/local/bin \
&& mv R Rdevel \
&& mv Rscript Rscriptdevel \
&& ln -s Rdevel RD \
&& ln -s Rscriptdevel RDscript
