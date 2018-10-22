FROM fedora:latest
# minimal docker file to get sp and sf running on ubunty 16.04 image,
# using gdal/geos/proj from ppa:ubuntugis/ubuntugis-unstable

MAINTAINER "edzerpebesma" edzer.pebesma@uni-muenster.de

RUN yum install -y gdal-devel geos-devel udunits2-devel
RUN yum install -y proj-devel proj-epsg proj-nad 
RUN yum install -y pandoc pandoc-citeproc
RUN yum install -y readline-devel curl-devel wget clang
RUN yum install -y R-devel
# needed by R packages:
RUN yum install -y libxml2-devel openssl-devel cairo-devel postgresql-devel unixODBC-devel

# install R-devel from source, with clang:
RUN wget https://stat.ethz.ch/R/daily/R-devel.tar.gz
RUN tar zxvf R-devel.tar.gz 
# Get https://www.stats.ox.ac.uk/pub/bdr/Rconfig/r-devel-linux-x86_64-fedora-clang into ./config.site:
RUN echo $'CC=clang \n\
OBJC=clang \n\
CXX=clang++ \n\
FC=gfortran \n\
F77=gfortran \n\
CFLAGS="-g -O3 -Wall -pedantic -mtune=native" \n\
FFLAGS="-g -O2 -mtune=native -Wall -pedantic" \n\
FCFLAGS="-g -O2 -mtune=native -Wall -pedantic" \n\
CXXFLAGS="-g -O3 -Wall -pedantic -mtune=native -frtti" \n\
CPPFLAGS="-I/usr/local/clang/include -I/usr/local/include" \n\
JAVA_HOME=/usr/lib/jvm/jre-11 \n\
LDFLAGS="-L/usr/local/clang/lib64 -L/usr/local/lib64"' > R-devel/config.site

RUN (cd R-devel; ./configure --with-x=no; make; make install)

RUN /usr/local/bin/Rscript -e 'install.packages(c("XML", "Rcpp", "units", "DBI", "classInt", "magrittr", "lwgeom", "tibble", "knitr", "sp", "maps", "markdown", "testthat", "maptools", "dplyr", "rgeos", "rgdal", "tidyr", "stars", "rmarkdown", "covr", "ggplot2", "mapview", "microbenchmark", "odbc", "pool", "raster", "rmarkdown", "RPostgres", "RPostgreSQL", "RSQLite", "spatstat", "tmap"), repos = "https://cloud.r-project.org")'

# get CRAN version of sf:
RUN cd ..
RUN wget https://cran.r-project.org/src/contrib/sf_0.7-0.tar.gz
RUN /usr/local/bin/R CMD check --as-cran sf_0.7-0.tar.gz

CMD ["/bin/bash"]
