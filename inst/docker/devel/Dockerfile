FROM sf:latest

MAINTAINER "edzerpebesma" edzer.pebesma@uni-muenster.de

## Check out & build R-devel:
RUN apt-get install -y subversion

RUN cd /tmp \
	&& svn co https://svn.r-project.org/R/trunk R-devel

RUN /tmp/R-devel/tools/rsync-recommended
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
               --with-recommended-packages \
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

# reinstall all pkgs:
RUN Rscriptdevel -e 'sel = installed.packages()[,"LibPath"] == "/usr/local/lib/R/site-library"; install.packages(names(which(sel)), dependencies = TRUE, repos = "https://cloud.r-project.org")'
RUN rm -fr sf.Rcheck
RUN RD CMD check --as-cran sf_*tar.gz

CMD ["/bin/bash"]
