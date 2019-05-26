do_package_checks()

get_stage("install") %>%
	# install lwgeom with its own library since linking again postgis source install fails sometimes
	add_code_step(install.packages("lwgeom", configure.args="--without-liblwgeom"))

###
# deploy pkgdowm site
###
do_pkgdown(document = FALSE)
