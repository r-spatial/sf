add_package_checks()

get_stage("install") %>%
	# install lwgeom with its own library since linking again postgis source install fails sometimes
	add_code_step(install.packages("lwgeom", configure.args="--without-liblwgeom"))

get_stage("after_success") %>%
	add_code_step(system("dropdb postgis")) %>%
	add_code_step(system("createdb postgis")) %>%
	add_code_step(system("psql -d postgis -c")) # CREATE EXTENSION postgis
    add_code_step(system("psql -d postgis -c")) # GRANT CREATE ON DATABASE postgis TO travis

###
# deploy pkgdowm site
###
if (Sys.getenv("id_rsa") != "") {

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(orphan = TRUE, path = "docs", branch = "gh-pages"))
}
