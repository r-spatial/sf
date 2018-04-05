library(sf)
library(DBI)
library(testthat)
context("sf: postgis using ODBC")

can_con <- function(x) inherits(x, "DBIObject")

db_drop_table_schema <- function(con, schema, table = NULL) {
    if (is.null(table)) {
        table <- paste(c("public", schema), collapse = ".")
    } else {
        table <- paste(c(schema, table), collapse = ".")
    }
    DBI::dbSendQuery(pg, paste("DROP TABLE ", table, " CASCADE;"))
}
require("sp")
data(meuse)
pts <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

epsg_31370 = paste0("+proj=lcc +lat_1=51.16666723333333 +lat_2=49.8333339 ",
                    "+lat_0=90 +lon_0=4.367486666666666 +x_0=150000.013 ",
                    "+y_0=5400088.438 +ellps=intl +towgs84=-106.869,52.2978,",
                    "-103.724,0.3366,-0.457,1.8422,-1.2747 +units=m +no_defs")

pg <- NULL
test_that("check utils", expect_false(can_con(pg)))
# requires to apt-get install odbc-postgresql
try(pg <- dbConnect(odbc::odbc(), "PostgreSQL"), silent=TRUE)
pg <- NULL
# tests ------------------------------------------------------------------------
test_that("can write to db", {
	skip_if_not(can_con(pg), "could not connect to postgis database")
	expect_silent(suppressMessages(st_write(pts, pg, "sf_meuse__")))
	expect_error(st_write(pts, pg, "sf_meuse__"), "exists")
	expect_true(st_write(pts, pg, "sf_meuse__", overwrite = TRUE))
	expect_true(st_write(pts, pg, "sf_meuse2__", binary = FALSE))
	expect_warning(z <- st_set_crs(pts, epsg_31370))
	expect_message(st_write(z, pg, "sf_meuse3__"), "Inserted local crs")
	expect_silent(st_write(z, pg, "sf_meuse3__", append = TRUE))
	expect_warning(expect_equal(nrow(DBI::dbReadTable(pg, "sf_meuse3__")), nrow(z) * 2), "Unknown field type")
	expect_silent(st_write(z, pg, "sf_meuse3__", overwrite = TRUE))
})

test_that("can handle multiple geom columns", {
	skip_if_not(can_con(pg), "could not connect to postgis database")
	multi <- cbind(pts[["geometry"]], st_transform(pts, 4326))
	expect_silent(st_write(multi, pg, "meuse_multi", overwrite = TRUE))
	expect_silent(x <- st_read("PG:host=localhost dbname=postgis", "meuse_multi", quiet = TRUE))
	expect_equal(st_crs(x[["geometry"]]), st_crs(multi[["geometry"]]))
	expect_equal(st_crs(x[["geometry.1"]]), st_crs(multi[["geometry.1"]]))
	expect_silent(x <- st_read("PG:host=localhost dbname=postgis", "meuse_multi", quiet = TRUE, type = c(1,4)))
	expect_silent(x <- st_read("PG:host=localhost dbname=postgis", "meuse_multi", quiet = TRUE, type = c(4,4)))
	expect_silent(x <- st_read("PG:host=localhost dbname=postgis", "meuse_multi", quiet = TRUE, promote_to_multi = FALSE))
	expect_silent(x <- st_read("PG:host=localhost dbname=postgis", "meuse_multi", quiet = TRUE, geometry_column = "geometry.1"))
	x <- st_layers("PG:host=localhost dbname=postgis")
	multi2 <- cbind(pts[["geometry"]], st_set_crs(st_transform(pts, 4326), NA))
	expect_silent(st_write(multi2, pg, "meuse_multi2", overwrite = TRUE))
	expect_silent(x <- st_read(pg, "meuse_multi2"))
	expect_equal(st_crs(x[["geometry"]]), st_crs(multi2[["geometry"]]))
	expect_equal(st_crs(x[["geometry.1"]]), st_crs(multi2[["geometry.1"]]))
	expect_silent(x <- st_read("PG:host=localhost dbname=postgis", "meuse_multi2", quiet = TRUE))
	#expect_equal(st_crs(x[["geometry"]]), st_crs(multi2[["geometry"]]))
	expect_equal(st_crs(x[["geometry.1"]]), st_crs(multi2[["geometry.1"]]))
})

test_that("sf can write units to database (#264)", {
	skip_if_not(can_con(pg), "could not connect to postgis database")
	ptsu <- pts
	ptsu[["u"]] <- ptsu[["cadmium"]]
	units(ptsu[["u"]]) <- units::make_unit("km")
	expect_silent(st_write(ptsu, pg, "sf_units__", overwrite = TRUE))
	r <- st_read(pg, "sf_units__")
	expect_is(r[["u"]], "numeric")
	expect_equal(sort(r[["u"]]), sort(as.numeric(ptsu[["u"]])))
	dbRemoveTable(pg, "sf_units__")
})

test_that("sf can preserve types (#592)", {
	skip_if_not(can_con(pg), "could not connect to postgis database")
	dtypes <- data.frame(
		logi = c(TRUE, FALSE, NA),
		chara = c("a", "", NA),
		nume = c(1.1e1, 2.2e2, NA),
		inte = c(1L, 2L, NA),
		fact = factor(c("a", "b", NA), levels = letters),
		#comp = c(complex(1, 2), complex(2, 3)),
		date = c(rep(Sys.Date(), 2), NA),
		time = c(rep(Sys.time(), 2), NA),
		x = c(1, 2, 4),
		y = c(1, 2, 4), stringsAsFactors = FALSE)
	# cannot write lists
	#dtypes$lst <- c(list(matrix("a")), list(matrix(c("b", "c"))), list(NA))
	dtypes <- st_as_sf(dtypes, coords = c("x", "y"))
	st_write(dtypes, pg, overwrite = TRUE)
	x <- st_read(pg, "dtypes")
	dtypes$fact <- as.character(dtypes$fact)
	expect_equal(x, dtypes)
	DBI::dbRemoveTable(pg, "dtypes")
})

test_that("can write to other schema", {
	skip_if_not(can_con(pg), "could not connect to postgis database")
	try(DBI::dbSendQuery(pg, "CREATE SCHEMA sf_test__;"), silent = TRUE)
	q <- "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'sf_test__';"
	suppressWarnings(could_schema <- DBI::dbGetQuery(pg, q) %>% nrow() > 0)

	skip_if_not(could_schema, "Could not create schema (might need to run 'GRANT CREATE ON DATABASE postgis TO <user>')")
	expect_error(st_write(pts, pg, Id(schema = "public", table = "sf_meuse__")), "exists")
	expect_silent(st_write(pts, pg, Id(schema = "sf_test__", table = "sf_meuse__")))
	expect_error(st_write(pts, pg, Id(schema = "sf_test__", table = "sf_meuse__")), "exists")
	expect_silent(st_write(pts, pg, Id(schema = "sf_test__", table = "sf_meuse__"), overwrite = TRUE))
	expect_warning(z <- st_set_crs(pts, epsg_31370))
	expect_silent(st_write(z, pg, Id(schema = "sf_test__", table = "sf_meuse33__")))
	expect_silent(st_write(z, pg, Id(schema = "sf_test__", table = "sf_meuse4__")))

	# weird name work
	expect_silent(st_write(pts, pg, c(NULL, "sf_test__.meuse__"), overwrite = TRUE))
	expect_silent(st_write(pts.2 <- pts, pg, overwrite = TRUE))
	expect_true(DBI::dbRemoveTable(pg, "pts.2 <- pts"))
})

test_that("support for capital names (#571)", {
	skip_if_not(can_con(pg), "could not connect to postgis database")
	expect_silent(st_write(pts, pg, "Meuse_tbl"))
	expect_true(DBI::dbRemoveTable(pg, "Meuse_tbl"))
	try(DBI::dbSendQuery(pg, "CREATE SCHEMA \"CAP__\";"), silent = TRUE)
	q <- "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'CAP__';"
	suppressWarnings(could_schema <- DBI::dbGetQuery(pg, q) %>% nrow() > 0)
	skip_if_not(could_schema, "Could not create schema (might need to run 'GRANT CREATE ON DATABASE postgis TO <user>')")
	expect_silent(st_write(pts, pg, Id(schema = "CAP__", table = "Meuse_tbl")))
	expect_true(DBI::dbRemoveTable(pg, Id(schema = "CAP__", table = "Meuse_tbl")))
	dbExecute(pg, 'DROP SCHEMA "CAP__" CASCADE;')
})

test_that("can read from db", {
	skip_if_not(can_con(pg), "could not connect to postgis database")
	q <- "select * from sf_meuse__"
	#expect_warning(x <- st_read(pg, query = q), "crs")
	expect_silent(x <- st_read(pg, query = q))

	expect_error(st_read(pg), "table name or a query")

	y <- st_read(pg, "sf_meuse__")
	expect_equal(dim(pts), dim(y))
	expect_identical(st_crs(pts), st_crs(y))
	expect_identical(st_precision(pts), st_precision(y))

	expect_warning(z <- st_read(pg, "sf_meuse3__"), "code \\d+ not found")
	expect_equal(dim(pts), dim(z))
	#expect_identical(st_crs(NA), st_crs(z))
	expect_true(st_crs(epsg_31370) == st_crs(z))
	expect_identical(st_precision(pts), st_precision(z))

	w <- st_read(pg, c("sf_test__", "sf_meuse__"))
	expect_equal(dim(y), dim(w))
	expect_identical(st_crs(y), st_crs(w))
	expect_identical(st_precision(y), st_precision(w))

	expect_error(st_read(pg, "missing"), "not exist")
	expect_error(st_read(pg, c("missing", "missing")), "not exist")
	# make sure it reads in the correct schema
	expect_error(st_read(pg, c("sf_test__", "sf_meuse3__")), "not exist")
})

test_that("can read views (#212)", {
	skip_if_not(Sys.getenv("USER") != "edzer") # this stopped working for me
	skip_if_not(can_con(pg), "could not connect to postgis database")
	expect_equal(DBI::dbExecute(pg,
								"CREATE VIEW sf_view__ AS SELECT * FROM sf_meuse__;"), 0)
	expect_equal(DBI::dbExecute(pg,
								"CREATE VIEW sf_test__.sf_view__ AS SELECT * FROM sf_meuse__;"), 0)
	expect_equal(DBI::dbExecute(pg,
								"CREATE MATERIALIZED VIEW sf_viewm__ AS SELECT * FROM sf_meuse__;"), 155)
	expect_equal(DBI::dbExecute(pg,
								"CREATE MATERIALIZED VIEW sf_test__.sf_viewm__ AS SELECT * FROM sf_meuse__;"), 155)
	x <- st_read(pg, "sf_meuse__")
	expect_identical(st_read(pg, "sf_view__"), x)
	expect_identical(st_read(pg, c("public", "sf_view__")), x)
	expect_identical(st_read(pg, c("sf_test__", "sf_view__")), x)
	expect_identical(st_read(pg, c("sf_viewm__")), x)
	expect_identical(st_read(pg, c("sf_test__", "sf_viewm__")), x)

	try(DBI::dbExecute(pg, "DROP VIEW sf_view__"), silent = TRUE)
	try(DBI::dbExecute(pg, "DROP VIEW sf_test__.sf_view__"), silent = TRUE)
	try(DBI::dbExecute(pg, "DROP MATERIALIZED VIEW sf_viewm__"), silent = TRUE)
	try(DBI::dbExecute(pg, "DROP MATERIALIZED VIEW sf_test__.sf_viewm__"), silent = TRUE)
})

test_that("round trips", {
	skip_if_not(can_con(pg), "could not connect to postgis database")
	round_trip = function(conn, wkt) {
		query = paste0("SELECT '", wkt, "'::geometry;")
		returnstr = suppressWarnings(DBI::dbGetQuery(conn, query)$geometry)
		wkb = structure(returnstr, class = "WKB")
		ret = st_as_sfc(wkb, EWKB = TRUE)
		message(paste("IN:  ", wkt, "\n"))
		# OUT contains WKB created in PostGIS from wkt, interpreted to R by sf, printed as WKT by sf
		message(paste("OUT: ", txt <- st_as_text(ret, EWKT=TRUE)[[1]], "\n"))
		if (length(grep("SRID", txt)) == 0) {
			query = paste0("SELECT ST_AsText('",sf:::CPL_raw_to_hex(st_as_binary(ret[[1]])),"');")
			received = suppressWarnings(DBI::dbGetQuery(conn, query)$st_astext)
			# PG: contains the PostGIS WKT, after reading the WKB created by sf from R native
			message(paste("PG:  ", received, "\n"))
		}
		expect_equal(wkt, txt)
	}
	round_trip(pg, "SRID=4326;POINT M (0 0 0)")
	round_trip(pg, "POINT Z (0 0 0)")
	round_trip(pg, "POINT ZM (0 0 0 0)")
	round_trip(pg, "POINT (0 0)")
	round_trip(pg, "LINESTRING (0 0, 1 1, 2 2)")
	round_trip(pg, "MULTIPOINT (0 0, 1 1, 2 2)")
	round_trip(pg, "POLYGON ((0 0, 1 0, 1 1, 0 0))")
	round_trip(pg, "MULTIPOLYGON (((0 0, 1 0, 1 1, 0 0)), ((2 2, 3 2, 3 3, 2 2)))")
	round_trip(pg, paste("MULTIPOLYGON (((0 0, 1 0, 1 1, 0 0),",
						 "(0.2 0.2, 0.8 0.2, 0.8 0.8, 0.2 0.2)),",
						 "((2 2, 3 2, 3 3, 2 2)))"))
	round_trip(pg, paste("MULTILINESTRING ((0 0, 1 0, 1 1, 0 0),",
						 "(0.2 0.2, 0.8 0.2, 0.8 0.8, 0.2 0.2),",
						 "(2 2, 3 2, 3 3, 2 2))"))

	# other types; examples taken from the PostGIS manuals (ch 4):
	round_trip(pg, "CIRCULARSTRING (0 0, 1 1, 1 0)")
	round_trip(pg, "CIRCULARSTRING (0 0, 4 0, 4 4, 0 4, 0 0)")
	round_trip(pg, paste("CURVEPOLYGON (CIRCULARSTRING (0 0, 4 0, 4 4, 0 4, 0 0),",
						 "LINESTRING (1 1, 3 3, 3 1, 1 1))"))
	round_trip(pg, paste("COMPOUNDCURVE (CIRCULARSTRING (0 0, 1 1, 1 0),",
						 "LINESTRING (1 0, 0 1))"))
	round_trip(pg, paste0("CURVEPOLYGON (COMPOUNDCURVE (CIRCULARSTRING (0 0, 2 0, 2 1, 2 3, 4 3), ",
						  "LINESTRING (4 3, 4 5, 1 4, 0 0)), ",
						  "CIRCULARSTRING (1.7 1, 1.4 0.4, 1.6 0.4, 1.6 0.5, 1.7 1))"))
	round_trip(pg, "MULTICURVE (LINESTRING (0 0, 5 5), CIRCULARSTRING (4 0, 4 4, 8 4))")
	round_trip(pg, paste("MULTISURFACE (CURVEPOLYGON (CIRCULARSTRING (0 0, 4 0, 4 4, 0 4, 0 0),",
						 "LINESTRING (1 1, 3 3, 3 1, 1 1)),",
						 "POLYGON ((10 10, 14 12, 11 10, 10 10),",
						 "(11 11, 11.5 11, 11 11.5, 11 11)))"))

	round_trip(pg, paste("MULTICURVE (LINESTRING (0 0, 5 5),",
						 "CIRCULARSTRING (4 0, 4 4, 8 4))"))
	round_trip(pg, paste("POLYHEDRALSURFACE Z (((0 0 0, 0 0 1, 0 1 1, 0 1 0, 0 0 0)),",
						 "((0 0 0, 0 1 0, 1 1 0, 1 0 0, 0 0 0)),",
						 "((0 0 0, 1 0 0, 1 0 1, 0 0 1, 0 0 0)),",
						 "((1 1 0, 1 1 1, 1 0 1, 1 0 0, 1 1 0)),",
						 "((0 1 0, 0 1 1, 1 1 1, 1 1 0, 0 1 0)),",
						 "((0 0 1, 1 0 1, 1 1 1, 0 1 1, 0 0 1)))"))
	round_trip(pg, "TRIANGLE ((0 0, 0 9, 9 0, 0 0))")
	round_trip(pg, "TIN Z (((0 0 0, 0 0 1, 0 1 0, 0 0 0)), ((0 0 0, 0 1 0, 1 1 0, 0 0 0)))")
})

test_that("can read using driver", {
	skip_if_not(can_con(pg), "could not connect to postgis database")
	layers <- st_layers("PG:host=localhost dbname=postgis")
	lyr_expect <- sort(c("sf_meuse__", "sf_meuse2__", "sf_meuse3__", "meuse_multi2",
						 "sf_test__.sf_meuse__",  "sf_test__.meuse__",
						 "sf_test__.sf_meuse33__", "sf_test__.sf_meuse4__"))
	expect_true(all(lyr_expect %in% layers$name))
	expect_true(all(layers$features == 155))
	expect_true(all(layers$fields == 12))

	skip_if_not(can_con(try(DBI::dbConnect(RPostgres::Postgres(), dbname = "empty"), silent=TRUE)),
				"could not connect to 'empty' database")
	expect_error(st_read("PG:host=localhost dbname=empty", quiet = TRUE), "No layers")
})

test_that("Can safely manipulate crs", {
	skip_if_not(can_con(pg), "could not connect to postgis database")
	srid <- 4326
	expect_true(get_postgis_crs(pg, srid) == st_crs(srid))
	expect_error(set_postgis_crs(pg, st_crs(srid)))
	expect_warning(expect_true(is.na(st_crs(get_new_postgis_srid(pg)))), "not found")
	new_crs <- st_crs(get_new_postgis_srid(pg), "+proj=longlat +datum=WGS84 +no_defs", valid = FALSE)
	expect_message(set_postgis_crs(pg, new_crs, auth_name = "sf_test"), "Inserted local crs")
	expect_warning(expect_error(set_postgis_crs(pg, new_crs), "duplicate key"),
				   "not found")
	expect_equal(delete_postgis_crs(pg, new_crs), 1)
	expect_equal(delete_postgis_crs(pg, new_crs), 0)
})

test_that("new SRIDs are handled correctly", {
	skip_if_not(can_con(pg), "could not connect to postgis database")
	data(meuse, package = "sp")
	meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = NA_crs_)

	crs = st_crs(NA_integer_, paste("+proj=sterea +lat_0=52 +lon_0=5", # creates FALSE, but new one
									"+k=1.0 +x_0=155000 +y_0=463000 +ellps=bessel",
									"+towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,",
									"-1.87740,4.0725 +units=m +no_defs"), valid = FALSE)
	st_crs(meuse_sf) = crs
	expect_message(st_write(meuse_sf, pg, overwrite = TRUE), "Inserted local crs")
	expect_warning(x <- st_read(pg, query = "select * from meuse_sf limit 3;"),
				   "not found in EPSG support files")
	expect_true(st_crs(x)$proj4string == crs$proj4string)
	expect_silent(st_write(meuse_sf, pg, overwrite = TRUE))
})

test_that("schema_table", {
	expect_error(sf:::schema_table(pg, NA), "character vector")
	expect_error(sf:::schema_table(pg, NA_character_), "cannot be NA")
	expect_error(sf:::schema_table(pg, "a", NA), "cannot be NA")
	expect_error(sf:::schema_table(pg, letters), "longer than 2")
	expect_equal(sf:::schema_table(pg, "a", "b"), c("b", "a"))
	expect_equal(sf:::schema_table(pg, "a"), c("public", "a"))
})

if (can_con(pg)) {
	# cleanup
	try(db_drop_table_schema(pg, "meuse_sf"), silent = TRUE)
	try(db_drop_table_schema(pg, "meuse_multi"), silent = TRUE)
	try(db_drop_table_schema(pg, "sf_meuse__"), silent = TRUE)
	try(db_drop_table_schema(pg, "sf_meuse2__"), silent = TRUE)
	try(db_drop_table_schema(pg, "sf_meuse3__"), silent = TRUE)
	try(db_drop_table_schema(pg, "meuse_multi2"), silent = TRUE)
	try(db_drop_table_schema(pg, '"sf_test__.meuse__"'), silent = TRUE)
	try(db_drop_table_schema(pg, "sf_test__", "sf_meuse__"), silent = TRUE)
	try(db_drop_table_schema(pg, "sf_test__", "sf_meuse2__"), silent = TRUE)
	try(db_drop_table_schema(pg, "sf_test__", "sf_meuse33__"), silent = TRUE)
	try(db_drop_table_schema(pg, "sf_test__", "sf_meuse4__"), silent = TRUE)
	try(DBI::dbExecute(pg, "DROP SCHEMA sf_test__ CASCADE;"), silent = TRUE)
	try(DBI::dbExecute(pg, "DELETE FROM spatial_ref_sys WHERE auth_name = 'sf';"), silent = TRUE)
	try(DBI::dbDisconnect(pg), silent = TRUE)
}
