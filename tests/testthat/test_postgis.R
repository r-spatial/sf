
library(sf)
library(testthat)
context("sf: postgis")

can_con <- function(x) inherits(x, "PostgreSQLConnection")

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
try(pg <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = "postgis"), silent=TRUE)

# tests ------------------------------------------------------------------------
test_that("can write to db", {
    skip_if_not(can_con(pg), "could not connect to postgis database")
    expect_error(st_write_db(), "connection")
    expect_silent(st_write_db(pg, pts, "sf_meuse__"))
    expect_error(st_write_db(pg, pts, "sf_meuse__"), "exists")
    expect_silent(st_write_db(pg, pts, "sf_meuse__", drop = TRUE))
    expect_silent(st_write_db(pg, pts, "sf_meuse2__", binary = FALSE))
    expect_warning(z <- st_set_crs(pts, epsg_31370))
    #expect_warning(st_write_db(pg, z, "sf_meuse3__",  binary = TRUE), "proj4")
    expect_silent(st_write_db(pg, z, "sf_meuse3__",  binary = TRUE))
})

test_that("sf can write units to database (#264)", {
    skip_if_not(can_con(pg), "could not connect to postgis database")
    ptsu <- pts
    ptsu[["length"]] <- ptsu[["cadmium"]]
    units(ptsu[["length"]]) <- units::make_unit("km")
    expect_silent(st_write_db(pg, ptsu, "sf_units__", drop = TRUE))
    r <- st_read_db(pg, "sf_units__")
    expect_is(r$length, "numeric")
    expect_equal(sort(r[["length"]]), sort(as.numeric(ptsu[["length"]])))
    try(db_drop_table_schema(pg, "sf_units__"), silent = TRUE)
})

test_that("can write to other schema", {
    skip_if_not(can_con(pg), "could not connect to postgis database")
    try(DBI::dbSendQuery(pg, "CREATE SCHEMA sf_test__;"), silent = TRUE)
    q <- "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'sf_test__';"
    could_schema <- DBI::dbGetQuery(pg, q) %>% nrow() > 0
    
    skip_if_not(could_schema, "Could not create schema (might need to run 'GRANT CREATE ON DATABASE postgis TO <user>')")
    expect_error(st_write_db(pg, pts, c("public", "sf_meuse__")), "exists")
    expect_silent(st_write_db(pg, pts, c("sf_test__", "sf_meuse__")))
    expect_error(st_write_db(pg, pts, c("sf_test__", "sf_meuse__")), "drop")
    expect_silent(st_write_db(pg, pts, c("sf_test__", "sf_meuse__"), drop = TRUE))
    expect_silent(st_write_db(pg, pts, c("sf_test__", "sf_meuse2__"), binary = FALSE))
    expect_warning(z <- st_set_crs(pts, epsg_31370))
    expect_silent(st_write_db(pg, z, c("sf_test__", "sf_meuse33__"),  binary = TRUE))
    expect_silent(st_write_db(pg, z, c("sf_test__", "sf_meuse4__"),  binary = FALSE))
    
    # weird name work, but create lots of noise from RPostgreSQL
    #expect_silent(st_write_db(pg, pts, c(NULL, "sf_test__.meuse__")))
})

test_that("can read from db", {
    skip_if_not(can_con(pg), "could not connect to postgis database")
    q <- "select * from sf_meuse__"
    #expect_warning(x <- st_read_db(pg, query = q), "crs")
    expect_silent(x <- st_read_db(pg, query = q))
    
    expect_error(st_read_db(), "no connection provided")
    expect_error(st_read_db(pg), "table name or a query")
    
    y <- st_read_db(pg, "sf_meuse__")
    expect_equal(dim(pts), dim(y))
    expect_identical(st_crs(pts), st_crs(y))
    expect_identical(st_precision(pts), st_precision(y))
    
    z <- st_read_db(pg, "sf_meuse2__")
    expect_equal(dim(pts), dim(z))
    expect_identical(st_crs(pts), st_crs(z))
    expect_identical(st_precision(pts), st_precision(z))
    
    z <- st_read_db(pg, "sf_meuse3__")
    expect_equal(dim(pts), dim(z))
    #expect_identical(st_crs(NA), st_crs(z))
    expect_identical(st_crs(epsg_31370), st_crs(z))
    expect_identical(st_precision(pts), st_precision(z))
    
    w <- st_read_db(pg, c("sf_test__", "sf_meuse__"))
    expect_equal(dim(y), dim(w))
    expect_identical(st_crs(y), st_crs(w))
    expect_identical(st_precision(y), st_precision(w))
    
    expect_error(st_read_db(pg, "missing"), "not exist")
    expect_error(st_read_db(pg, c("missing", "missing")), "not exist")
    # make sure it reads in the correct schema
    expect_error(st_read_db(pg, c("sf_test__", "sf_meuse3__")), "not exist")
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
    x <- st_read_db(pg, "sf_meuse__")
    expect_identical(st_read_db(pg, "sf_view__"), x)
    expect_identical(st_read_db(pg, c("public", "sf_view__")), x)
    expect_identical(st_read_db(pg, c("sf_test__", "sf_view__")), x)
    expect_identical(st_read_db(pg, c("sf_viewm__")), x)
    expect_identical(st_read_db(pg, c("sf_test__", "sf_viewm__")), x)
    
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
    layers <- st_layers("PG:dbname=postgis")
    lyr_expect <- sort(c("sf_meuse__", "sf_meuse2__", "sf_meuse3__", 
                    "sf_test__.sf_meuse__", "sf_test__.sf_meuse2__", 
                    "sf_test__.sf_meuse33__", "sf_test__.sf_meuse4__"))
    expect_equal(sort(layers$name), lyr_expect)
    expect_equal(layers$features, rep(155, length(lyr_expect)))
    expect_equal(layers$fields, rep(13, length(lyr_expect)))
    
    skip_if_not(can_con(RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = "empty")), 
                "could not connect to 'empty' database")
    expect_error(st_read("PG:dbname=empty", quiet = TRUE), "No layers")
})

test_that("multi geom columns work", {
    skip_if_not(can_con(pg), "could not connect to postgis database")
	cmd = "CREATE TABLE meuse_multi (id int4 PRIMARY KEY, zinc real);
SELECT AddGeometryColumn('', 'meuse_multi','geom_1',28992,'GEOMETRY', 2);
SELECT AddGeometryColumn('', 'meuse_multi','geom_2',28992,'GEOMETRY', 2);
INSERT INTO meuse_multi VALUES ( 1 , 1022 , ST_GeomFromText('POINT( 181072 333611 )', 28992),
ST_GeomFromText('MULTIPOINT( 181390 333260, 0 0)', 28992));
INSERT INTO meuse_multi VALUES ( 2 , 1141 , ST_GeomFromText('POINT( 181025 333558 )', 28992),
ST_GeomFromText('POINT( 181165 333370 )', 28992));
INSERT INTO meuse_multi VALUES ( 3 , 640 , ST_GeomFromText('POINT( 181165 333537 )', 28992),
ST_GeomFromText('POINT( 181027 333363 )', 28992));
INSERT INTO meuse_multi VALUES ( 4 , 257 , ST_GeomFromText('POINT( 181298 333484 )', 28992),
ST_GeomFromText('POINT( 181060 333231 )', 28992));
INSERT INTO meuse_multi VALUES ( 5 , 269 , ST_GeomFromText('POINT( 181307 333330 )', 28992),
ST_GeomFromText('POINT( 181232 333168 )', 28992));"
    try(DBI::dbExecute(pg, cmd), silent = TRUE)
	expect_silent(x <- st_read("PG:dbname=postgis", "meuse_multi", quiet = TRUE))
	expect_silent(x <- st_read("PG:dbname=postgis", "meuse_multi", quiet = TRUE, type = c(1,4)))
	expect_silent(x <- st_read("PG:dbname=postgis", "meuse_multi", quiet = TRUE, type = c(4,4)))
	expect_silent(x <- st_read("PG:dbname=postgis", "meuse_multi", quiet = TRUE, promote_to_multi = FALSE))
	expect_silent(x <- st_read("PG:dbname=postgis", "meuse_multi", quiet = TRUE, geometry_column = "geom_2"))
	x <- st_layers("PG:dbname=postgis")
})

test_that("new SRIDs are handled correctly", {
    skip_if_not(can_con(pg), "could not connect to postgis database")
	data(meuse, package = "sp")
	meuse_sf = st_as_sf(meuse, coords = c("x", "y"), crs = NA_crs_)

	crs = st_crs(paste("+proj=sterea  +lat_0=52 +lon_0=5", # creates FALSE, but new one
		"+k=1.0 +x_0=155000 +y_0=463000 +ellps=bessel",
		"+towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.87740,4.0725 +units=m +no_defs"))
	st_crs(meuse_sf) = crs
	expect_silent(st_write_db(pg, meuse_sf, drop = TRUE))
	expect_warning(x <- st_read_db(pg, query = "select * from meuse_sf limit 3;"), 
		"not found in EPSG support files")
	expect_true(st_crs(x) == crs)
})

if (can_con(pg)) {
    # cleanup
    try(db_drop_table_schema(pg, "meuse_sf"), silent = TRUE)
    try(db_drop_table_schema(pg, "meuse_multi"), silent = TRUE)
    try(db_drop_table_schema(pg, "sf_meuse__"), silent = TRUE)
    try(db_drop_table_schema(pg, "sf_meuse2__"), silent = TRUE)
    try(db_drop_table_schema(pg, "sf_meuse3__"), silent = TRUE)
    try(db_drop_table_schema(pg, '"sf_test__.meuse__"'), silent = TRUE)
    try(db_drop_table_schema(pg, "sf_test__", "sf_meuse__"), silent = TRUE)
    try(db_drop_table_schema(pg, "sf_test__", "sf_meuse2__"), silent = TRUE)
    try(db_drop_table_schema(pg, "sf_test__", "sf_meuse33__"), silent = TRUE)
    try(db_drop_table_schema(pg, "sf_test__", "sf_meuse4__"), silent = TRUE)
    try(DBI::dbSendQuery(pg, "DROP SCHEMA sf_test__ CASCADE;"), silent = TRUE)
    try(RpostgreSQL::dbDisconnect(pg), silent = TRUE)
}

test_that("schema_table", {
    expect_error(sf:::schema_table(NA), "character vector")
    expect_error(sf:::schema_table(NA_character_), "cannot be NA")
    expect_error(sf:::schema_table("a", NA), "cannot be NA")
    expect_error(sf:::schema_table(letters), "longer than 2")
    expect_equal(sf:::schema_table("a", "b"), c("b", "a"))
    expect_equal(sf:::schema_table("a"), c("public", "a"))
})
