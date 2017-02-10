context("sf: postgis")

can_con <- function(x) inherits(x, "PostgreSQLConnection")

db_drop_table_schema <- function(con, schema, table = NULL) {
    if (is.null(table)) {
        table <- paste(c("public", schema), collapse = ".")
    } else {
        table <- paste(c(schema, table), collapse = ".")
    }
    DBI::dbSendQuery(pg, paste("DROP TABLE", table))
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
    expect_silent(st_write_db(pg, pts, "sf_meuse__", overwrite = TRUE))
    expect_silent(st_write_db(pg, pts, "sf_meuse2__", binary = FALSE))
    expect_warning(z <- st_set_crs(pts, epsg_31370))
    expect_warning(st_write_db(pg, z, "sf_meuse3__",  binary = TRUE), "proj4")
})

test_that("can write to other schema", {
    skip_if_not(can_con(pg), "could not connect to postgis database")
    try(DBI::dbSendQuery(pg, "CREATE SCHEMA sf_test__;"), silent = TRUE)
    q <- "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'sf_test__';"
    could_schema <- DBI::dbGetQuery(pg, q) %>% nrow() > 0
    
    skip_if_not(could_schema, "Could not create schema (might need to run 'GRANT CREATE ON DATABASE postgis TO <user>')")
    expect_error(st_write_db(pg, pts, c("public", "sf_meuse__")), "exists")
    expect_silent(st_write_db(pg, pts, c("sf_test__", "sf_meuse__")))
    expect_error(st_write_db(pg, pts, c("sf_test__", "sf_meuse__")), "overwrite")
    expect_silent(st_write_db(pg, pts, c("sf_test__", "sf_meuse__"), overwrite = TRUE))
    expect_silent(st_write_db(pg, pts, c("sf_test__", "sf_meuse2__"), binary = FALSE))
    expect_warning(z <- st_set_crs(pts, epsg_31370))
    expect_warning(st_write_db(pg, z, c("sf_test__", "sf_meuse33__"),  binary = TRUE), "proj4")
    expect_warning(st_write_db(pg, z, c("sf_test__", "sf_meuse4__"),  binary = FALSE), "proj4")
    
    # weird name work, but create lots of noise from RPostgreSQL
    #expect_silent(st_write_db(pg, pts, c(NULL, "sf_test__.meuse__")))
})

test_that("can read from db", {
    skip_if_not(can_con(pg), "could not connect to postgis database")
    q <- "select * from sf_meuse__"
    expect_warning(x <- st_read_db(pg, query = q), "crs")
    
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
    expect_identical(st_crs(NA), st_crs(z))
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
        returnstr = suppressWarnings(dbGetQuery(conn, query)$geometry)
        wkb = structure(returnstr, class = "WKB")
        ret = st_as_sfc(wkb, EWKB = TRUE)
        message(paste("IN:  ", wkt, "\n"))
        # OUT contains WKB created in PostGIS from wkt, interpreted to R by sf, printed as WKT by sf
        message(paste("OUT: ", txt <- st_as_text(ret, EWKT=TRUE)[[1]], "\n"))
        if (length(grep("SRID", txt)) == 0) {
            query = paste0("SELECT ST_AsText('",sf:::CPL_raw_to_hex(st_as_binary(ret[[1]])),"');")
            received = suppressWarnings(dbGetQuery(conn, query)$st_astext)
            # PG: contains the PostGIS WKT, after reading the WKB created by sf from R native
            message(paste("PG:  ", received, "\n"))
        }
        expect_equal(wkt, txt)
    }
    round_trip(pg, "SRID=4326;POINTM(0 0 0)")
    round_trip(pg, "POINTZ(0 0 0)")
    round_trip(pg, "POINTZM(0 0 0 0)")
    round_trip(pg, "POINT(0 0)")
    round_trip(pg, "LINESTRING(0 0, 1 1, 2 2)")
    round_trip(pg, "MULTIPOINT(0 0, 1 1, 2 2)")
    round_trip(pg, "POLYGON((0 0, 1 0, 1 1, 0 0))")
    round_trip(pg, "MULTIPOLYGON(((0 0, 1 0, 1 1, 0 0)), ((2 2, 3 2, 3 3, 2 2)))")
    round_trip(pg, paste("MULTIPOLYGON(((0 0, 1 0, 1 1, 0 0),",
                         "(0.2 0.2, 0.8 0.2, 0.8 0.8, 0.2 0.2)),", 
                         "((2 2, 3 2, 3 3, 2 2)))"))
    round_trip(pg, paste("MULTILINESTRING((0 0, 1 0, 1 1, 0 0),", 
                         "(0.2 0.2, 0.8 0.2, 0.8 0.8, 0.2 0.2),", 
                         "(2 2, 3 2, 3 3, 2 2))"))
    
    # other types; examples taken from the PostGIS manuals (ch 4):
    round_trip(pg, "CIRCULARSTRING(0 0, 1 1, 1 0)")
    round_trip(pg, "CIRCULARSTRING(0 0, 4 0, 4 4, 0 4, 0 0)")
    round_trip(pg, paste("CURVEPOLYGON(CIRCULARSTRING(0 0, 4 0, 4 4, 0 4, 0 0),", 
                         "LINESTRING(1 1, 3 3, 3 1, 1 1))"))
    round_trip(pg, paste("COMPOUNDCURVE(CIRCULARSTRING(0 0, 1 1, 1 0),", 
                         "LINESTRING(1 0, 0 1))"))
    round_trip(pg, paste0("CURVEPOLYGON(COMPOUNDCURVE(CIRCULARSTRING(0 0, 2 0, 2 1, 2 3, 4 3), ", 
                          "LINESTRING(4 3, 4 5, 1 4, 0 0)), ", 
                          "CIRCULARSTRING(1.7 1, 1.4 0.4, 1.6 0.4, 1.6 0.5, 1.7 1))"))
    round_trip(pg, "MULTICURVE(LINESTRING(0 0, 5 5), CIRCULARSTRING(4 0, 4 4, 8 4))")
    round_trip(pg, paste("MULTISURFACE(CURVEPOLYGON(CIRCULARSTRING(0 0, 4 0, 4 4, 0 4, 0 0),",
                         "LINESTRING(1 1, 3 3, 3 1, 1 1)),", 
                         "POLYGON((10 10, 14 12, 11 10, 10 10),", 
                         "(11 11, 11.5 11, 11 11.5, 11 11)))"))
    
    round_trip(pg, paste("MULTICURVE(LINESTRING(0 0, 5 5),",
                         "CIRCULARSTRING(4 0, 4 4, 8 4))"))
    round_trip(pg, paste("POLYHEDRALSURFACEZ(((0 0 0, 0 0 1, 0 1 1, 0 1 0, 0 0 0)),", 
                         "((0 0 0, 0 1 0, 1 1 0, 1 0 0, 0 0 0)),",
                         "((0 0 0, 1 0 0, 1 0 1, 0 0 1, 0 0 0)),", 
                         "((1 1 0, 1 1 1, 1 0 1, 1 0 0, 1 1 0)),",
                         "((0 1 0, 0 1 1, 1 1 1, 1 1 0, 0 1 0)),",
                         "((0 0 1, 1 0 1, 1 1 1, 0 1 1, 0 0 1)))"))
    round_trip(pg, "TRIANGLE((0 0, 0 9, 9 0, 0 0))")
    round_trip(pg, "TINZ(((0 0 0, 0 0 1, 0 1 0, 0 0 0)), ((0 0 0, 0 1 0, 1 1 0, 0 0 0)))")
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

if (can_con(pg)) {
    # cleanup
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
    expect_error(schema_table(NA), "character vector")
    expect_error(schema_table(NA_character_), "cannot be NA")
    expect_error(schema_table("a", NA), "cannot be NA")
    expect_error(schema_table(letters), "longer than 2")
    expect_equal(schema_table("a", "b"), c("b", "a"))
    expect_equal(schema_table("a"), c("public", "a"))
})
