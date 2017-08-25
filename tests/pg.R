library(sf)
library(testthat)

pg <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = "postgis")

    round_trip = function(conn, wkt, do_pg = TRUE) {
        query = paste0("SELECT '", wkt, "'::geometry;")
        returnstr = suppressWarnings(DBI::dbGetQuery(conn, query)$geometry)
        wkb = structure(returnstr, class = "WKB")
        ret = st_as_sfc(wkb, EWKB = TRUE)
        message(paste("IN:  ", wkt, "\n"))
        # OUT contains WKB created in PostGIS from wkt, interpreted to R by sf, printed as WKT by sf
        message(paste("OUT: ", txt <- st_as_text(ret, EWKT=TRUE)[[1]], "\n"))
        if (length(grep("SRID", txt)) == 0 && do_pg) {
            query = paste0("SELECT ST_AsText('",sf:::CPL_raw_to_hex(st_as_binary(ret[[1]])),"');")
            received = suppressWarnings(DBI::dbGetQuery(conn, query)$st_astext)
            # PG: contains the PostGIS WKT, after reading the WKB created by sf from R native
            message(paste("PG:  ", received, "\n"))
        }
        if (expect)
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
                         "((0 0 1, 1 0 1, 1 1 1, 0 1 1, 0 0 1)))"), sf::sf_extSoftVersion()["GDAL"] > "2.1.0")
    round_trip(pg, "TRIANGLE ((0 0, 0 9, 9 0, 0 0))", sf::sf_extSoftVersion()["GDAL"] > "2.1.0")
    round_trip(pg, "TIN Z (((0 0 0, 0 0 1, 0 1 0, 0 0 0)), ((0 0 0, 0 1 0, 1 1 0, 0 0 0)))", 
		sf::sf_extSoftVersion()["GDAL"] > "2.1.0")
