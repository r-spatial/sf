#' Read PostGIS table directly, using DBI and binary conversion
#'
#' Read PostGIS table directly through DBI and RPostgreSQL interface, converting binary
#' @param conn open database connection
#' @param table table name
#' @param query SQL query to select records; see details
#' @param geom_column character or integer: indicator of name or position of the geometry column; if not provided, the last column of type character is chosen
#' @param EWKB logical; is the WKB is of type EWKB? if missing, defaults to \code{TRUE} if \code{conn} is of class code{PostgreSQLConnection} or \code{PqConnection}, and to \code{FALSE} otherwise
#' @details if \code{table} is not given but \code{query} is, the spatial reference system (crs) of the table queried is only available in case it has been stored into each geometry record (e.g., by PostGIS, when using EWKB)
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' conn = dbConnect(PostgreSQL(), dbname = "postgis")
#' x = st_read_db(conn, "meuse", query = "select * from meuse limit 3;")
#' x = st_read_db(conn, table = "public.meuse")
#' print(st_crs(x)) # SRID resolved by the database, not by GDAL!
#' dbDisconnect(conn)}
#' @name st_read
#' @details in case geom_column is missing: if table is missing, this function will try to read the name of the geometry column from table \code{geometry_columns}, in other cases, or when this fails, the geom_column is assumed to be the last column of mode character. If table is missing, the SRID cannot be read and resolved into a proj4string by the database, and a warning will be given.
#' @export
st_read_db = function(conn = NULL, table = NULL, query = NULL,
					  geom_column = NULL, EWKB, ...) {
	if (is.null(conn))
		stop("no connection provided")

	if (!is.null(table)) {
		table <- schema_table(table)
		if (!db_exists(conn, table))
			stop("`", paste0(table, collapse = "."), "` does not exist.", call. = FALSE)
		if (!is.null(query))
			warning("Ignoring query argument, only using table")
		query <- paste("SELECT * FROM", paste0(table, collapse = "."), ";")
	} else if(is.null(query)) {
		stop("Provide either a table name or a query", call. = FALSE)
	}

	# suppress warning about unknown type "geometry":
	suppressWarnings(tbl <- dbGetQuery(conn, query))
	if (is.null(tbl))
		stop("`", query, "` returned no results.", call. = FALSE) # nocov

	if("row.names" %in% colnames(tbl)) {
		row.names(tbl) = tbl[["row.names"]]
		tbl = tbl[,setdiff(colnames(tbl), "row.names")]
	}
	gc = try(dbReadTable(conn, "geometry_columns"))

	if (is.null(geom_column)) { # try find the geometry column:
		geom_column = if (class(gc) == "try-error" || is.null(table))
			tail(which(vapply(tbl, is.character, TRUE)), 1) # guess it's the last character column
		else
			gc[gc$f_table_schema == table[1] & gc$f_table_name == table[2], "f_geometry_column"]
	}
	crs = if (class(gc) == "try-error" || is.null(table))
			NA_crs_
		else {
			srid = gc[gc$f_table_schema == table[1] & gc$f_table_name == table[2], "srid"]
			if (srid != 0) # srid 0 is used for missing in postgis
				make_crs(get_postgis_crs(conn, srid))
			else
				NA_crs_
		}

	if (missing(EWKB))
		EWKB = inherits(conn, "PostgreSQLConnection") || inherits(conn, "PqConnection")

	geom = st_as_sfc(structure(tbl[[geom_column]], class = "WKB"), EWKB = EWKB, crs = crs)
	if (!is.null(attr(geom, "srid"))) {
		st_crs(geom) = make_crs(get_postgis_crs(conn, attr(geom, "srid")))
		attr(geom, "srid") = NULL
	}

	tbl[[geom_column]] = geom
	st_sf(tbl, ...)
}

#' Write simple feature table to a spatial database
#'
#' Write simple feature table to a spatial database
#' @param conn open database connection
#' @param table character; name for the table in the database, possibly of length 2, \code{c("schema", "name")}; default schema is \code{public}
#' @param geom_name name of the geometry column in the database
#' @param ... ignored for \code{st_write}, for \code{st_write_db} arguments passed on to \code{dbWriteTable}
#' @param drop logical; should \code{table} be dropped first?
#' @param append logical; append to table? (NOTE: experimental, might not work)
#' @param binary logical; use well-known-binary for transfer?
#' @param debug logical; print SQL statements to screen before executing them.
#' @name st_write
#' @export
#' @examples
#' \dontrun{
#'   library(sp)
#'   data(meuse)
#'   sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
#'   library(RPostgreSQL)
#'   conn = dbConnect(PostgreSQL(), dbname = "postgis")
#'   st_write_db(conn, sf, "meuse_tbl", drop = FALSE)
#' }
#' @details st_write_db was written with help of Josh London, see \url{https://github.com/r-spatial/sf/issues/285}
st_write_db = function(conn = NULL, obj, table = deparse(substitute(obj)), geom_name = "wkb_geometry",
		..., drop = FALSE, debug = FALSE, binary = TRUE, append = FALSE) {

	DEBUG = function(x) { if (debug) message(x); x }
	if (is.null(conn))
		stop("No connection provided")
	table <- schema_table(table)

	if (db_exists(conn, table)) {
		if (drop)
			DBI::dbGetQuery(conn, DEBUG(paste("drop table if exists", paste(table, collapse = "."), ";")))
		else
			stop("Table ", paste(table, collapse = "."), " exists already, use drop = TRUE",
					 call. = FALSE)
	}

	geom = st_geometry(obj)
	DIM = nchar(class(geom[[1]])[1]) # FIXME: is this correct? XY, XYZ, XYZM
	crs = st_crs(geom)
	SRID = crs$epsg
	if (is.na(SRID)) {
		SRID = if (is.na(crs$proj4string))
				0L
			else {
				srid = get_possibly_new_srid(conn, crs$proj4string, debug);
				attr(geom, "crs") = list(epsg = srid, proj4string = crs$proj4string, class = "crs")
				srid
			}
	}

	sfc_name = attr(obj, "sf_column")
	df = obj
	st_geometry(df) = NULL # is now data.frame
	df[[sfc_name]] <- if (binary)
			st_as_binary(geom, EWKB = TRUE, hex = TRUE)
 		else
			st_as_text(geom, EWKT = TRUE)

	dbWriteTable(conn, table, clean_columns(df, factorsAsCharacter = TRUE), ...)

	TYPE = class(geom[[1]])[2]
	if (! append) {
		query = DEBUG(paste0("SELECT AddGeometryColumn('", table[1],"','", table[2], "','", geom_name,
							 "','", SRID, "','", TYPE, "',", DIM, ");"))
		dbExecute(conn, query)
	}

	# convert text column `sfc_name' into geometry column `geom_name':
	query = if (binary)
		DEBUG(paste0("UPDATE ", paste(table, collapse = "."),
			" set ", geom_name," = ST_GeomFromEWKB(cast(", sfc_name, " as geometry));
			ALTER TABLE ", paste(table, collapse = "."),
			" DROP COLUMN IF EXISTS ", sfc_name))
	  else
		DEBUG(paste0("UPDATE ", paste(table, collapse = "."),
			" set ", geom_name," = ST_GeomFromEWKT(", sfc_name, ");
			ALTER TABLE ", paste(table, collapse = "."),
			" DROP COLUMN IF EXISTS ", sfc_name))
	invisible(dbExecute(conn, query))
}

schema_table <- function(table, public = "public") {
	if (!is.character(table))
		stop("table must be a character vector", call. = FALSE)

	if (length(table) == 1L)
		table = c(public, table)
	else if (length(table) > 2)
		stop("table cannot be longer than 2 (schema, table)", call. = FALSE)

	if (any(is.na(table)))
		stop("table and schema cannot be NA", call. = FALSE)

	return(table)
}

db_list_tables_schema <- function(con) {
	q <- paste("SELECT schemaname AS table_schema, tablename AS table_name",
			   "FROM pg_tables",
			   "WHERE schemaname !='information_schema'",
			   "AND schemaname !='pg_catalog';")
	DBI::dbGetQuery(con, q)
}

db_list_views_schema <- function(con) {
	q <- paste("SELECT table_schema, table_name",
			   "FROM information_schema.views",
			   "WHERE table_schema !='information_schema'",
			   "AND table_schema !='pg_catalog';")
	DBI::dbGetQuery(con, q)
}

db_list_mviews_schema <- function(con) {
	q <- paste("SELECT nspname as table_schema, relname as table_name",
			   "FROM pg_catalog.pg_class c",
			   "JOIN pg_namespace n ON n.oid = c.relnamespace",
			   "WHERE c.relkind = 'm'")
	DBI::dbGetQuery(con, q)
}

db_exists <- function(conn, name, ...) {
	lst <- rbind(db_list_views_schema(conn),
				 db_list_tables_schema(conn),
				 db_list_mviews_schema(conn))
	return(paste0(name, collapse = ".") %in% with(lst, paste0(table_schema, ".", table_name)))
}

get_possibly_new_srid = function(conn, proj4string, debug = FALSE) {

	srs_table = try(dbReadTable(conn, "spatial_ref_sys"))

	if (class(srs_table) == "try-error")
		return(0);

	DEBUG = function(x) { if (debug) message(x); x } # nocov
	trim <- function (x) gsub("^\\s+|\\s+$", "", x) # https://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
	srs_table$proj4text = sapply(srs_table$proj4text, trim)
	eq = srs_table$proj4text == proj4string
	if (any(eq))
		srs_table[min(which(eq)) , "srid"]
	else { # create a new srid in conn if proj4string is not found:
		srid = max(srs_table$srid) + 1
		wkt = st_as_text(st_crs(proj4string))
		query = DEBUG(paste0("INSERT INTO spatial_ref_sys (srid,srtext,proj4text) VALUES (",
			srid, ",'", wkt, "','",  proj4string, "');"))
		dbExecute(conn, query)
		srid
	}
}

get_postgis_crs = function(conn, srid, debug = FALSE) {
	DEBUG = function(x) { if (debug) message(x); x }
	if (srid > 900913) { # query postgis' own table:
		query = DEBUG(paste0("select proj4text from spatial_ref_sys where srid = ", srid, ";"))
		st_crs(dbGetQuery(conn, query)[[1]])
	} else
		st_crs(srid) # trust native epgs
}
