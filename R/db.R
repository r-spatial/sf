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
#' try(conn <- dbConnect(PostgreSQL(), dbname = "postgis"))
#' if (exists("conn") && !inherits(conn, "try-error")) {
#'   x = st_read(conn, "meuse", query = "select * from meuse limit 3;")
#'   x = st_read(conn, table = "public.meuse")
#'   print(st_crs(x)) # SRID resolved by the database, not by GDAL!
#'   dbDisconnect(conn)
#'  }
#' }
#' @name st_read
#' @details in case geom_column is missing: if table is missing, this function will try to read the name of the geometry column from table \code{geometry_columns}, in other cases, or when this fails, the geom_column is assumed to be the last column of mode character. If table is missing, the SRID cannot be read and resolved into a proj4string by the database, and a warning will be given.
st_read.PostgreSQLConnection = function(conn = NULL, table = NULL, query = NULL,
					  geom_column = NULL, EWKB, quiet = TRUE, ...) {
	if (is.null(conn))
		stop("no connection provided")

	if (!is.null(table)) {
		table <- schema_table(conn, table)
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

	if (missing(EWKB))
		EWKB = inherits(conn, "PostgreSQLConnection") || inherits(conn, "PqConnection")

	tbl[geom_column] <- lapply(tbl[geom_column], postgis_as_sfc, EWKB = EWKB, conn = conn)

	st_sf(tbl, ...)
}

postgis_as_sfc <- function(x,  EWKB, conn) {
	geom <- st_as_sfc(as_wkb(x), EWKB = EWKB)
	if (!is.null(attr(geom, "srid"))) {
		st_crs(geom) = make_crs(get_postgis_crs(conn, attr(geom, "srid")))
		attr(geom, "srid") = NULL
	}
	return(geom)
}

schema_table <- function(conn, table, public = "public") {
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

as_wkb <- function(x) {
	structure(x, class = "WKB")
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

get_postgis_crs = function(conn, srid) {
		if (is.na(srid)) return(st_crs(NA))
		query = paste0("select proj4text from spatial_ref_sys where srid = ", srid, ";")
		proj4text <- dbGetQuery(conn, query)
		if (nrow(proj4text) != 1)  return(st_crs(NA))
		return(st_crs(proj4text[[1]]))
}

set_postgis_crs = function(conn, crs, update = is.na(get_postgis_crs(conn, crs$epsg))) {
	if (update) {
		if (is.na(crs$epsg)) crs$epgs <- get_new_postgis_crs(conn)
		wkt = st_as_text(crs)
		query = paste0("INSERT INTO spatial_ref_sys (srid,srtext,proj4text) VALUES (",
					   crs$epgs, ",'", wkt, "','",  proj4string, "');")
		dbExecute(conn, query)
		return(srid)
	}
	stop("crs ", crs$epsg, " already exists in database.",
		 " Cautiously use `update = TRUE` to  force replace it.", call. = FALSE)
}

get_new_postgis_crs <- function(conn) {
	query = paste0("select srid + 1 from spatial_ref_sys order by srid desc limit 1;")
	dbGetQuery(conn, query)[[1]]
}

# for RPostgreSQL
#' @importClassesFrom RPostgreSQL PostgreSQLConnection
#' @importMethodsFrom DBI dbWriteTable
#' @export
setMethod("dbWriteTable", c("PostgreSQLConnection", "character", "sf"),
		  function(conn, name, value, ..., row.names = FALSE, overwrite = FALSE,
		  		 append = FALSE, field.types = NULL, temporary = FALSE,
		  		 copy = TRUE, factorsAsCharacter = TRUE, binary = TRUE) {
		  	if (!requireNamespace("RPostgreSQL"))
		  		stop("Missing package `RPostgreSQL`.",
		  			 " Use `install.packages(\"RPostgreSQL\")` to install.", call. = FALSE)
		  	field.types <- if (is.null(field.types)) dbDataType(conn, value)
		  	if (temporary) warning("`RPostgreSQL` does not support temporary tables")
		  	tryCatch({
		  		dbWriteTable(conn, name, to_postgis(conn, value, binary),..., row.names = row.names,
		  					 overwrite = overwrite, append = append,
		  					 field.types = field.types, temporary = temporary,
		  					 copy = copy)
		  	}, warning=function(w) {
		  		stop(conditionMessage(w), call. = FALSE)
		  	})
		  }
)

to_postgis <- function(conn, x, binary) {
	geom_col <- vapply(x, inherits, TRUE, what = "sfc")
	x[geom_col] <- lapply(x[geom_col], sync_crs, conn = conn)
	if (binary) {
		x[geom_col] <- lapply(x[geom_col], st_as_binary, "", EWKB = TRUE, hex = TRUE)
	} else {
		x[geom_col] <- lapply(x[geom_col], st_as_text, EWKT = TRUE)
	}
	x <- as.data.frame(x)
}

sync_crs <- function(conn, geom) {
	crs <- st_crs(geom)
	srid <- crs$epsg
	if (is.na(crs) || is.na(srid)) {
		if (is.na(crs$proj4string))
			crs <- make_dummy_crs(0)
		else {
			srid <- get_possibly_new_srid(conn, crs$proj4string)
			crs <- make_dummy_crs(epsg = srid, proj4string = crs$proj4string)
		}
	}
	st_set_crs(geom, crs)
}

#' Determine database type for R vector
#'
#' @export
#' @inheritParams RPostgreSQL dbDataType
#' @rdname dbDataType
#' @importClassesFrom RPostgreSQL PostgreSQLConnection
#' @importMethodsFrom DBI dbDataType
#' @param dbObj PostgreSQLConnection driver or connection.
#' @param obj Object to convert
setMethod("dbDataType", c("PostgreSQLConnection", "sf"), function(dbObj, obj) {
	dtyp <- vapply(obj, RPostgreSQL::dbDataType, character(1), dbObj =  dbObj)
	gtyp <- vapply(obj, inherits, TRUE, what = "sfc")
	dtyp[gtyp] <- "geometry"
	# explicit cast for units
	gtyp <- vapply(obj, inherits, TRUE, what = "units")
	dtyp[gtyp] <- "numeric"
	return(dtyp)
})
