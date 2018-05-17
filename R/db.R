#' Read PostGIS table directly, using DBI and binary conversion
#'
#' Read PostGIS table directly through DBI and RPostgreSQL interface, converting
#' Well-Know Binary geometries to sfc
#' @param query SQL query to select records; see details
#' @param geom_column character or integer: indicator of name or position of the geometry column; if not provided, the last column of type character is chosen
#' @param EWKB logical; is the WKB is of type EWKB? if missing, defaults to \code{TRUE}
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
#' @export
st_read.DBIObject = function(dsn = NULL, layer = NULL, query = NULL,
					  geom_column = NULL, EWKB = TRUE, ...) {
	if (is.null(dsn))
		stop("no connection provided")

	if (!is.null(layer)) {
		layer <- schema_table(dsn, layer)
		if (!db_exists(dsn, layer))
			stop("`", paste0(layer, collapse = "."), "` does not exist.", call. = FALSE)
		if (!is.null(query))
			warning("Ignoring query argument, only using table")
		query <- paste("SELECT * FROM", paste0(layer, collapse = "."), ";")
	} else if(is.null(query)) {
		stop("Provide either a table name or a query", call. = FALSE)
	}

	# suppress warning about unknown type "geometry":
	suppressWarnings(tbl <- dbGetQuery(dsn, query))
	if (is.null(tbl))
		stop("`", query, "` returned no results.", call. = FALSE) # nocov

	if("row.names" %in% colnames(tbl)) {
		row.names(tbl) = tbl[["row.names"]]
		tbl = tbl[,setdiff(colnames(tbl), "row.names")]
	}
	gc = try(dbReadTable(dsn, "geometry_columns"))

	if (is.null(geom_column)) { # try find the geometry column:
		geom_column = if (class(gc) == "try-error" || is.null(layer))
			tail(which(vapply(tbl, is.character, TRUE)), 1) # guess it's the last character column
		else
			gc[gc$f_table_schema == layer[1] & gc$f_table_name == layer[2], "f_geometry_column"]
	}

	tbl[geom_column] <- lapply(tbl[geom_column], postgis_as_sfc, EWKB = EWKB, conn = dsn)

	st_sf(tbl, ...)
}

#' @export
st_read.PostgreSQLConnection <- function(...) {
    st_read.DBIObject(...)
}

postgis_as_sfc <- function(x, EWKB, conn) {
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
        srid <- srs_table[min(which(eq)) , "srid"]
    else { # create a new srid in conn if proj4string is not found:
        srid <- get_new_postgis_srid(conn)
        set_postgis_crs(conn, st_crs(srid, proj4string, valid = FALSE))
    }
    return(srid)
}

get_postgis_crs = function(conn, srid) {
    if (is.na(srid)) return(st_crs(NA))
    query = paste0("select proj4text from spatial_ref_sys where srid = ", srid, ";")
    proj4text <- dbGetQuery(conn, query)
    if (nrow(proj4text) != 1)  return(st_crs(NA))
    crs <-  st_crs(srid, gsub("^\\s+|\\s+$", "", proj4text[[1]]), valid = FALSE)
    local_crs <- st_crs(srid)
    if(crs != local_crs & !is.na(local_crs))
        warning("Local crs different from database crs. You can inspect the ",
                "database crs using `dbReadtable(conn, \"spatial_ref_sys\")` ",
                "and compare it to `st_crs(", srid,")`.")
    return(crs)
}

set_postgis_crs = function(conn, crs, auth_name = "sf", update = FALSE, verbose = TRUE) {
    if (is.na(crs[["epsg"]])) {
        crs[["epsg"]] <- get_new_postgis_srid(conn)
    } else {
        get_postgis_crs(conn, crs[["epsg"]])
    }
    wkt <- st_as_text(crs)
    q <- function(x) paste0("'", x, "'")
    if (update) {
        query <- paste("UPDATE spatial_ref_sys",
                       "auth_name =", q(auth_name),
                       ", srtext =", q(wkt),
                      ", proj4text =", q(crs[["proj4string"]]),
                      "WHERE srid =", q(crs[["epsg"]]), ";")
    } else {
        query <- paste("INSERT INTO spatial_ref_sys (srid,auth_name,auth_srid,srtext,proj4text)",
                      "VALUES (",
                      paste(crs[["epsg"]], q(auth_name), crs[["epsg"]], q(wkt), q(crs[["proj4string"]]), sep = ", "),
                      ");")
    }
    tryCatch(dbExecute(conn, query),
            error = function(err) {
                if(grepl("permission denied", err))
                    stop("Write permission denied on table `spatial_ref_sys`.",
                        "\n * Local crs is not in the database;",
                        "\n * Write permission on table `spatial_ref_sys` is denied.",
                        "\nEither: ",
                        "\n * Change the crs locally using `st_transform()`;",
                        "\n * Grant write access on `spatial_sys_ref` for this connection.",
                        "\nLocal crs is:`", crs[["proj4string"]], "` (SRID:", crs[["epsg"]], ")")
                stop(err)
            })
    if (verbose) message("Inserted local crs: `", crs[["proj4string"]],
                         "` in database as srid:", crs[["epsg"]], ".")
    return(crs)
}

delete_postgis_crs = function(conn, crs) {
    if (is.na(crs[["epsg"]])) stop("missing crs")
    wkt <- st_as_text(crs)
    query <- paste0("DELETE FROM spatial_ref_sys ",
                   "WHERE srid = '", crs[["epsg"]], "' ",
                   "AND srtext = '", wkt, "' ",
                   "AND proj4text = '", crs[["proj4string"]], "';")
    dbExecute(conn, query)
}

get_new_postgis_srid <- function(conn) {
	query = paste0("select srid + 1 from spatial_ref_sys order by srid desc limit 1;")
	dbGetQuery(conn, query)[[1]]
}

# for RPostgreSQL

#' Write `sf` object to Database
#' @inheritParams RPostgreSQL::postgresqlWriteTable
#' @md
#' @rdname st_write
#' @importMethodsFrom DBI dbWriteTable
#' @export
setMethod("dbWriteTable", c("PostgreSQLConnection", "character", "sf"),
          function(conn, name, value, ..., row.names = FALSE, overwrite = FALSE,
                   append = FALSE, field.types = NULL, factorsAsCharacter = TRUE, binary = TRUE) {
              field.types <- if (is.null(field.types)) dbDataType(conn, value)
              tryCatch({
                  dbWriteTable(conn, name, to_postgis(conn, value, binary),..., row.names = row.names,
                               overwrite = overwrite, append = append,
                               field.types = field.types)
              }, warning=function(w) {
                  stop(conditionMessage(w), call. = FALSE)
              })
          }
)

#' Write `sf` object to Database
#' @inheritParams DBI::dbWriteTable
#' @param conn DBIObject
#' @param binary Send geometries serialized as Well-Known Binary (WKB);
#' if `FALSE`, uses Well-Known Text (WKT). Defaults to `TRUE` (WKB).
#' @param row.names Add a `row.name` column, or a vector of length `nrow(obj)`
#' containing row.names; default `FALSE`.
#' @param overwrite Will try to `drop` table before writing; default `FALSE`.
#' @param append Append rows to existing table; default `FALSE`.
#' @param field.types default `NULL`. Allows to override type conversion from R
#' to PostgreSQL. See `dbDataType()` for details.
#' @md
#' @rdname st_write
#' @importMethodsFrom DBI dbWriteTable
#' @export
setMethod("dbWriteTable", c("DBIObject", "character", "sf"),
          function(conn, name, value, ..., row.names = FALSE, overwrite = FALSE,
                   append = FALSE, field.types = NULL, factorsAsCharacter = TRUE, binary = TRUE) {
              field.types <- if (is.null(field.types)) dbDataType(conn, value)
              # DBI cannot set field types with append
              if (append) field.types <- NULL
              tryCatch({
                  dbWriteTable(conn, name, to_postgis(conn, value, binary),..., row.names = row.names,
                               overwrite = overwrite, append = append,
                               field.types = field.types)
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
            crs <- st_crs(0, valid = FALSE)
        else {
            srid <- get_possibly_new_srid(conn, crs$proj4string)
            crs <- st_crs(srid, proj4text = crs$proj4string, valid = FALSE)
        }
    }
    st_set_crs(geom, crs)
}

#' Determine database type for R vector
#'
#' @export
#' @inheritParams RPostgreSQL dbDataType
#' @rdname dbDataType
#' @importMethodsFrom DBI dbDataType
setMethod("dbDataType", c("PostgreSQLConnection", "sf"), function(dbObj, obj) {
	dtyp <- vapply(obj, RPostgreSQL::dbDataType, character(1), dbObj =  dbObj)
	gtyp <- vapply(obj, inherits, TRUE, what = "sfc")
	dtyp[gtyp] <- "geometry"
	# explicit cast for units
	gtyp <- vapply(obj, inherits, TRUE, what = "units")
	dtyp[gtyp] <- "numeric"
	return(dtyp)
})

#' Determine database type for R vector
#'
#' @export
#' @inheritParams DBI dbDataType
#' @rdname dbDataType
#' @importClassesFrom DBI DBIObject
#' @importMethodsFrom DBI dbDataType
#' @param dbObj DBIObject driver or connection.
#' @param obj Object to convert
setMethod("dbDataType", c("DBIObject", "sf"), function(dbObj, obj) {
    dtyp <- vapply(obj, DBI::dbDataType, character(1), dbObj =  dbObj)
    gtyp <- vapply(obj, inherits, TRUE, what = "sfc")
    dtyp[gtyp] <- "geometry"
    # explicit cast for units
    gtyp <- vapply(obj, inherits, TRUE, what = "units")
    dtyp[gtyp] <- "numeric"
    return(dtyp)
})
