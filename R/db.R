#' Read PostGIS table directly, using DBI and binary conversion
#'
#' Read PostGIS table directly through DBI and RPostgreSQL interface, converting
#' Well-Know Binary geometries to sfc
#' @param query SQL query to select records; see details
#' @param EWKB logical; is the WKB of type EWKB? if missing, defaults to
#'   \code{TRUE}
#' @param as_tibble logical; should the returned table be of class tibble or data.frame?
#' @details if \code{table} is not given but \code{query} is, the spatial
#'   reference system (crs) of the table queried is only available in case it
#'   has been stored into each geometry record (e.g., by PostGIS, when using
#'   EWKB)
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
#' @details The function will automatically find the `geometry` type columns for
#'   drivers that support it. For the other drivers, it will try to cast all the
#'   character columns, which can be slow for very wide tables.
#' @export
st_read.DBIObject = function(dsn = NULL,
                             layer = NULL,
                             query = NULL,
                             EWKB = TRUE,
                             quiet = TRUE,
                             as_tibble = FALSE,
                             ...) {
    if (is.null(dsn))
        stop("no connection provided") # nocov

    if (as_tibble && !requireNamespace("tibble", quietly = TRUE)) {
        stop("package tibble not available: install first?") # nocov
    }

    # check that ellipsis contains only what is needed
    expe <- setdiff(names(list(...)), names(formals(st_sf)))
    if(length(expe) > 0) {
        # error,  these arguments would be passed to st_sf
        suggest <- NULL
        if("table" %in% expe){
            suggest <- c(suggest, "\nMaybe you should use `layer` rather than `table` ?")
        }
        pref <- if(length(expe) > 1) "\t *" else  ""
        stop(
            "Unused arguments: ",
            if(length(expe) > 1) "\n" else "",
            paste(pref, expe, "=", list(...)[expe], collapse = "\n", sep = " "),
            suggest,
            "\nCheck arguments for `st_sf()` for details.",
            call. = FALSE
        )
    }

    # filter expected warnings (for RPostgreSQL driver)
    filter_warning <- function(expr, regexp) {
        wlist <- NULL
        warning_handler <- function(w) {
            wlist <<- c(wlist, list(w))
            invokeRestart("muffleWarning")
        }
        msg <- function(x) x$message
        out <- withCallingHandlers(expr, warning = warning_handler)
        if(!all(grepl(regexp, wlist))) {
            lapply(vapply(wlist, msg, character(1)), warning, call. = FALSE)  # nocov
        }
        return(out)
    }

    # Check layer and query conflict
    if (!is.null(layer)) {
        if (!is.null(query)) {
            warning("You provided both `layer` and `query` arguments,",
                    " will only use `layer`.", call. = FALSE)
        }
        # capture warnings from RPostgreSQL package
        if (inherits(dsn, "PostgreSQLConnection")) {
            tbl <- filter_warning(dbReadTable(dsn, layer), "unrecognized PostgreSQL field type geometry")
        } else {
            tbl <- dbReadTable(dsn, layer)
        }
    } else if(is.null(query)) {
        stop("Provide either a `layer` or a `query`", call. = FALSE)
    } else {
        # capture warnings from RPostgreSQL package
        if (inherits(dsn, "PostgreSQLConnection")) {
            filter_warning(tbl <- dbGetQuery(dsn, query), "unrecognized PostgreSQL field type geometry")
        } else {
            tbl <- dbGetQuery(dsn, query)
        }
    }

    if (is.null(tbl)) {
        stop("Query `", query, "` returned no results.", call. = FALSE)  #nocov
    }

    # check for simple features column
    geometry_column = is_geometry_column(dsn, tbl)

    tbl[geometry_column] <- lapply(tbl[geometry_column], try_postgis_as_sfc, EWKB = EWKB, conn = dsn)

    # if there are no simple features geometries, return a data frame
    if (! any(vapply(tbl, inherits, logical(1), "sfc"))) {
		# try reading blob columns:
    	blob_columns = vapply(tbl, inherits, logical(1), "blob")
		success = FALSE
		for (i in which(blob_columns)) {
			try(sfc <- st_as_sfc(tbl[[i]]), silent = TRUE)
			if (!inherits(sfc, "try-error")) {
				tbl[[i]] = sfc
				success = TRUE
			}
		}
    	if (! success) {
        	warning("Could not find a simple features geometry column. Will return a `data.frame`.")
        	return(tbl)
		}
    }

    x <- st_sf(tbl, ...)

    if (!quiet) print(x, n = 0) # nocov

    if (as_tibble) {
        x <- tibble::as_tibble(x)
    }
    return(x)
}

#' @export
st_read.Pool = function(dsn = NULL, layer = NULL, ...) {
	if (! requireNamespace("pool", quietly = TRUE)) # nocov start
		stop("package pool required, please install it first")
	dsn = pool::poolCheckout(dsn)
	on.exit(pool::poolReturn(dsn))
	st_read(dsn, layer = layer, ...)                # nocov end
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

try_postgis_as_sfc <- function(x, EWKB, conn) {
    tryCatch(postgis_as_sfc(x, EWKB, conn), error = function(...) return(x))
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

get_possibly_new_srid <- function(conn, proj4string) {

    srs_table = try(dbReadTable(conn, "spatial_ref_sys"))

    if (class(srs_table) == "try-error") {  # nocov start
        warning("Could not find table `spatial_ref_sys` on remote connexion; ",
                "CRS is set to unknown.")
        return(0)
    } # nocov end

    trim <- function (x) gsub("^\\s+|\\s+$", "", x) # https://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
    srs_table$proj4text = sapply(srs_table$proj4text, trim)
    eq = srs_table$proj4text == proj4string
    if (any(eq))
        srid <- srs_table[min(which(eq)) , "srid"]
    else { # create a new srid in conn if proj4string is not found:
        srid <- get_new_postgis_srid(conn)
        set_postgis_crs(conn, st_crs(srid, proj4string, valid = FALSE))
    }
    srid
}

get_postgis_crs = function(conn, srid) {
    if (is.na(srid)) return(st_crs(NA))
    query <- paste0("select proj4text from spatial_ref_sys where srid = ", srid, ";")
    proj4text <- dbGetQuery(conn, query)
    if (nrow(proj4text) != 1) return(st_crs(NA))
    crs <- st_crs(srid, gsub("^\\s+|\\s+$", "", proj4text[[1]]), valid = FALSE)
    local_crs <- st_crs(srid)
    if(crs != local_crs & !is.na(local_crs)) {  # nocov start
        warning("Local crs different from database crs. You can inspect the ",
                "database crs using `dbReadtable(conn, \"spatial_ref_sys\")` ",
                "and compare it to `st_crs(", srid,")`.")
    }  # nocov end
    crs
}

set_postgis_crs <- function(conn,
                            crs,
                            auth_name = "sf",
                            update = FALSE,
                            verbose = TRUE) {
    if (is.na(crs[["epsg"]])) {
        crs[["epsg"]] <- get_new_postgis_srid(conn)
    } else {
        get_postgis_crs(conn, crs[["epsg"]])
    }
    wkt <- st_as_text(crs)
    q <- function(x) paste0("'", x, "'")
    if (update) {
        query <- paste("UPDATE spatial_ref_sys SET",
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
                if(grepl("permission denied", err)) {  # nocov start
                    stop("Write permission denied on table `spatial_ref_sys`.",
                         "\n * Local crs is not in the database;",
                         "\n * Write permission on table `spatial_ref_sys` is denied.",
                         "\nEither: ",
                         "\n * Change the crs locally using `st_transform()`;",
                         "\n * Grant write access on `spatial_sys_ref` for this connection.",
                         "\nLocal crs is:`", crs[["proj4string"]], "` (SRID:", crs[["epsg"]], ")")
                }
                stop(err) # nocov end
            })
    if (verbose) message("Inserted local crs: `", crs[["proj4string"]],
                         "` in database as srid:", crs[["epsg"]], ".")
    return(crs)
}

delete_postgis_crs <- function(conn, crs) {
    if (is.na(crs[["epsg"]])) stop("Missing SRID")
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
              if (is.null(field.types)) field.types <- dbDataType(conn, value)
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
          	if (is.null(field.types)) field.types <- dbDataType(conn, value)
              # DBI cannot set field types with append
              if (append) field.types <- NULL
              tryCatch({
                  dbWriteTable(conn, name, to_postgis(conn, value, binary),..., row.names = row.names,
                               overwrite = overwrite, append = append,
                               field.types = field.types)
              }, warning=function(w) {
                  stop(conditionMessage(w), call. = FALSE)  # nocov
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
	clean_columns(x, factorsAsCharacter = TRUE)
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

#' Check if the columns could be of a coercable type for sf
#'
#' @param con database connection
#' @param x inherits data.frame
#' @param classes classes inherited
is_geometry_column <- function(con, x, classes = "") UseMethod("is_geometry_column")

is_geometry_column.PqConnection <- function(con, x, classes = c("pq_geometry")) {
    vapply(x, inherits, logical(1), classes)
}

is_geometry_column.default <- function(con, x, classes = c("character")) {
    # try all character columns (in conjunction with try_postgis_as_sfc)
    vapply(x, function(x) inherits(x, classes) && !all(is.na(x)),
    	   FUN.VALUE = logical(1))
}
