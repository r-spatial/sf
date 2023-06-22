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
                             geometry_column = NULL,
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

    if (is.null(geometry_column)) {
        # scan table for simple features column
        geometry_column = is_geometry_column(dsn, tbl)
        tbl[geometry_column] <- lapply(tbl[geometry_column], try_postgis_as_sfc, EWKB = EWKB, conn = dsn)
    } else {
        if (!all(geometry_column %in% names(tbl))) {
            # prepare error message
            nm <- names(tbl)
            prefix <- ""
            new_line <- ""
            if(length(nm) > 1) {
                prefix <- "  *"
                new_line <- "\n"
            }
            stop("Could not find `geometry_column` (\"", paste(geometry_column, collapse = "\", \""), "\") ",
                "in column names. Available names are:",
                new_line,
                paste(prefix, nm, collapse = "\n", sep = " "),
                call. = FALSE)
        }
        tbl[geometry_column] <- lapply(tbl[geometry_column], postgis_as_sfc, EWKB = EWKB, conn = dsn)
    }

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
        x <- tibble::new_tibble(x, nrow = nrow(x), class = "sf")
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
	geom <- st_as_sfc(as_wkb_(x), EWKB = EWKB)
	srid <- attr(geom, "srid")
	if (!is.null(srid)) {
		st_crs(geom) = db_find_srid(conn, srid = srid, validate = FALSE)
		attr(geom, "srid") = NULL
		warning("Could not find database srid (", srid, ") locally; using the remote database definition.")
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

	if (anyNA(table))
		stop("table and schema cannot be NA", call. = FALSE)

	return(table)
}

as_wkb_ <- function(x) {
	structure(x, class = "WKB")
}

get_possibly_new_srid <- function(conn, crs) {

	db_crs <- db_find_srid(conn, crs)
	if(!is.na(db_crs)) {
		return(db_crs)
	}

	db_crs <- db_find_srtext(conn, crs)
	if (!is.na(db_crs)) {
		return(db_crs)
	}

	db_insert_crs(conn, crs)
}

# Find srid in a database by using the srid
# @param conn Dababase connection (e.g. `DBI`)
# @param srid An integer descriing the srid to fetch
# @param validate if TRUE, then the crs_local is used to validate the remote crs.
#   Use validate = FALSE when searching for an srid unavailable locally, or when
#   the wkt is unknown locally.
# @returns a `crs`
db_find_srid = function(conn, crs_local = st_crs(srid), srid = epsg(crs_local), validate = TRUE) {
    if (validate && is.na(crs_local)) return(st_crs(NA))
	if (is.na(srid)) {
		return(st_crs(NA))
	}

    query <- paste0("select srtext from spatial_ref_sys where srid = ", srid)
    db_crs <- dbGetQuery(conn, query)
    if (nrow(db_crs) < 1) {
    	return(st_crs(NA))
    }
    if (nrow(db_crs) > 1) {
    	# TODO: pretty print db_spatial_ref
    	stop("SRID should be unique, but the database returned ", nrow(db_crs), " matching crs. \n",
    		 db_crs, call. = FALSE)  # nocov
    }
    crs_found <- st_crs(db_crs[["srtext"]])
    crs_found[["input"]] <- build_epsg(srid)
    if(validate && crs_found != crs_local && !is.na(crs_local)) {
    	# TODO: pretty print db_spatial_ref
        warning("Local crs different from database crs. You can inspect the ",
                "database crs using `dbReadtable(conn, \"spatial_ref_sys\")` ",
                "and compare it to `st_crs(", srid,")`.")  # nocov
    }
    crs_found
}

# Find database projection using srtext (wkt)
db_find_srtext = function(conn, crs_local = st_crs(wkt), wkt = st_as_text(crs_local)) {
	if (is.na(crs_local)) return(st_crs(NA))
	if (is.na(wkt)) {
		return(st_crs(NA))
	}

	query <- paste0("select * from spatial_ref_sys where srtext = '", wkt, "'")
	db_spatial_ref <- DBI::dbGetQuery(conn, query)
	if (nrow(db_spatial_ref) < 1) {
		# need to relax comparison

		# read table, and find equivalent projections using ==
		query <- "select * from spatial_ref_sys where srtext is not null and srtext != ''"
		db_spatial_ref <- DBI::dbGetQuery(conn, query)
		db_crs <- lapply(db_spatial_ref[["srtext"]], function(string) try(st_crs(string)))
		reject <- vapply(db_crs, function(x) inherits(x, "try-error"), logical(1))
		eq <- vapply(db_crs[!reject], function(x) crs_local == x, logical(1))
		db_spatial_ref <- db_spatial_ref[eq, ]
	}

	if (nrow(db_spatial_ref) > 1) {  # nocov start
		# Use the first match, but warn the user.
		# Only show first 10 matches
		db_spatial_ref <- db_spatial_ref[seq_len(min(nrow(db_spatial_ref), 10)), ]
		# TODO: make it a warning -- check classes so they can be grabbed in dbWriteTable.DBI
		message("Found multiple matching projections, will use srid = ",
				db_spatial_ref[["srid"]][[1]],
				".\nOther database srid matching the projection WKT description: ",
				paste(db_spatial_ref[["srid"]][-1], collapse = ", "), "\n",
				"You can suppress this warning by setting the projection to `st_crs(",
				db_spatial_ref[["srid"]][[1]], ")`.")
		db_spatial_ref <- db_spatial_ref[1, ]
	} # nocov end

	if (nrow(db_spatial_ref) < 1) {
		return(st_crs(NA))
	} else {
		crs_found <- make_empty_crs(db_spatial_ref[["srid"]], db_spatial_ref[["srtext"]])
	}

	if(crs_found != crs_local) {  # nocov start
		warning("Local crs different from database crs. You can inspect the ",
				"database crs using `dbReadtable(conn, \"spatial_ref_sys\")` ",
				"and compare it to `st_crs(\"", wkt,"\")`.")
	}  # nocov end
	crs_found
}

make_empty_crs <- function(epsg = NA, text = NA, wkt = NA) {
	if(!is.na(epsg)) {
		epsg <- build_epsg(epsg)[1]
	}
	if(is.na(wkt)) {
		wkt = st_as_text(st_crs(text))
	}
	structure(
		list(
			input = epsg,
			wkt = wkt),
		class = "crs")
}

build_epsg <- function(auth_srid, auth_name = "EPSG") {
	paste0(auth_name, ":", auth_srid)
}

db_insert_crs <- function(conn,
						  crs,
						  srid = epsg(crs),
						  auth_name = "sf",
						  auth_srid = srid,
						  wkt = st_as_text(crs),
						  proj4text = proj4string(crs),
						  update = FALSE,
						  verbose = TRUE) {
	# fail fast ----------------------------------------------
	# We also try to provide all error messages at once
	error_msg <- NULL
	if (update) {
		if (is.na(srid)) {
			error_msg <- c(error_msg, paste0(
			   	"You need to provide an `srid` to update a projection,  but the `srid` is NA.",
			   	"\n  Either: \n  * provide an `srid` or \n  * use `update = FALSE` to receive an srid",
			   	collapse = ""
			   ))
		}
	}
	if (is.na(wkt)) {
		error_msg <- c(error_msg,
					   "You need to provide a `wkt` to update the database `spatial_ref_sys`.")
	}
	if (!is.null(error_msg)) {
		n_errors <- length(error_msg)
		if (n_errors > 1) {
			error_msg <- c(paste0("We found ", n_errors, " errors:\n"), error_msg)
		}
		stop(paste(error_msg, collapse = "\n"), call. = FALSE)
	}
	# end tests ---------------------------------------

	if (is.na(srid)) {
        srid <- get_new_postgis_srid(conn)
	}
	if (is.na(auth_srid)) {
        auth_srid <- auth_srid
	}
	crs <- make_empty_crs(epsg = srid, text = wkt)

	q <- function(x) paste0("'", x, "'")
	if (update) {
		query <- paste("UPDATE spatial_ref_sys SET",
					   "auth_name =", q(auth_name), ", ",
					   "auth_srid =", auth_srid, ", ",
					   "srtext =", q(wkt), ", ",
					   "proj4text =", q(proj4string(crs)),
					   "WHERE srid =", srid, ";")
	} else {
		query <- paste("INSERT INTO spatial_ref_sys (srid, auth_name, auth_srid, srtext, proj4text)",
                      "VALUES (",
                      paste(
                      	srid,
                      	q(auth_name),
                      	auth_srid,
                      	q(wkt),
                      	q(proj4string(crs)), sep = ", "),
                      ");")
    }
    tryCatch(dbExecute(conn, query),
            error = function(err) {
                if(grepl("permission denied", err)) {  # nocov start
                    stop("Write permission denied on table `spatial_ref_sys` because:",
                         "\n * Local crs is not in the database; ",
                         "\n * Write permission on table `spatial_ref_sys` is denied.",
                         "\nEither: ",
                         "\n * Change the crs locally using `st_transform()` on your `sf` object;",
                         "\n * Set the crs to NA using `st_set_crs({your_sf}, NA)`.",
                         "\n * Grant write access on `spatial_sys_ref`.",
                         "\n * Ask the database administrator to add your projection with :",
                    	 "\n ``` sql\n", query, "\n ```",
                    	 call. = FALSE)
                }
                stop(err) # nocov end
            })
    if (verbose) {
    	message("Inserted local crs: `", wkt, "` in database as srid:", srid, ".")
    }
    return(crs)
}

db_check_user_permission <- function(conn, table, permission, strict = FALSE) {
	q <- paste0("select has_table_privilege('", table, "', '", permission, "') as has")
	can <- try(dbReadTable(conn, q)[["has"]])
	if (inherits(can, "try-error")){
		if (strict) {
			return(FALSE)
		}
		# we don't know if the user has the permission, but we'll let it pass since
		# the check isn't strict that way we can see what happens when the permission
		# is actually needed
		return(TRUE)
	}
	return(can)
}

delete_postgis_crs <- function(conn, crs) {
    if (is.na(epsg(crs))) stop("Missing SRID")
    wkt <- st_as_text(crs)
    query <- paste0("DELETE FROM spatial_ref_sys ",
                   "WHERE srid = '", epsg(crs), "' ",
                   "AND srtext = '", wkt, "' ",
                   "AND proj4text = '", proj4string(crs), "';")
    dbExecute(conn, query)
}

get_new_postgis_srid <- function(conn) {
    query = paste0("select srid + 1 as srid from spatial_ref_sys order by srid desc limit 1;")
    dbGetQuery(conn, query)[["srid"]]
}

# for RPostgreSQL

#' Write `sf` object to Database
#' @inheritParams RPostgreSQL::postgresqlWriteTable
#' @md
#' @rdname dbWriteTable
#' @importMethodsFrom DBI dbWriteTable
#' @export
setMethod("dbWriteTable", c("PostgreSQLConnection", "character", "sf"),
          function(conn, name, value, ..., row.names = FALSE, overwrite = FALSE,
                   append = FALSE, field.types = NULL, binary = TRUE) {
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
#' @rdname dbWriteTable
#' @importMethodsFrom DBI dbWriteTable
#' @export
setMethod("dbWriteTable", c("DBIObject", "character", "sf"),
          function(conn, name, value, ..., row.names = FALSE, overwrite = FALSE,
                   append = FALSE, field.types = NULL, binary = TRUE) {
          	if (is.null(field.types)) field.types <- dbDataType(conn, value)
              # DBI cannot set field types with append
              if (append) field.types <- NULL
              #tryCatch({
                  dbWriteTable(conn, name, to_postgis(conn, value, binary),..., row.names = row.names,
                               overwrite = overwrite, append = append,
                               field.types = field.types)
              # }, warning=function(w) {
              #     stop(conditionMessage(w), call. = FALSE)  # nocov
              # })
          }
)

to_postgis <- function(conn, x, binary) {
	geom_col <- vapply(x, inherits, TRUE, what = "sfc")
	x[geom_col] <- lapply(x[geom_col], sync_crs, conn = conn)
	if (binary) {
		x[geom_col] <- lapply(x[geom_col], db_binary)
	} else {
		x[geom_col] <- lapply(x[geom_col], st_as_text, EWKT = TRUE)
	}
	x <- as.data.frame(x)
	clean_columns(x, factorsAsCharacter = TRUE)
}

# Version of st_as_binary that allows locally invalid srids
db_binary <- function(x) {
	st_as_binary(x, EWKB = TRUE, hex = TRUE, pureR = FALSE, srid = epsg(st_crs(x)))
}

sync_crs <- function(conn, geom) {
    crs <- st_crs(geom)
    srid <- epsg(crs)
    if (is.na(crs) || is.na(srid)) {
        if (is.na(st_as_text(crs)))
            crs <- st_crs(NA)
        else {
            crs <- get_possibly_new_srid(conn, crs)
        }
    }
    st_set_crs(geom, crs)
}

#' Determine database type for R vector
#'
#' @export
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

# https://github.com/r-spatial/sf/issues/1195 :
# RPostgres::dbGetQuery returns geometry columns of class pq_geometry:
#' @name st_as_sfc
#' @export
st_as_sfc.pq_geometry <- function(x, ..., EWKB = TRUE, spatialite = FALSE,
		pureR = FALSE, crs = NA_crs_) { # nocov start
  st_as_sfc.WKB(x, ..., EWKB = EWKB, spatiallite = spatialite, pureR = pureR, crs = crs)
} # nocov end
