#' Read PostGIS table directly, using DBI and binary conversion
#' 
#' Read PostGIS table directly through DBI and RPostgreSQL interface, converting binary
#' @param conn open database connection
#' @param table table name
#' @param query SQL query to select records
#' @param geom_column character or integer: indicator of name or position of the geometry column; if not provided, the last column of type character is chosen
#' @param EWKB logical; is the WKB is of type EWKB? defaults to TRUE if \code{conn} is of class code{PostgreSQLConnection} or \code{PqConnection}
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
        if (!db_exists(conn, table)) {
            stop("`", paste0(table, collapse = "."), "` does not exist.", call. = FALSE)
        }
        if (!is.null(query)) warning("Ignoring query argument, only using table")
        query <- paste("SELECT * FROM", paste0(table, collapse = "."), ";")
    } else if(is.null(query)) {
        stop("Provide either a table name or a query", call. = FALSE)
    }
    
    # suppress warning about unknown type "geometry":
    suppressWarnings(tbl <- dbGetQuery(conn, query))
    if (is.null(tbl)) {
        stop("`", query, "` returned no results.", call. = FALSE)
    }
    
    if("row.names" %in% colnames(tbl)){
        row.names(tbl) = tbl[["row.names"]]
        tbl = tbl[,setdiff(colnames(tbl), "row.names")]
    }
    gc = try(dbReadTable(conn, "geometry_columns"))
    
    if (is.null(geom_column)) { # try find the geometry column:
        geom_column = if (class(gc) == "try-error" | is.null(table))
            tail(which(vapply(tbl, is.character, TRUE)), 1) # guess it's the last character column
        else {
            gc[gc$f_table_schema == table[1] & gc$f_table_name == table[2], "f_geometry_column"]
        }
    }
    crs = if (class(gc) == "try-error" | is.null(table)) {
        warning("argument table missing: returning object without crs")
        NA_crs_
    } else {
        srid = gc[gc$f_table_schema == table[1] & gc$f_table_name == table[2], "srid"]
        if (srid != 0) {
            # srid 0 is used for missing in postgis
            make_crs(srid)
        } else {
            NA_crs_
        }
    }
    if (missing(EWKB))
        EWKB = inherits(conn, "PostgreSQLConnection") | inherits(conn, "PqConnection")
    tbl[[geom_column]] = st_as_sfc(structure(tbl[[geom_column]], class = "WKB"), EWKB = EWKB, crs = crs)
    st_as_sf(tbl, ...)
}

#' Write simple feature table to a spatial database
#' 
#' Write simple feature table to a spatial database
#' @param conn open database connection
#' @param table character; name for the table in the database, possibly of length 2, \code{c("schema", "name")}; default schema is \code{public}
#' @param geom_name name of the geometry column in the database
#' @param ... ignored for \code{st_write}, for \code{st_write_db} arguments passed on to \code{dbWriteTable}
#' @param overwrite logical; should \code{table} be dropped first?
#' @param append logical; append to table? (NOTE: experimental, might not work)
#' @param binary logical; use well-known-binary for transfer?
#' @param debug logical; print SQL statements to screen before executing them.
#' @name st_write
#' @export
#' @examples
#' \dontrun{
#' library(sp)
#' data(meuse)
#' sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
#' library(RPostgreSQL)
#' conn = dbConnect(PostgreSQL(), dbname = "postgis")
#' st_write_db(conn, sf, "meuse_tbl", drop_table = FALSE)}
#'   
st_write_db = function(conn = NULL, obj, table = substitute(obj), geom_name = "wkb_geometry",
                       ..., overwrite = FALSE, append = FALSE, binary = TRUE, debug = FALSE) {
    DEBUG = function(x) { if (debug) print(x); x }
    if (is.null(conn))
        stop("No connection provided")
    table <- schema_table(table)
    
    if (db_exists(conn, table)) {
        if (overwrite) {
            DBI::dbGetQuery(conn, DEBUG(paste("drop table", paste(table, collapse = "."), ";")))
        } else {
            stop("Table ", paste(table, collapse = "."), " exists already, use overwrite = TRUE", call. = FALSE)
        }
    }
    df = obj
    df[[attr(df, "sf_column")]] = NULL
    class(df) = "data.frame"
    dbWriteTable(conn, table, clean_columns(df, factorsAsCharacter = TRUE), ...)
    geom = st_geometry(obj)
    DIM = nchar(class(geom[[1]])[1]) # FIXME: is this correct? XY, XYZ, XYZM
    crs = st_crs(geom)
    SRID = crs$epsg
    if (is.null(SRID) || is.na(SRID)) {
        if (!is.na(crs)) {
            warning("Postgis does not support proj4string, the SRID is set to missing (0)")
        }
        SRID = 0
    }
    
    TYPE = class(geom[[1]])[2]
    if (! append) {
        query = DEBUG(paste0("SELECT AddGeometryColumn('", table[1],"','", table[2], "','", geom_name, 
                             "','", SRID, "','", TYPE, "',", DIM, ");"))
        dbSendQuery(conn, query)
    }
    rn = row.names(obj)
    if (! binary) {
        wkt = st_as_text(geom)
        for (r in seq_along(rn)) {
            cmd = DEBUG(paste0("UPDATE ", paste0(table, collapse = "."), " SET ", geom_name, 
                               " = ST_GeomFromText('", wkt[r], "',",SRID,") WHERE \"row.names\" = '", rn[r], "';"))
            dbGetQuery(conn, cmd)
        }
    } else {
        wkb = st_as_binary(geom, EWKB = TRUE)
        for (r in seq_along(rn)) {
            cmd = DEBUG(paste0("UPDATE ", paste0(table, collapse = "."), " SET ",
                               geom_name, " = '", CPL_raw_to_hex(wkb[[r]]), 
                               "' WHERE \"row.names\" = '", rn[r], "';"))
            dbGetQuery(conn, cmd)
        }
    }
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
