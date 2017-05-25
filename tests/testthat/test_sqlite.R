context("sf: sqlite")

db <- system.file("sqlite/nc.sqlite", package="sf")
sq <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), db)

test_that("can connect to sqlite", {
	expect_silent(nc <- st_read_db(sq, "nc.sqlite"))
})

test_that("can read from SQL query", {
	expect_silent(nc <- st_read_db(sq, query = "SELECT * FROM \"nc.sqlite\""))
})
