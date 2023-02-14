suppressMessages(require(dplyr, quietly = TRUE))
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

test_that("select works", {
  skip_if_not_installed("dplyr")
  expect_true(nc %>% select_("AREA", attr(., "sf_column"))  %>% inherits("sf"))
  expect_true(nc %>% select(AREA) %>% inherits("sf"))
})

test_that("filter to sfc works", {
  skip_if_not_installed("dplyr")
  tbl = tibble(a = c("A", "B", "C"),
              geometry = st_sfc(st_point(c(1, 1)),
                                st_point(),
                                st_linestring()))
  d = st_sf(tbl)
  expect_identical(d %>% filter(!st_is_empty(geometry)) %>% st_cast(),
                   d[1, ])
  expect_identical(d %>% filter(st_is(geometry, "POINT")) %>% st_cast(),
                   d[1:2, ])
})

suppressMessages(require(tidyr, quietly = TRUE))
test_that("separate and unite work", {
  skip_if_not_installed("dplyr")
  expect_true(nc %>% separate(CNTY_ID, c("a", "b"), sep = 2) %>% inherits("sf"))
  expect_true(nc %>% separate(CNTY_ID, c("a", "b"), sep = 2) %>%
	unite(CNTY_ID_NEW, c("a", "b"), sep = "") %>% inherits("sf"))
})

test_that("separate_rows work", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  d <- st_as_sf(data.frame(
    x = seq_len(3),
    y = c("a", "d,e,f", "g,h"),
    geometry = st_sfc(st_point(c(1, 1)),
                      st_point(c(2, 2)),
                      st_point(c(3, 3))),
    stringsAsFactors = FALSE))
  expect_true(d %>%
                separate_rows(y, convert = TRUE) %>%
                inherits("sf"))
  expect_identical(d %>%
      separate_rows(y, convert = TRUE) %>%
      st_geometry(),
    st_sfc(st_point(c(1, 1)),
           st_point(c(2, 2)),
           st_point(c(2, 2)),
           st_point(c(2, 2)),
           st_point(c(3, 3)),
           st_point(c(3, 3))))
})

test_that("group/ungroup works", {
 skip_if_not_installed("dplyr")
 tbl = tibble(a = c(1,1,2,2), g = st_sfc(st_point(0:1), st_point(1:2), st_point(2:3), st_point(3:4)))
 d = st_sf(tbl)
 e <- d %>% group_by(a) %>% ungroup
 expect_equal(as.data.frame(d), as.data.frame(e))
})

test_that("sample_n etc work", {
  skip_if_not_installed("dplyr")
  tbl = tibble(a = c(1,1,2,2), g = st_sfc(st_point(0:1), st_point(1:2), st_point(2:3), st_point(3:4)))
  d = st_sf(tbl)

  expect_sampled <- function(x) {
    expect_true(inherits(x, c("sf", "tbl_df")))
    expect_named(x, c("a", "g"))
    expect_equal(nrow(x), 2)
    expect_true(inherits(x$g, "sfc_POINT"))
  }

  expect_sampled(sample_n(d, 2))
  expect_sampled(sample_frac(d, .5))
})

test_that("nest() works", {
	skip_if_not_installed("dplyr")
	skip_if_not_installed("tidyr")
	tbl = tibble(a = c(1,1,2,2), g = st_sfc(st_point(0:1), st_point(1:2), st_point(2:3), st_point(3:4)))
	d = st_sf(tbl)
	out = d %>% group_by(a) %>% nest()

	exp_data = list(d[1:2, "g"], d[3:4, "g"])

	# Work around issues of tibble comparison in dplyr 0.8.5 (faulty
	# all.equal.tbl_df() method)
	if (utils::packageVersion("dplyr") < "0.8.99") {
		dfs = lapply(out$data, function(x) st_sf(as.data.frame(x)))
		exp_data = lapply(exp_data, function(x) st_sf(as.data.frame(x)))
		expect_identical(dfs, exp_data)
	} else {
		exp = tibble(a = c(1, 2), data = exp_data) %>% group_by(a)
		expect_identical(out, exp)
	}
})

test_that("st_intersection of tbl returns tbl", {
 nc = read_sf(system.file("shape/nc.shp", package="sf"))
 nc = st_transform(nc[1:3,], 3857)
 st_agr(nc) = "constant"
 expect_s3_class(nc, "tbl_df")
 expect_s3_class(st_intersection(nc[1:3], nc[4:6]), "tbl_df")
})

test_that("unnest works", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not(utils::packageVersion("tidyr") > "0.7.2")
  nc = read_sf(system.file("shape/nc.shp", package = "sf")) %>%
    slice(1:2) %>%
    transmute(y = list(c("a"), c("b", "c")))
  unnest_explicit = unnest(nc, y)
  # The second row is duplicated because the "b" and "c" become separate rows
  expected = nc[c(1,2,2), ] %>% mutate(y = c("a", "b", "c"))
  # Would use expect_equal, but doesn't work with geometry cols
  expect_identical(unnest_explicit, expected)
})

test_that("bind_rows() returns type of first input", {
	skip_if_not_installed("dplyr", "0.8.99")
	skip_if_not_installed("vctrs", "0.3.0.9000")

	sf1 = st_sf(x = 1, y = st_sfc(st_point(0:1)))
	sf2 = st_sf(z = st_sfc(st_point(2:3)), x = 2)

	# Avoid as.data.frame.sfc() method
	data_frame = function(...) {
		df = tibble(...)
		class(df) = "data.frame"
		df
	}

	# Output is a data frame if first input is a data frame
	out = bind_rows(data.frame(x = 1), sf2)
	exp = data_frame(
		x = c(1, 2),
		z = st_sfc(NA, st_point(2:3))
	)
	expect_identical(out, exp)

	out = bind_rows(sf1, data.frame(x = 1))
	exp = st_as_sf(data_frame(
		x = c(1, 1),
		y = st_sfc(st_point(0:1), NA)
	))
	expect_identical(out, exp)

	out = bind_rows(sf1, sf2)
	exp = st_as_sf(data_frame(
		x = c(1, 2),
		y = st_sfc(st_point(0:1), NA),
		z = st_sfc(NA, st_point(2:3))
	))
	expect_identical(out, exp)

	out = bind_rows(sf2, sf1)
	exp = st_as_sf(sf_column_name = "z", data_frame(
		x = c(2, 1),
		z = st_sfc(st_point(2:3), NA),
		y = st_sfc(NA, st_point(0:1))
	))
	expect_identical(out, exp)
})

test_that("bind_cols() returns type of first input", {
	skip_if_not_installed("dplyr", "0.8.99")

	sf1 = st_sf(x = 1, y = st_sfc(st_point(0:1)))
	sf2 = st_sf(z = st_sfc(st_point(2:3)), w = 2)

	# Avoid as.data.frame.sfc() method
	data_frame = function(...) {
		df = tibble(...)
		class(df) = "data.frame"
		df
	}

	# Output is a data frame if first input is a data frame
	out = bind_cols(data.frame(x = 1), sf2)
	exp = data_frame(
		x = 1,
		w = 2,
		z = st_sfc(st_point(2:3))
	)
	expect_identical(out, exp)

	out = bind_cols(sf1, data.frame(w = 2))
	exp = st_as_sf(data_frame(
		x = 1,
		w = 2,
		y = st_sfc(st_point(0:1))
	))
	expect_identical(out, exp)

	out = bind_cols(sf1, sf2)
	exp = st_as_sf(data_frame(
		x = 1,
		w = 2,
		y = st_sfc(st_point(0:1)),
		z = st_sfc(st_point(2:3))
	))
	expect_identical(out, exp)

	out = bind_cols(sf2, sf1)
	exp = st_as_sf(data_frame(
		w = 2,
		x = 1,
		z = st_sfc(st_point(2:3)),
		y = st_sfc(st_point(0:1))
	))
	expect_identical(out, exp)
})

test_that("can rename geometry column with `select()`", {
    skip_if_not_installed("dplyr")
	sf = st_sf(
		x = 1,
		geo = st_sfc(st_point(1:2)),
		y = "foo"
	)
	out = dplyr::select(sf, foo = geo)
	expect_identical(out, st_sf(foo = sf$geo))

	# geometry column is sticky
	out = dplyr::select(sf, y)
	expect_identical(out, st_sf(geo = sf$geo, y = sf$y))
})

test_that("can rename geometry column with `rename()` (#1431)", {
  skip_if_not_installed("dplyr")
  geo_pt = st_sfc(st_point())
  geo_ln = st_sfc(st_linestring())
  sf = st_sf(x = 1, geo2 = geo_pt, geo1 = geo_ln, sf_column_name = "geo1")

  expect_identical(
    dplyr::rename(sf, y = x),
    st_sf(y = 1, geo2 = geo_pt, geo1 = geo_ln, sf_column_name = "geo1")
  )

  expect_identical(
    dplyr::rename(sf, foo = geo1),
    st_sf(x = 1, geo2 = geo_pt, foo = geo_ln, sf_column_name = "foo")
  )
  expect_identical(
    dplyr::rename(sf, foo = geo1, y = x),
    st_sf(y = 1, geo2 = geo_pt, foo = geo_ln, sf_column_name = "foo")
  )
  expect_identical(
    dplyr::rename(sf, foo = geo1, y = x, bar = geo2),
    st_sf(y = 1, bar = geo_pt, foo = geo_ln, sf_column_name = "foo")
  )
})

test_that("`select()` and `transmute()` observe back-stickiness of geometry column (#1425)", {
	skip_if_not_installed("dplyr")
	sf = read_sf(system.file("shape/nc.shp", package = "sf"))
	exp = sf[, c("NAME", "FIPS")]
	expect_identical(dplyr::select(sf, NAME, FIPS), exp)
	expect_identical(dplyr::transmute(sf, NAME, FIPS), exp)
})

test_that("rowwise_df class is retained on row slice", {
	skip_if_not_installed("dplyr")
	expect_true(nc %>% rowwise() %>% slice(1) %>% inherits("rowwise_df"))
})

test_that("grouped_df class is retained on row slice", {
	skip_if_not_installed("dplyr")
	expect_true(nc %>% group_by(PERIMETER > 2) %>% slice(1) %>% inherits("grouped_df"))
})

test_that("rowwise_df class is retained on filtered rows", {
	skip_if_not_installed("dplyr")
	expect_true(nc %>% rowwise() %>% filter(AREA > .1) %>% inherits("rowwise_df"))
})

test_that("`group_split.sf()` ignores `.keep` for rowwise_df class", {
	skip_if_not_installed("dplyr")
	expect_no_warning(nc %>% rowwise() %>% group_split())
})

test_that("group_split.sf()` does not ignore `.keep` for grouped_df class", {
	skip_if_not_installed("dplyr")

	nc_kept <- nc %>%
		group_by(CNTY_ID) %>%
		group_split(.keep = TRUE)

	nc_notkept <- nc %>%
		group_by(CNTY_ID) %>%
		group_split(.keep = FALSE)

	expect_identical(names(nc_kept[[1]]), names(nc))
	expect_identical(names(nc_notkept[[1]]), setdiff(names(nc), "CNTY_ID"))
})
