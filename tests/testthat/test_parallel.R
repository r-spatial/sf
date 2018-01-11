context("sf: parallel tests")

test_that("is_valid_thread_number works", {
	expect_true(is_valid_thread_number(1))
	expect_true(is_valid_thread_number(1L))
	expect_false(is_valid_thread_number(0L, FALSE))
	expect_false(is_valid_thread_number("A", FALSE))
	expect_false(is_valid_thread_number(NA_integer_, FALSE))
	expect_error(is_valid_thread_number(0L))
	expect_error(is_valid_thread_number("A"))
	expect_error(is_valid_thread_number(NA_integer_))
	if (is_parallel_available()) {
		expect_false(is_valid_thread_number(1e+100, FALSE))
		expect_error(is_valid_thread_number(1e+100))
	}
})

test_that("is_parallel_available works", {
	expect_is(is_parallel_available(), "logical")
	expect_true(!is.na(is_parallel_available()))
})

test_that("number_processors works", {
	if (is_parallel_available()) {
		expect_is(number_processors(), "integer")
		expect_true(is.finite(number_processors()))
	} else {
		expect_error(number_processors())
	}
})

test_that("st_simple works", {
	skip_if_not(is_parallel_available())
	ls = st_linestring(rbind(c(0,0), c(1,1), c(1,0), c(0,1)))
	s = st_sfc(ls, st_point(c(0,0)))
	s = s[sample.int(length(s), 100, replace = TRUE)]
	expect_equal(st_is_simple(s), st_is_simple(s, threads = 2))
})
