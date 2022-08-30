if (require(testthat, quietly = TRUE)) {
 suppressPackageStartupMessages(library(sf))
 test_check("sf")
}
