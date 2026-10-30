test_that("st_as_sf(<ppp>) does not ignore CRS", {
  skip_if_not_installed("spatstat.geom")

  sf_obj = st_read(system.file("gpkg/b_pump.gpkg", package = "sf"), quiet = TRUE)
  ppp_obj = spatstat.geom::as.ppp(sf_obj)
  expect_equal(
    st_as_sf(ppp_obj, crs = st_crs(sf_obj)) |> st_crs(), 
    st_crs(sf_obj)
  )
})