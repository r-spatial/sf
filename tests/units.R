suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(units))

if (utils::packageVersion("units") >= "0.5-0")
	units_options(auto_convert_names_to_symbols = FALSE)

st_crs(4326)$ud_unit

u = names(sf:::udunits_from_proj)[1:21]

unrecognized = NULL
out = sapply(u, function(x) { 
  p4s = paste0("+proj=laea +units=", x)
  cat(x, ": ")
  ret = try(st_crs(p4s)$ud_unit, silent = TRUE)
  if (! inherits(ret, "try-error"))
  	print(ret)
  else
    unrecognized = c(unrecognized, x)
})

if (length(unrecognized))
	print(paste("unrecognized units:", paste(unrecognized, collapse = ", "), ": older GDAL version?"))
