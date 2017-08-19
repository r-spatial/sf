suppressPackageStartupMessages(library(sf))
ll = st_crs(4326)
sf:::crs_parameters(ll)$ud_unit

u = names(sf:::udunits_from_proj)

unrecognized = NULL
out = sapply(u, function(x) { 
  p4s = paste0("+proj=aea +units=", x)
  cat(x, ": ")
  ret = try(sf:::crs_parameters(st_crs(p4s))$ud_unit, silent = TRUE)
  if (! inherits(ret, "try-error"))
  	print(ret)
  else
    unrecognized = c(unrecognized, x)
})

if (length(unrecognized))
	print(paste("unrecognized units:", paste(unrecognized, collapse = ", "), ": older GDAL version?"))
