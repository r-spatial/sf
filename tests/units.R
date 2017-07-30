suppressPackageStartupMessages(library(sf))
ll = st_crs(4326)
sf:::crs_parameters(ll)$ud_unit

u = names(sf:::udunits_from_proj)

out = sapply(u, function(x) { 
  p4s = paste0("+proj=aea +units=", x)
  cat(x, ": ")
  print(sf:::crs_parameters(st_crs(p4s))$ud_unit)
})
