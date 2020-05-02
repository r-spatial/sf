if(getRversion() < "3.3.0") {
  stop("Your version of R is too old. This package requires R-3.3.0 or newer on Windows.")
}

# For details see: https://github.com/rwinlib/gdal3
VERSION <- commandArgs(TRUE)
# wrong path: if(!file.exists(sprintf("../windows/gdal3-%s/include/gdal/gdal.h", VERSION))){
testfile <- sprintf("../windows/gdal3-%s/include/gdal-%s/gdal.h", VERSION, VERSION)
if(!file.exists(testfile)){  
  if(getRversion() < "3.3.0") setInternet2()
  download.file(sprintf("https://github.com/rwinlib/gdal3/archive/v%s.zip", VERSION), "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}
