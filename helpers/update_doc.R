#!/usr/bin/env Rscript

info<-function(...)
{
        message(sprintf("[%s] %s", Sys.time(), paste(..., collapse=" ") ))
}

library(devtools)
if(file.exists("README.Rmd")) {
    info("Building readme")
    build_readme()
}
info("Converting Roxygen files")
document()
info("Build pkgdown site")
pkgdown::build_site()

info("Done")

