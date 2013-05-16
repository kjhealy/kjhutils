##' Delete everything in the project "cache" directory
##' Removed all files in "../cache" by default.
##' @title Delete knitr cache
##' @param cachedir The name of the directory in the present project
##(via dirname(getwd())) where files are to be deleted.
##' @return Deletes all files in the specified directory
##' @author Kieran Healy
##' @export
clear.knitr.cache <- function(cachedir="cache") {
  orig.dir <- getwd()
  parent.dir <- dirname(getwd())
  target.dir <- paste(parent.dir, "/", cachedir, sep="")
  full.cmd <- paste("rm -f ",target.dir,"/*.*", sep="")
  system(full.cmd)
  setwd(orig.dir)
}
