##'Change the working directory within a project.
##' @title chwd
##' @param dname The directory name to change to (e.g., "paper",
##"setup", "analysis" etc)
##' @return new dir
##' @author Kieran Healy
chwd <-
function(dname){
  parent.path <- dirname(getwd())
  new.wd <- paste(parent.path, "/", dname, sep="")
  setwd(new.wd)
}
