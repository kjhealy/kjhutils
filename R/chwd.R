chwd <-
function(dname){
  parent.path <- dirname(getwd())
  new.wd <- paste(parent.path, "/", dname, sep="")
  setwd(new.wd)
}
