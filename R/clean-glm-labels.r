##' Get the coefficient labels from a GLM
##'
##' Get coef labels for processing
##' @title get.glm.labels
##' @param model.name The name of the GLM
##' @return The coefficient labels
##' @author Kieran Healy
get.glm.labels <- function(model.name){
  varlabs <- names(coef(model.name))
  return(varlabs)
}
##' Strip periods from a string
##' None
##' @title rm.periods
##' @param labs The string to be processed
##' @return The string with no periods
##' @author Kieran Healy
rm.periods <- function(labs){
  o <- gsub("\\.", " ", labs)
  return(o)
}
##' Insert spaces around < and > signs
##' String processing
##' @title space.around.sign
##' @param labs the string
##' @return processed string
##' @author Kieran Healy
space.around.sign <- function(labs){
  o <- gsub(">", " > ", labs)
  o <- gsub("<", " < ", o)
  return(o)
}
##' Space after "Age"
##' Part of text processing GLM labels
##' @title Insert space after the word "Age"
##' @param labs string to operate on
##' @return string with space
##' @author Kieran Healy
space.after.age <- function(labs){
  o <- gsub("(Age)([0-9])", "\\1 \\2")
  return(o)
}
##' Inserts space between camelcased variables
##'  Part of text processing GLM labels
##' @title Space out CamelCase labels
##' @param labs the string
##' @return CamelCase -> Camel Case
##' @author Kieran Healy
insert.space <- function(labs){
  o <- gsub("([a-z])([A-Z])", "\\1 \\2", labs)
  return(o)
}
##' Apply processed labels to GLM
##' Change the GLM's labels to the processed string
##' @title Set GLM Labels
##' @param model.name The model
##' @param newlabs The new labels
##' @return Model with new coef labels
##' @author Kieran Healy
set.glm.labels <- function(model.name, newlabs){
  new.model <- model.name
  names(new.model$coefficients) <- newlabs
  return(new.model)
}
##' Process GLM labels for ascii table
##' Clean up a GLM's model labels for prettier printing
##' @title Clean GLM labels
##' @param model.name The GLM
##' @return The GLM with cleaner labels, for use with ascii()
##' @author Kieran Healy
fix.glm.labels <- function(model.name){
  olabs <- get.glm.labels(model.name)
  o <- insert.space(olabs)
  o <- space.around.sign(o)
  o <- space.after.age(o)
  newlabs <- rm.periods(o)
  out <- set.glm.labels(model.name, newlabs)
  return(out)
}
