###--------------------------------------------------
### Utility functions
###--------------------------------------------------

## Not-in operator
"%nin%" <- Negate("%in%")


##' Refer to viewport layouts easily (Hadley Wickham, ggplot2 book)
##' @title vplayout
##' @param x n rows in viewport
##' @param y n columns in viewport
##' @return A viewport for plotting with x rows and y columns
##' @author Kieran Healy
##' @export
vplayout <- function(x, y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}

##' Footnote a plot.
##' @title makeFootnote
##' @param footnoteText Defaults to date stamp.
##' @param size
##' @param color
##' @return plot footnote
##' @author Kieran Healy
##' @export
makeFootnote <- function(footnoteText=
                         format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
             x = unit(1,"npc") - unit(2, "mm"),
             y= unit(2, "mm"),
             just=c("right", "bottom"),
             gp=gpar(cex= size, col=color))
   popViewport()
}


### Plot a credit line
##' Credit a plot
##' @title credit
##' @param credit.text Who gets the credit. Defaults to my name
##homepage URL.
##' @return credit
##' @author Kieran Healy
##' @export
credit <- function(credit.text="Kieran Healy, http://kieranhealy.org\n") {
  return(makeFootnote(credit.text))
}


##' Euclidean distance for two vectors
##' @title euc.dist
##' @param x1 First vector
##' @param x2 Second vector
##' @return The euclidean distance between x1 and x2
##' @author Kieran Healy
##' @export
euc.dist <- function(x1, x2){
  x <- sqrt(sum((x1 - x2) ^ 2))
  return(x)
}

##' Convenience function to show a palette of colors
##' from http://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
##' @title show.pal
##' @param col Vector of rgb colors
##' @param border border background
##' @return Plots the vector of colors you feed it
##' @author Kieran Healy
##' @export
show.pal <- function(col, border="light gray" , ...){
    n <- length(col)
    plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE, xlab = "", ylab = "", ...)
    rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
