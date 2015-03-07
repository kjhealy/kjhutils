##' Color-blind friendly palette
##' From \url{http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/}
##' @title Color-blind friendly palette
##' @param palette Choose "cb", "rcb", or "bly". cb is the Winston
##' Chang color blind palette; rcb is that palette in reverse; bly
##' puts the yellow and blue in the palette first.
##' @return Variations on an eight-color color-blind friendly palette.
##' @author Kieran Healy
##' @export
my.colors <- function(palette="cb"){
  ### The palette with grey:
  cb.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  ## Same one Reversed
  rcb.palette <- rev(cb.palette)
  ## Blue and yellow first choices
  bly.palette <- c("#E69F00", "#0072B2", "#999999", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#CC79A7")
  if (palette=="cb") return(cb.palette) else if (palette=="rcb") return(rcb.palette) else if (palette=="bly") return(bly.palette) else stop("Choose cb, rcb, or bly ony.")
}
