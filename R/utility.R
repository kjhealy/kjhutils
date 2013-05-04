###--------------------------------------------------
### Utility functions
###--------------------------------------------------

## Not-in operator
"%nin%" <- Negate("%in%")



## Refer to viewport locations easily (Wickham)
vplayout <- function(x, y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}

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
credit <- function(credit.text="Kieran Healy, http://kieranhealy.org\n") {
  return(makeFootnote(credit.text))
}


### Euclidean distance
euc.dist <- function(x1, x2){
  x <- sqrt(sum((x1 - x2) ^ 2))
  return(x)
}
