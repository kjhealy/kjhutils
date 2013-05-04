\name{makeFootnote}
\alias{makeFootnote}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a footnote to a figure.
}
\description{
  Add a note.
}
\usage{
%% makeFootnote(footnoteText = format(Sys.time(), "%d %b %Y"), size = 0.7, color = grey(0.5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{footnoteText}{
Text.
}
  \item{size}{
Size.
}
  \item{color}{
Color.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (footnoteText = format(Sys.time(), "\%d \%b \%Y"), size = 0.7, color = grey(0.5)) 
{
    require(grid)
    pushViewport(viewport())
    grid.text(label = footnoteText, x = unit(1, "npc") - unit(2, "mm"), y = unit(2, "mm"), just = c("right", "bottom"), gp = gpar(cex = size, col = color))
    popViewport()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ footnote }

