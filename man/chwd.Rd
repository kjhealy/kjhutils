\name{chwd}
\alias{chwd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Change working directory.
%%  ~~function to do ... ~~
}
\description{
Change the working directory within the current project.
}
\usage{
chwd(dname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dname}{
The subfolder of the current project that you want to change to. Usually
"setup", "analysis", or "paper". 
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
Changes the working directory.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Kieran Healy.
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
function (dname) 
{
    parent.path <- dirname(getwd())
    new.wd <- paste(parent.path, "/", dname, sep = "")
    setwd(new.wd)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~chdir }
\keyword{ ~change }% __ONLY ONE__ keyword per line
\keyword{ ~directory }% __ONLY ONE__ keyword per line
