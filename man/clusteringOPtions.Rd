\name{clusteringOPtions}
\alias{clusteringOPtions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performs analysis of different cutoff vaules for a BH-SNE clustering.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clusteringOPtions(cluster, dim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cluster}{
%%     ~~Describe \code{cluster} here~~
}
  \item{dim}{
%%     ~~Describe \code{dim} here~~
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
function (cluster, dim) 
{
    if (dim == 2) {
        rdis.test = as.matrix(cluster[, c("x", "y")])
        set.seed(0)
        chec = data.frame(matrix(ncol = 0, nrow = 0))
        for (i in seq(5, 100, 5)) {
            test.hd = hdbscan(rdis.test, minPts = i)
            che = as.data.frame(cbind(max(test.hd$cluster), i))
            chec = rbind(chec, che)
        }
        colnames(chec) = c("Modules", "Cutoff Value")
    }
    if (dim == 3) {
        rdis.test = as.matrix(cluster[, c("x", "y", "z")])
        set.seed(0)
        chec = data.frame(matrix(ncol = 0, nrow = 0))
        for (i in seq(5, 100, 5)) {
            test.hd = hdbscan(rdis.test, minPts = i)
            che = as.data.frame(cbind(max(test.hd$cluster), i))
            chec = rbind(chec, che)
        }
        colnames(chec) = c("Modules", "Cutoff Value")
    }
    return(chec)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
