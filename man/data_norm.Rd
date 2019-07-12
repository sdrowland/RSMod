\name{data_norm}
\alias{data_norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subset, Log2 transform and normalizes data.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
data_norm(trans, sub)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trans}{
%%     ~~Describe \code{trans} here~~
}
  \item{sub}{
%%     ~~Describe \code{sub} here~~
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
function (trans, sub) 
{
    names = colnames(trans[2:ncol(trans)])
    colnames(trans) = c("itag", names)
    if (!(missing(sub))) {
        colnames(sub) = c("itag")
        data3 = data[which(trans$itag \%in\% sub$itag), ]
        rownames(data3) = data3$itag
        data3 = data3[, c(2:ncol(data3))]
    }
    if (missing(sub)) {
        data3 = data
        rownames(data3) = data3$itag
        data3 = data3[, c(2:ncol(data3))]
    }
    countsadj = log2(data3)
    countsadj = as.matrix(countsadj)
    countsadj[is.infinite(countsadj)] = 0
    countsadj[is.na(countsadj)] = 0
    norm.data = data.Normalization(countsadj, type = "n1", normalization = "row")
    norm.data[is.na(norm.data)] = 0
    norm.data[is.infinite(norm.data)] = 0
    return(norm.data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
