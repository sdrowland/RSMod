\name{module.patterns}
\alias{module.patterns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Show Individual Module Expression Patterns.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
module.patterns(modules,data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base}{
%%     ~~Describe \code{base} here~~
}
  \item{overlay}{
%%     ~~Describe \code{overlay} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{color}{
%%     ~~Describe \code{color} here~~
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
function (base, overlay, n, color) 
{
    if (missing(color)) {
        color = n
    }
    cluster = overlay
    map = base
    map$genes = rownames(map)
    cluster$genes = rownames(cluster)
    maxClust = as.numeric(unique(cluster$colors))
    cluster$colors = factor(cluster$colors, levels = 1:length(maxClust))
    cluster.split = split(cluster, cluster$colors)
    cluster.split.sub = cluster.split[n]
    for (i in 1:length(color)) {
        cluster.split.sub[[i]]$colors = color[i]
    }
    cluster.sub = do.call(rbind, cluster.split.sub)
    cluster.color = map
    cluster.color$colors = "light grey"
    cluster.color.sub = cluster.sub
    p = ggplot(cluster.color, aes(x, y)) + theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) + 
        theme(legend.position = "none") + geom_point(data = cluster.color, 
        size = 2.5, color = cluster.color$colors) + geom_point(data = cluster.color.sub, 
        size = 5, pch = 21, fill = cluster.color.sub$colors) + 
        theme(axis.text.x = element_blank()) + theme(axis.text.y = element_blank()) + 
        theme(axis.ticks.x = element_blank()) + theme(axis.ticks.y = element_blank()) + 
        xlab("") + ylab("")
    print(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
