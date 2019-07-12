\name{showtimeMapping}
\alias{showtimeMapping}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates new plot with identified modules.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
showtimeMapping(base, overlay)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base}{
%%     ~~Describe \code{base} here~~
}
  \item{overlay}{
%%     ~~Describe \code{overlay} here~~
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
function (base, overlay) 
{
    cluster = overlay
    map = base
    map$genes = rownames(map)
    cluster$genes = rownames(cluster)
    map$colors = 0
    n = max(cluster$colors)
    for (i in 1:nrow(cluster)) {
        map[which(cluster[i, "genes"] == map$genes), "colors"] = cluster[i, 
            "colors"]
    }
    cluster.color = map
    cluster.color$colors[cluster.color$colors == 0] = "light grey"
    cluster.color.sub = cluster.color[cluster.color$colors == 
        "light grey", ]
    cluster.color = cluster.color[cluster.color$colors != "light grey", 
        ]
    p = ggplot(cluster.color, aes(x, y)) + theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) + 
        theme(legend.position = "none") + geom_point(data = cluster.color.sub, 
        size = 2.5, color = cluster.color.sub$colors) + geom_point(size = 5, 
        pch = 21, fill = cluster.color$colors) + theme(axis.text.x = element_blank()) + 
        theme(axis.text.y = element_blank()) + theme(axis.ticks.x = element_blank()) + 
        theme(axis.ticks.y = element_blank()) + xlab("") + ylab("")
    print(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
