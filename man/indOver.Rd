\name{indOver}
\alias{indOver}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Overlay gene list onto mapping.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
indOver(set, overlay)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{set}{
%%     ~~Describe \code{set} here~~
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
function (set, overlay) 
{
    cluster = overlay
    ind.genes = set
    cluster$genes = rownames(cluster)
    cluster$genes = substr(cluster$genes, 1, 14)
    names = colnames(ind.genes[2:ncol(ind.genes)])
    colnames(ind.genes) = c("itag", names)
    ind.genes$itag = substr(ind.genes$itag, 1, 14)
    cluster.color = cluster
    cluster.color.number = as.data.frame(cluster.color[which(cluster.color$genes \%in\% 
        ind.genes$itag), ])
    cluster.color$colors = NA
    cluster.color.number$colors = "red"
    cluster.color$colors[is.na(cluster.color$colors)] = "grey"
    cluster.grey = cluster.color[cluster.color$colors == "grey", 
        ]
    cluster.red = cluster.color.number
    p = ggplot(cluster.grey, aes(x, y)) + theme_bw() + geom_point(size = 1.5, 
        colour = cluster.grey$colors) + geom_point(data = cluster.red, 
        fill = cluster.red$colors, pch = 21, size = 5) + theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) + 
        theme(legend.position = "none") + theme(legend.position = "none") + 
        theme(axis.text.x = element_blank(), axis.text.y = element_blank()) + 
        xlab("") + ylab("")
    print(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
