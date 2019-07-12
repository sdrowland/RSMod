\name{clusteringOptics}
\alias{clusteringOptics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform Optics analysis of mapping.
}
\description{
Generates a 2D mapping dataframe with module selection using Optics from the dbscan package and KNNdist.
}
\usage{
clusteringOptics(cluster, cutoff)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cluster}{
Data frame obtained from BH-SNE analysis. Requires x and y coordinates for each point in the 2D mapping.
}
  \item{cutoff}{
The neighborhood size cutoff value based on KNNdist analysis. The typical range for BH-SNE mappings is between 2 and 5, other mappings will vary. Typical "optimal" value falls along the linear portion of the graph, but this will vary by dataset and type.
}
}
\details{
An x/y coordiante mapping is required for clusteringOptics. The OPTICS portion of the dbscan package is used along with KNNdist to determine the cutoff points for clusters. The value supplied to cutoff provides a value to KNNdist to generate the optimal eps for dbscan.
}
\value{
Returns a matrix with the orginal x and y coordinates and module number assignments listed as colors. These values are used for plotting.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Steven D. Rowland
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Coming Soon
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
