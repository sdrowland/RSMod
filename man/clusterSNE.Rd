\name{clusterSNE}
\alias{clusterSNE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Implementation of BH-SNE on RNA-Seq data.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clusterSNE(data.set, perp, dim, tha, pc, iter, seed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.set}{
    Normalized RNA-Seq data set.
}
  \item{perp}{
    Initial perplexity value provided to Rtsne. (See Rtsne documentation)
}
  \item{dim}{
    Number of final dimensions. Typically 2 or 3.
}
  \item{tha}{
    Theta value for Rtsne. Value should be between 0 and 1, with lower values more accurate but requiring significantly longer processing times. Default = 0.3. (See Rtsne documentation)
}
  \item{pc}{
    Whether to perform a PCA on the data prior to BH-SNE. Default = FALSE.
}
  \item{iter}{
    Number of iterations to perform. Default = 1000.
}
  \item{seed}{
    Set seed for reproducability of results. Defualt = 0.
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
## The function is currently defined as
function (data.set, perp, dim, tha, pc, iter, seed) 
{
    set.seed(seed)
    Rdis.sne = Rtsne(data.set, perplexity = perp, dims = dim, 
        theta = tha, initial_dims = ncol(data.set), check_duplicates = FALSE, 
        pca = pc, verbose = TRUE, max_iter = iter)
    Rdis.sne = as.data.frame(Rdis.sne[1:6])
    colnames(Rdis.sne) = c("Theta", "Perplexity", "N", "orgD", 
        "x", "y", "Costs")
    rownames(Rdis.sne) = rownames(data.set)
    p = ggplot(Rdis.sne, aes(x, y)) + theme_bw() + theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) + 
        geom_point(size = 2, pch = 21, fill = "grey") + theme(legend.position = "none")
    print(p)
    return(Rdis.sne)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
