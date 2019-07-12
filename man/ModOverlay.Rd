\name{ModOverlay}
\alias{ModOverlay}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Overlays selected modules onto the basic mapping.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ModOverlay(base, overlay, n)
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
function (base, overlay, n) 
{
    maxClust = as.numeric(unique(overlay$colors))
    if (n == 1) {
        manipulate(singleModOverlay(base = base, overlay = overlay, 
            n = c(mod1), color = c(col1)), mod1 = slider(1, length(maxClust), 
            label = "First Module"), col1 = picker("Red", "Orange", 
            "Yellow", "Green", "Blue", "Purple", "Violet", label = "Color of Module"))
    }
    if (n == 2) {
        manipulate(singleModOverlay(base = base, overlay = overlay, 
            n = c(mod1, mod2), color = c(col1, col2)), mod1 = slider(1, 
            length(maxClust), label = "First Module"), col1 = picker("Red", 
            "Orange", "Yellow", "Green", "Blue", "Purple", "Violet", 
            label = "Color of Module"), mod2 = slider(1, length(maxClust), 
            label = "Second Module"), col2 = picker("Red", "Orange", 
            "Yellow", "Green", "Blue", "Purple", "Violet", label = "Color of Module"))
    }
    if (n == 3) {
        manipulate(singleModOverlay(base = base, overlay = overlay, 
            n = c(mod1, mod2, mod3), color = c(col1, col2, col3)), 
            mod1 = slider(1, length(maxClust), label = "First Module"), 
            col1 = picker("Red", "Orange", "Yellow", "Green", 
                "Blue", "Purple", "Violet", label = "Color of Module"), 
            mod2 = slider(1, length(maxClust), label = "Second Module"), 
            col2 = picker("Red", "Orange", "Yellow", "Green", 
                "Blue", "Purple", "Violet", label = "Color of Module"), 
            mod3 = slider(1, length(maxClust), label = "Third Module"), 
            col3 = picker("Red", "Orange", "Yellow", "Green", 
                "Blue", "Purple", "Violet", label = "Color of Module"))
    }
    if (n == 4) {
        manipulate(singleModOverlay(base = base, overlay = overlay, 
            n = c(mod1, mod2, mod3, mod4), color = c(col1, col2, 
                col3, col4)), mod1 = slider(1, length(maxClust), 
            label = "First Module"), col1 = picker("Red", "Orange", 
            "Yellow", "Green", "Blue", "Purple", "Violet", label = "Color of Module"), 
            mod2 = slider(1, length(maxClust), label = "Second Module"), 
            col2 = picker("Red", "Orange", "Yellow", "Green", 
                "Blue", "Purple", "Violet", label = "Color of Module"), 
            mod3 = slider(1, length(maxClust), label = "Third Module"), 
            col3 = picker("Red", "Orange", "Yellow", "Green", 
                "Blue", "Purple", "Violet", label = "Color of Module"), 
            mod4 = slider(1, length(maxClust), label = "Third Module"), 
            col4 = picker("Red", "Orange", "Yellow", "Green", 
                "Blue", "Purple", "Violet", label = "Color of Module"))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
