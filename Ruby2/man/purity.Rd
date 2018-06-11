\name{purity}
\alias{purity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Compute purity of cluster result.
}
\description{
  Purity is defined as the accuracy rate. Namely, the # of observations that is correctly clustered.
  
}
\usage{
purity(label, cluster)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{label}{
  vector, the labels in original data.
}
  \item{cluster}{
  vector, the cluster result.
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
res = EM(iris[,-5],3,10,seed = 689)
purity(iris[,5],res$cluster)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
