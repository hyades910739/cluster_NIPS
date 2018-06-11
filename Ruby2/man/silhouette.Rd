\name{silhouette}
\alias{silhouette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Silhouette
}
\description{
  This is the warpper function for cluster::silhouette.
}
\usage{
silhouette(cluster,data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cluster}{
  vector, the cluster result vector.
}
  \item{data}{
  the data which you implement cluster.
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
silhouette(res$cluster,iris[,-5])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
