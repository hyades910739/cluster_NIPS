\name{EM}
\alias{EM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
EM Algorithm for Gaussian Mixture Model
}
\description{
  use EM to implement clustering.
}
\usage{
EM(data, cluster, random = 10, error = 1e-06, n_exp = 100, seed = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  dataframe or matrix,data for clustering, only allow numeric columns.
}
  \item{cluster}{
  numeric, the number of cluster.
}
  \item{random}{
  numeric, specify how many random starter, the best(best log liklihood) is kept. Default 10. 
}
  \item{error}{
  numeric, the error rate to stp iteration. Default 1e-6.
}
  \item{n_exp}{
  numeric, the limit of iteration time.
}
  \item{seed}{
  numeric or NULL, default is NULL. If not NULL,set seed for random starter. 
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
