\name{EM_main}
\alias{EM_main}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Implement EM
}
\description{
This is the main function of EM algorithm. Use wrapper function EM. 
}
\usage{
EM_main(data, alpha, mean, sigma, error = 1e-06, n_exp = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  data to cluster, only allow numeric columns.
}
  \item{alpha}{
  list or vector, The probability of each cluster.
}
  \item{mean}{
  list of vector, mean of each cluster.
}
  \item{sigma}{
  list of matrix, cov matrix of each cluster.
}
  \item{error}{
  numeric, the error rate to stp iteration. Default 1e-6.
}
  \item{n_exp}{
  numeric, the limit of iteration time.
}
}
\details{
  
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
## Dont use this function! 
## But here's some example.
alpha = list(a=0.33,b=0.34,c=0.33)
mean = list(a=c(5.8,3.0,3.76,1.2),
            b=c(0.8,2.0,1.76,10.2),
            c=c(5.8,30.0,3.76,11.2))
sigma = list(a=diag(1,4),
             b=diag(1,4),
             c=diag(1,4))

res = EM_main(iris[,-5],alpha,mean,sigma)
res

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
