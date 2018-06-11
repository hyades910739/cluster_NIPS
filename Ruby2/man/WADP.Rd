\name{WADP}
\alias{WADP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  WADP
}
\description{
  WADP examine the robustness of the dataset. In each column, WADP adds some noise( normal distribution with stadard deviation proportional to column SD). 
  
  Usually, the WADP will conduct several times and take average.
}
\usage{
WADP(data, noise_rate = 0.1, n_iter = 10, method = "EM", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  dataframe or matrix, the data to cluster.
}
  \item{noise_rate}{
  numeric, if noise_rate is x, then we add normal noise with mean=0 and sd=sd(col(i))*a Col(i) to each columns.
}
  \item{n_iter}{
  numeric, how many times WADP is conducted.
}
  \item{method}{
  character, only accept "EM" or "Kmeans".
}
  \item{\dots}{
  other parameters passing to function EM or function Kmeans.
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
# test for Kmeans.
res = WADP(iris[,-5],noise_rate = 0.1,n_iter = 100,method = "Kmeans",centers=3,nstart=10)
summary(res)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
