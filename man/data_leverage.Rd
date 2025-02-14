\name{data_leverage}
\alias{data_leverage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Finding high leverage points }
\description{
The function \code{data_leverage()} uses linear model methodology to identify unusual observations in 
the explnatory variables.
}
\usage{
data_leverage(data, response, weights, quan.val = 0.99, annotate = TRUE, 
     line.col = "steelblue4", point.col = "steelblue4", annot.col = "darkred", 
     plot = TRUE, title, percentage, seed = 123, ...)
}
           
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame}
  \item{response}{the response variable}
  \item{weights}{prior weights}
  \item{quan.val}{the quantile value to plot the line}
  \item{annotate}{whether to annotate the points}
  \item{line.col}{the colour of the horozontal line}
  \item{point.col}{the colour of the leverage points}
  \item{annot.col}{the colour of the annotated points}
  \item{plot}{whether to plot}
  \item{title}{whether to put a title}
  \item{percentage}{for very large data the percentage of points to use in the calculations}
  \item{seed}{the seed needed for selecteing a subset of data if only a percentage of points are plotted}
  \item{\dots}{for extra argument}
}
\details{
The method used is as follows. Fit a linear model to all explanatory variables in the data and calculate the leverge points.  
}
\value{
The function  plot the leverge points unless \code{plot=FALSE} is used where in thos case the leverage points can be saved.  
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion),
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.


Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017) \emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}
\author{
Mikis Stasinopoulos
}

\seealso{\code{\link{data_outliers}}}
\examples{
data_leverage(rent, response=R)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{regression}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{leverage}
% \concept{ ~cpt2 }
% Use only one concept per line.
