\name{data_leverage}
\alias{data_leverage}

\title{
Getting the Leverage of All explanatory variables 
}
\description{
The function \code{data_leverage()} is designed to identify observations in the explanatory variable space with high leverage values and, therefore, potential outliers. 
}
\usage{
data_leverage(data, response, weights, quantile.value = 0.99, annotate = TRUE, 
            line.col = "steelblue4", point.col = "steelblue4", 
            annot.col = "darkred", plot = TRUE, title, percentage, 
            seed = 123, print.info=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The \code{data.frame}
}
  \item{response}{The response variable (to be excuded)}
  \item{weights}{Prior weights if needed}
  \item{quantile.value}{The quantile values for the identification of high leverage. The default is \code{quantile.value=0.99} so only the 1 percent of the high leverage are identifyied as outliers. This should be increase for large data}
  \item{annotate}{whether to annotete the the outliers}
  \item{line.col}{The color of the line}
  \item{point.col}{The colout of the plotting points]}
  \item{annot.col}{The colour of the annotate outliers}
  \item{plot}{whether to show the leverage plot}
  \item{title}{what title to put}
  \item{percentage}{what percentage of data to use in the plot}
  \item{seed}{The seed use to calculete the percentage}
  \item{print.info}{whether to print infomation when cutting the data usinf  \code{data_cut()}}
  \item{\dots}{other arguments}
}
\details{
The function \code{data_leverage()} uses the linear model methodology to identify unusual observations as a group within the explnatory variables. It fit a linear model to all explanatory variables in the data, calculate the leverge points and plots them. It identifies one percent of the data as outliers.
The line in the plot is calulated as \eqn{2(r/n)} where \eqn{r} is the number of explanatory variables and  \eqn{n} the number of obsrervations. \eqn{2(r/n)} is given in the literature as the rule of thumb is that an observation is considered to have high leverage.  In practice the value  \eqn{2(r/n)}  is too low for indetification of outliers. Here we use \code{quan.val=.99} which identify 1\% of the obsrvation with high leverage.    
}
\value{It plots the leverages \code{plot=TRUE} or identify outliers \code{plot=FALSE}.
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion),
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}
\author{
Mikis Stasinopoulos
}

\seealso{
\code{\link{data_zscores}},
}
\examples{
data_leverage(rent99[,-c(2,9)],  response=rent)
rent99[, -c(2,9)] |> data_leverage( response=rent, plot=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{regression}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
