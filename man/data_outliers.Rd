\name{data_outliers}
\alias{data_outliers}
\alias{y_outliers}
\alias{y_outliers_both}

\title{Outlier identification}
\description{
Those  functions idententify outliers in variables in the data.
The functions \code{} and \code{}


}
\usage{
data_outliers(data, value, min.distinct = 50, family = SHASHo, 
         type = c("zscores","quantile") )

y_outliers(var, value, family = SHASHo,  type = c("zscores","quantile"))

y_outliers_both(var, value, family = SHASHo, 
        method =c("intersect", "union"))
}

\arguments{
  \item{data}{a data frame}
  \item{var}{a continues variable}
  \item{value}{max value from which the absolute value of the z-scores should be  greater to identify outliers}
  \item{min.distinct}{if a variable has less distinct values than \code{min.distinct} is excluded}
  \item{family}{the distribution family used for standardization}
  \item{type}{for \code{y_outliers()} and \code{data_outliers()} the type of outlier detection \code{"zscores"} or \code{"quantile"}}
  \item{method}{for \code{y_outliers()} and \code{data_outliers()} the type of outlier detection \code{"zscores"} or \code{"quantile"}}
  for  \code{y_outliers_both()} whether the \code{"intersect"} (default) or the \code{"union"} of the two sets will be used
}
\details{the continuous variables are power transforemed and then standartised}
\value{
return a list
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
\code{\link{data_names}}
}
\examples{
da <- rent99[,-2]
data_outliers(da)
}

