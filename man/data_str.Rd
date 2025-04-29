\name{data_str}
\alias{data_str}
\alias{y_distinct}
\alias{data_distinct}
\alias{data_cha2fac}
\alias{data_few2fac}
\alias{data_int2num}
\alias{data_fac2num}


\title{Function applied to data}
\description{
his is a set of function are designed to help the user to deal with the structure of new data sets.
}
\usage{
data_str(data, min.values = 100, min.levels = 10)

y_distinct(var)

data_distinct(data, get.distinct = FALSE, print=TRUE)

data_cha2fac(data, show.str = FALSE)

data_few2fac(data, max.levels = 5, show.str = FALSE)

data_int2num(data, min.values = 50, show.str = FALSE)

data_fac2num(data, vars)
}

\arguments{
  \item{data}{a data frame}
  \item{min.values}{the minimal value distinct values before warning }
  \item{max.levels}{the maximum value for distinct values in the variable}
   \item{min.levels}{the minimal value distinct levels befor warning}
  \item{var}{a vector}
  \item{vars}{a character vector with names from the data}
   \item{get.distinct}{TRUE if you need to save the values FALSE if not not}
  \item{show.str}{whether to show the structure}
   \item{print}{TRUE or FALSE}
}
\details{
The function \code{data_str()} gives the structure of the data set.

The function \code{data_distinct()} gives the distinct values of the vectors in the data set

The function \code{y_distinct()} gives the distinct values of single  vector

The function \code{data_cha2fac()} tranforms all character vectors to factors

The function \code{data_few2fac()} transform all vectors with fewer values than \code{min.levels} into factors

The function \code{data_int2num()} transform all integer vectors with more values than \code{min.values} into numeric

The function \code{data_fac2num() transform sellected variables factors into numeric vectors} 
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
\author{Mikis Stasinopoulos, Bob Rigby and Fernanda De Bastiani}



\seealso{\code{\link{data_dim}}}
\examples{
data_str(rent)
data_distinct(rent)
data_cha2fac(rent)
data_few2fac(rent)
data_int2num(rent)
}

\keyword{regression}

