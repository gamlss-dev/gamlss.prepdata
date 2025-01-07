\name{data_rm}
\alias{data_rm}
\alias{data_exclude_class}
\alias{data_rm1val}
\alias{data_continuous}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function operating on data}
\description{
There are several function operating on a \code{data.frame} and export a \code{data.frame}. The functions are

1) \code{data_rm()}: this function removes the variables specified by \code{vars} from the \code{data.frame}. Note that \code{vars} can take either character names or numbers.

2) \code{data_rm1val()}: This function looks for varables with a unique distinct value (most likely factors left from a previous \code{subset()} operation) and remove them form the data. 

3) \code{data_exclude_class()}: This function looks for variable (columns) of a specified `R` class and remove them from the data. The default class is "factor".

4) \code{data_continuous()}": This function pick up only the continuous variable in the \code{data.frame}.


}
\usage{
data_rm(data, vars)

data_rm1val(data)

data_exclude_class(data, class.out = "factor")

data_continuous(data)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame}
  \item{vars}{selected variables (columns from the data frame)}
  \item{class.out}{a specific variable class to be excluded form the data frame}
}
\details{
 All the above  functions can be used for piping i.e. \code{da |> data_rm1val()}.
}
\value{
returm a \code{data.frame}
}
\references{
Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}
\author{Mikis Stasinopoulos, Bob Rigby and Fernanda De Bastiani}


\seealso{data_cor}
\examples{
library(gamlss)
da <- rent |> data_rm( vars=c("Sp", "Sm"))
head(da)

da<- rent |> data_exclude_class() 
head(da)

da<- data_continuous(rent)
head(da)
}

\keyword{regression}

