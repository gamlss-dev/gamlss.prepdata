\name{data_dim}
\alias{data_dim}
\alias{data_na_vars}
\alias{data_na_obs}
\alias{data_names}
\alias{data_shorter_names}
\alias{data_rename}
\alias{data_omit}


\title{Function to get information  from data.}
\description{
This is a set of function are designed to help the user to deal with new data sets.

\code{data_dim()}: the class, the  dimension and the \% NA's in the data  

\code{data_na_vars()}: which variables have NA's and how many

\code{data_na_obs()}: which observations have NA's 

\code{data_omit()}: omit the NA's from the data.

\code{data_names()}: The names of the variables in the data.

\code{data_shorter_names()}: abbriviate the names up to specified digits. 


\code{data_rename()} renames some of of the variables.


}
\usage{

data_dim(data)

data_na_vars(data)

data_na_obs(data)

data_omit(data)

data_names(data)

data_shorter_names(data, max = 5, newnames)

data_rename(data, oldnames, newnames)
}

\arguments{
  \item{data}{a data frame}
  \item{max}{the maximum number of characters allowed, with default 5. Make sure that you are using enought characters otherwise you could end up with variables with the some name }
  \item{newnames}{New names if not abbreviation is required, as characters}
  \item{oldnames}{the old names as characters}
}
\details{
The function \code{data_dim()} gives the the class, the dimension and the \% NA's in the data.

The function \code{data_na_vars()} gives the number of missing observation for each variable in the data.


The function \code{data_omit()}: omits the NA's from the data.

The function \code{data_names()} gives the names of the variables.

The function \code{data_shorter_names()} takes the current names and abbreviates to \code{max} characters.

The function \code{data_rename()} renames variable from the \code{data}.
}
\value{

The function \code{data_dim()} after printing gives the originasl data set. 

The function \code{data_na_vars()} prints the number of missing observation for each variable in the data and passes the original data set. 

The function \code{data_omit()}: omits the NA's from the data and passes the new data set.

The function \code{data_names()} prints the names of the variables in the data andpasses the original data  

The function \code{data_shorter_names()} takes the current names and abbreviates to \code{max} characters and return the  data with shorter names.
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
Mikis Stasinopoulos, Bob Rigby and Fernanda De Bastiani
}

\seealso{\code{\link{data_cor}}}
\examples{
data_dim(rent)
data_na_vars(rent)
data_na_obs(rent)
data_omit(rent)
data_names(rent)
data_shorter_names(rent)
pp=data_rename(rent, c("R", "Fl"), c("rent", "floor"))
data_names(pp)
}

\keyword{regression}

