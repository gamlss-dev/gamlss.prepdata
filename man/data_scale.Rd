\name{data_scale}
\alias{data_scale}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scalling Continuous Variables in Data
}
\description{
The function \code{data_scale()} takes a \code{data.frame} and creates a new data set with all continous variable standarised. The standardization can be 

i) scale 0 variance 1 which is equivalent to have the options \code{scale.to="z-scores"} and \code{family="NO"}. That is the z-scores after fitting a normal distribution to a continuous variable

ii) A more general \code{z-score} using say  \code{scale.to="z-scores"} and \code{family="SHASH"} in which case correction to the skewness and kurtosis is done to the specified variable or

iii) the range is resticted from zero to one, i.e. \code{scale.to="0to1"}.

}
\usage{
data_scale(data, response, position.response = NULL, 
          scale.to = c("z-scores", "0to1"), family = "NO", 
          scale.response = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data frame}
  \item{response}{The name of the response variable}
  \item{position.response}{ or the position of the response variable in the data.
}
  \item{scale.to}{
how to scape by normalization, \code{scale.to="z-scores"} or range 0 to 1 \code{scale.to="0to1"}.
}
  \item{family}{The family used in the standarization, defaul is \code{family="NO"} but any other continuous family from \eqn{-Inf} to \eqn{+Inf} will do i.e. \code{family="SHASH"}. }
  \item{scale.response}{whether to scale also the response. The default value is \code{scale.response=FALSE} because in GAMLSS we are hoping to get the right family for the response.}
}

\value{
A data frame is return with all continous variables standarised.
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
Mikis Stasinopolos
}


\seealso{
\code{\link{data_zscores}}
}
\examples{
rent[, -c(4,5)] |> data_scale(, response=R)|> head()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
\keyword{regression}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
