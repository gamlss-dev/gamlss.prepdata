\name{data_scale}
\alias{data_scale}
\alias{data_vars2data}
\alias{data_formulae}
\alias{data_form2X}



%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scalling Continuous Variables in Data
}
\description{
The function \code{data_scale()} takes a \code{data.frame} and creates a new data set with all continous variable standarised. The standardization can be 

    i) mean 0 variance 1 which is equivalent to have the options \code{scale.to="z-scores"} and \code{family="NO"}. That is the z-scores (residuals) after fitting a normal distribution to a continuous variable

  ii) A more general \code{z-score} using say  \code{scale.to="z-scores"} and \code{family="SHASH"} in which case correction to the skewness and kurtosis is done to the specified variable. Finaly,
  
   iii) the range is resticted from zero to one, i.e. \code{scale.to="0to1"}.


The function \code{data_vars2data()} creates a new dataframe where the continuous variables are remain the same or become polynomials and the factors becomes dummy variables.   

The function \code{data_formulae()} takes a \code{data.frame} and creates four formulae

   1) The first contains the response and all main effects of the variables in the data i.e. \code{R~Fl+A+K+loc}
   
   2) The second contains the response and all first order interactions  i.e \code{R~(Fl+A+K+loc)^2}
   
   3) The third contains  all main effects of the variables in the data with no response i.e. \code{~Fl+A+K+loc}
   
   2) The fourth contains all first order interactions with no response  i.e \code{~(Fl+A+K+loc)^2}


The function \code{data_form2X} takes a \code{data.frame} and a forrmula and creates a design matrix.

}
\usage{
data_scale(data, response, position.response = NULL, 
          scale.to = c("z-scores", "0to1"), family = "NO", 
          scale.response = FALSE)
          
data_vars2data(data, response, exclude = NULL, 
          type = c("main.effect", "first.order"), 
          weights = NULL, nonlinear = FALSE, basis = "poly", 
          arg = 2)   

data_formulae(data, response)

data_form2X(data, formula, response, 
          scale.to = c("no", "z-scores", "0to1"), 
          family = NO)

          
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
  \item{exclude}{which variable to exclude}
  \item{type}{whether the main effects only ot dummies for first order interactions also}
  \item{weights}{Prior weights when the matrix is created}
  \item{nonlinear}{Im not sure what it does? }
  \item{basis}{what basis should be used for non linearities, deault is \code{poly()} }
  \item{arg}{the argument for the basis i.e 3}
  \item{formula}{the formula to create the design matrix}
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


rent[, -c(4,5)] |> data_vars2data( response=R, nonlinear = TRUE, 
                  basis = "poly", arg=3)  -> D
D |> head()
D |> str()

ff <- data_formulae(D, response=R)
ff

head(data_form2X(D, formula=ff[[1]], response=R))



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
