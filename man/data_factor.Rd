\name{data_factor}
\alias{y_factor}
\alias{data_factor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Changing the Reference Level of Factors
}
\description{
Function to change the reference levels of factors. \code{y_factor()} takes only one factor  \code{data_factor()} takes a \code{data.frame}.



}
\usage{
y_factor(x, how = c("lower", "higher"))

data_factor(data, how = c("lower", "higher"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame}
  \item{x}{a variable}
  \item{how}{which reference level, default the one woth fewer obsrvervesions}
}

\value{
A factor  or a \code{data.frame} depending whether \code{y_factor()} or \code{data_factor()}  is used.
}
\references{
Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. \doi{10.1201/9780429298547}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}


Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).

}
\author{
Mikis Stasinopoulos
}
\note{
The idea here is that in model selection maybe we like the first level to be the weakest level so when we select levels the stonger level has a change if different.  This is more likelily to be usfull in \code{gamlss2()} where the stepwise select levels rather than factors. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{data_str}}
}
\examples{
levels(rent$B)
da<-data_factor(rent)
levels(rent$B)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
 \keyword{ ~regression }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
