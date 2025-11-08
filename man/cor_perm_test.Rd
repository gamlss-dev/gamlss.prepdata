\name{cor_perm_test}
\alias{cor_perm_test}
\alias{cor_boot}



\title{
Pemutatione and Boostrap Tests for Paitwise EReletionships
}
\description{
There are several functions here;

\code{cor_perm_test} peerfoems a premutation test 

}
\usage{
cor_perm_test(x, y, data = NULL, B = 1000, seed = 123, 
  tail = c("one", "two"), fun = cor, ...)
  
cor_boot(x, y, data = NULL, B = 1000, seed = 123, 
  tail = c("one", "two"), fun = cor, ...)  
  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a coninuous variable}
  \item{y}{a coninuous variable}
  \item{data}{the data whete to find \code{x} and \code{y}}
  \item{B}{Th enumber of simulations or boostraps}
  \item{seed}{settinf the seed number}
  \item{tail}{\code{one} or \code{two} tail test}
  \item{fun}{the function to calculate the assosiation measeure between two contibuous variables}
  \item{\dots}{extar argument pass to the function, \code{fun}}
}

\details{Those two function help to test pairwise associations. The function \code{cor_perm_test()}
uses the Fisher's permutation test while \code{cor_boot()} using bootstraping. }
\value{
The function \code{cor_perm_test()} retunrs a \code{R} object called  \code{permutationTest} with methods \code{print} and \code{plot}.  

}
\references{
Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. \doi{10.1201/9780429298547}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}


Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}
\author{Mikis Stasinopoulos}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{data_cor}
\examples{
data_cor(rent99)
pp <-  cor_perm_test(rent99$rent, rent99$rent)
pp
plot(pp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{regression}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
