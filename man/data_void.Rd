\name{data_void}
\alias{data_void}
\alias{void}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finding the Percentage of Empty Spaces 
}
\description{
The function \code{void()} is looking for the \% of empty spaces in the direction of two variables \code{x} and \code{y}.

The function \code{data_void()} is looking pair-wise for  empty spaces in all the continuous variables in the \code{data} set.

}
\usage{
data_void(data, digits = 3, plot = TRUE, diag.off = TRUE, lower.tri.off = FALSE, 
method = c("square", "circle"), 
outline.color = "gray", colors = c("blue", "white", "red"), 
legend.title = "Void", title, ggtheme = ggplot2::theme_minimal(), 
tl.cex = 12, tl.col = "black", tl.srt = 45, lab = TRUE, 
lab_col = "black", lab_size = 3, circle.size = 20, seed = 123, percentage)

void(x, y, plot = TRUE, print = TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data frame}
  \item{digits}{the digits for printing the correlation coefficients}
  \item{plot}{whether to plot or not}
  \item{diag.off}{whether to show the diagonal ellements}
  \item{lower.tri.off}{whether to show the lower part of the matrix}
  \item{method}{plotting in \code{"square"} or \code{"cicle"}}
  \item{outline.color}{the outline colour}
  \item{colors}{the range of colours}
  \item{legend.title}{title for the legend}
  \item{title}{the main tittle}
  \item{ggtheme}{the theme for the plot, see package \pkg{ggthemes} for more themes}
  \item{tl.cex}{the text size for the marginal labels}
  \item{tl.col}{the colour of the he marginal labels}
  \item{tl.srt}{the angle of the text in the bottom labels of the table}
  \item{lab}{whether to show the correlation coefficients in the table}
  \item{lab_col}{the colour of the lettering of the correlation coefficients}
  \item{lab_size}{the size of the lettering of the correlation coefficients, increase (or decrease) if the defaul 3 is not appropriate}
  \item{circle.size}{the size of the circles, increase (or decrease) if the defaul 20 is not appropriate}
  \item{seed}{the \code{set.seed()} value}
  \item{percentage}{the percentage of data to show if the observation number is too big}
  \item{x}{the first variable in \code{void()}}
  \item{y}{the second variable in \code{void()}}
   \item{print}{whether to print the results }
}

 
\details{
The functions \code{void()} and \code{data_void()} work with discretising the data in the \code{x} and \code{y} direction and then calculate the \% of zeros.  By discretising the data  we mean cut the \code{x} variable in equal spaced \code{k} points. The same for  \code{y}.

The problem with any attempt to calculated the \% of empty spaces is that more detailed discretation (that is increasing \code{k}) in the \code{x} and \code{y} direction will results more \% empty spaces. To avoid this we need a way to stop the discretazation at a stage before the data become too sparce. The way is done here is as follows;

i) If the points are randomly allocated we would expect the number of counts in the table to be Poisson distributed with a probability for zeros equal to \eqn{exp(-\mu)} where \eqn{\mu} is the mean of the Poisson.

ii) To identify at which stage we should stop \code{k} for given number of observations say \code{n}, we have genarated randomly from the uniform distribution \code{n} values for \code{x} and \code{n} values for \code{y} and calculate which \code{k} will gives a probability of zero close to 0.05. We have found tha this relationship is firmly holding.
\eqn{\log k= -0.516+ 0.498 \log n} 
so \code{k} can be easlily calclated using the equation 
\eqn{k= \exp [-0.516+ 0.498 \log n]}.  This will quaranty that all data sets are treated equally.








}

\value{
It produce a value between zero and 1.
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


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{data_cor}}
}
\examples{
void(rent$A, rent$Fl)
data_void(rent)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ regression}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ diagnostic}
% \concept{ ~cpt2 }
% Use only one concept per line.
