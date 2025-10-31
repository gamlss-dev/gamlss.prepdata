\name{data_mcor}
\alias{data_mcor}
\alias{mcor}
\alias{ACE}
\alias{ACE.iter}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to fit Maximal Correlation to  Data
}
\description{
The function \code{dsata_mcor()} is using the function \code{mcor()} to find the non-linear maximal correlation between the continuous variabes of the data. 

The function \code{mcor()} fits a single  maximal correlation. It uses the function `\code{ACE()}.


The function \code{ACE()} it takes two variables and produce a mong other thing the maximal coefficient.

The function \code{ACE.iter()} it takes two variables and show the fitting to acheive the maximal coefficient.

}
\usage{
data_mcor(data, digits = 3, plot = TRUE, diag.off = TRUE, 
       lower.tri.off = FALSE, method = c("square", "circle"), 
       fit.method = c("P-splines", "loess"), 
       outline.color = "gray", colors = c("blue", "white", "red"), 
       legend.title = "Corr", title, 
       ggtheme = ggplot2::theme_minimal(), tl.cex = 12,
       tl.col = "black", tl.srt = 45, lab = TRUE, lab_col = "black", 
       lab_size = 3, circle.size = 20, ...)
       
mcor(x, y, data = NULL, fit.method = c("loess", "P-splines"), 
               nseg = 10, max.df = 6, ...)       

ACE(x, y, weights, data = NULL, con_crit = 0.001, 
        fit.method = c("loess", "P-splines"), 
        nseg = 10, max.df = 6, ...)   
        
ACE.iter(x, y, weights, data = NULL, con_crit = 0.001, 
         fit.method = c("loess", "P-splines"), 
         nseg = 10, max.df = 6, ...)        
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame}
  \item{digits}{how many digit to show in the correlation table}
  \item{plot}{whether to plot the results}
  \item{diag.off}{whether to show the diagonal entries}
  \item{lower.tri.off}{whether to show the lower triangular prt of the matrix}
  \item{method}{plotting in "square" or "cicle}
  \item{fit.method}{whether to use "loess" or "P-splines"}
  \item{outline.color}{the outline colour}
  \item{colors}{the range of colours}
  \item{legend.title}{title for the legend}
  \item{title}{the main title}
  \item{ggtheme}{the theme use by \code{ggplot2} with default \code{theme_minimal()}}
  \item{tl.cex}{the text size for the marginal labels}
  \item{tl.col}{the colour of the he marginal labels}
  \item{tl.srt}{the angle of the text in the bottom labels of the table}
  \item{lab}{whether to show the correlation coefficients in the table}
  \item{lab_col}{the colour of the lettering of the correlation coefficients}
  \item{lab_size}{the size of the lettering of the correlation coefficients, increase (or decrease) if the defaul 3 is not appropriate}
  \item{circle.size}{the size of the circles, increase (or decrease) if 
         the defaul 20 is not appropriate}
    \item{x}{a continuous variable}
  \item{y}{a continuous variable}
  \item{weights}{prior weights}
  \item{con_crit}{convergence critirion for  \code{"P-splines"} fit}
  \item{nseg}{The number of breakpoints n the definition of \code{"P-splines"}}
  \item{max.df}{the maximal degree of freedom alowen if \code{"P-splines"} is fitted}
  \item{\dots}{for more arguments}
}
\details{
The function \code{dsata_mcor()} is using the function \code{mcor()} to find the non-linear maximum correlation between the continuous varaibes of the data. 
}
\value{
The function \code{data_mcor()} produve a table

The function \code{mcor()} produve the maxomal correlations

The function \code{ACE()} produve a object classn "ACE", with methods \code{plot()} ands \code{print()} 

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




\seealso{data_cor}
\examples{
data_mcor(rent99)
max.cor <-  data_mcor(rent99, plot=FALSE)

high_val(max.cor, val=0.5)
}

\keyword{regression}
