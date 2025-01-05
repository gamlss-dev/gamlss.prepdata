\name{data_cor}
\alias{data_cor}
\alias{data_pcor}

\title{
Plotting pairwise linear and partial correlations.
}
\description{
The function \code{data_cor} takes a data frame and plots the pairwise Pearson's correlation coefficients of all continuous variables in the data.


The function \code{data_pcor} takes a data frame and plots the pairwise partial Pearson's correlation coefficients of all continuous variables in the data.
}
\usage{
data_cor(data,  digits = 3, plot = TRUE, diag.off = TRUE,
        lower.tri.off = FALSE, method = c("square", "circle"),
        outline.color = "gray", colors = c("blue", "white", "red"),
        legend.title = "Corr", title, ggtheme = theme_minimal(),
        tl.cex = 12, tl.col = "black", tl.srt = 45, lab = TRUE,
        lab_col = "black", lab_size = 3, circle.size = 20, seed = 123, 
        percentage)

data_pcor(data, digits = 3, plot = TRUE, diag.off = TRUE,
        lower.tri.off = FALSE, method = c("square", "circle"),
        outline.color = "gray", colors = c("blue", "white", "red"),
        legend.title = "Corr", title, ggtheme = theme_minimal(),
        tl.cex = 12, tl.col = "black", tl.srt = 45, lab = TRUE,
        lab_col = "black", lab_size = 3, circle.size = 20, seed = 123, 
        percentage)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame}
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
   \item{percentage}{this is for big data sets. if more tha a milion ony 10\% is plotted, if from 100.00 to a milion, 20\%, if 50.000 to 100.000, 50\% otherwise 100\% of the data.}
  \item{seed}{Setting a seed value for selection of the percantage of data (for big data sets)}
}

\value{
creates a correlation matrix plot.
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


\seealso{mcor}
\examples{
data_cor(rent)

data_pcor(rent)
}

\keyword{regression}

