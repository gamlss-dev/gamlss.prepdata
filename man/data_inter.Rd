\name{data_inter}
\alias{data_inter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identifying Pair-Wise Interactions in the Data Frames
}
\description{
The function \code{data_inter()} is trying to identify pair-wise interations given the response variable using  linear regression methodology. At the moment it works only with continuous reponse variables.
}
\usage{
data_inter(data, response, weights, digits = 3, plot = TRUE, 
        lower.tri.off = TRUE, 
        method = c("circle", "square"), fit.method = c("linear", "nonlinear"), 
        outline.color = "gray", colors = c("blue", "white", "red"), 
        legend.title = "Inter", title, ggtheme = theme_minimal(), 
        tl.cex = 12, tl.col = "black", tl.srt = 45, lab = TRUE, 
        lab_col = "black", lab_size = 3, circle.size = 20,  
        percentage, seed = 123, print.info = TRUE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame}
  \item{response}{the response variable}
  \item{weights}{prior weights}
  \item{digits}{the number of digits in the plot}
  \item{plot}{whether to plot the results}
 \item{lower.tri.off}{whether to show the lower part of the matrix}
  \item{method}{plotting in \code{"square"} or \code{"cicle"}}
  \item{fit.method}{whether in \code{"linear"} or \code{"nonlinear"}}
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
  \item{print.info}{whether to print infomation when cutting the data usinf  \code{data_cut()}}
  \item{percentage}{the percentage of data to show if the observation number is too big}
  \item{seed}{Setting a seed value for selection of the percantage of data (for big data sets)}
}
\details{The function \code{data_inter()} uses the funcion \code{z_scores()} to standarized the continuous response variable and then uses linear model fits to establish whether the  first order interactions between the the x's are singificant or not. It reports the significant level based on Chi-square tests. Note that for large data sets it uses the function \code{data_cut()} to cut randomnly the size of the data in order to use \code{ggplo2} graphs to plot it.    

Typically for linear model first ortder interaction it fits the models \code{y~x1+x2} and \code{y~x1*x2}, respectively, and calculated significant level based on the difference in deviances. Under the \eqn{H_o} hypothesis  the  difference in deviances follow be a Chi-square distribution with degrees of freedom based on  the difference of the degrees of freedom of the two fitted models.   

}
\value{
It produce a plot \code{plot=TRUE} or a square upper triangular table. 
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
The function  \code{data_inter()} works only for continuous responses.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{
\code{\link{data_cor}}
}

\examples{
data_inter(rent[,-4,5], response=R)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{regression}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
