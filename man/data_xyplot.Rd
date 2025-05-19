\name{data_xyplot}
\alias{data_xyplot}
\alias{data_plot}
\alias{data_bucket}
\alias{y_zscores}
\alias{data_zscores}
\alias{data_response}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting the response against the explanatory variables}
\description{
The function \code{data_xyplot()} plots the response against all other  variables
in a given data set.

The function \code{data_plot()} plots all variables individually.

The function \code{data_bucket()} plots the bucket plot for all continuous variables.

The function \code{data_zscores()} calculates and plots the z-scores (obtained after fitting the SHASHo distribution) for  all continuous variables.

The function \code{y_zscores()}  calculates and plots the z-scores (obtained after fitting the SHASHo distribution) for a single variable.

The function \code{data_response()}  calculates and plots the z-scores (obtained after fitting the SHASHo distribution) for the response variable.

}
\usage{
data_xyplot(data, response, point.size = 0.5, nrow = NULL,
           print.info = TRUE,
           ncol = NULL, percentage, seed = 123,  max.levels = 10,
           plots.per.page = 9, one.by.one = FALSE, title, ...)

data_plot(data, value = 3, hist.col = "black", hist.fill = "white",
           dens.fill = "#FF6666", nrow = NULL, ncol = NULL,
           percentage, seed = 123, print.info = TRUE,
           plot.hist = TRUE, plots.per.page = 9,
           one.by.one = FALSE,
           title, ...)
           
data_bucket(data, value = 3, max.levels = 20,
           nrow = NULL, ncol = NULL, plots.per.page = 9,
           one.by.one = FALSE, title, percentage, seed = 123, 
           print.info = TRUE,
           ...)

y_zscores(x, family = SHASHo, value  = 3, plot = TRUE, hist = FALSE, ...)

data_zscores(data, plot = TRUE, hist=FALSE, value = 3, family = SHASHo,
          max.levels = 10, hist.col = "black", hist.fill = "white",
          dens.fill = "#FF6666", nrow = NULL, ncol = NULL,
          plots.per.page = 9, one.by.one = FALSE, title, percentage,
          seed = 123,...)

data_response(data, response, plot = TRUE, 
             percentage, seed = 123, print.info = TRUE)

}

\arguments{
  \item{data}{a data frame}
  \item{x}{a single variable}
  \item{family}{a gamlss distribution family (continuous)}
  \item{response}{the respose variable should be in the data}
  \item{point.size}{the size of points in scatter plots}
  \item{nrow}{the number of rows in the plot}
  \item{ncol}{the number of columns in the plot}
  \item{plots.per.page}{maximu plots per page}
  \item{one.by.one}{whether plotted individually}
  \item{value}{value to identify outliers if  \code{y_dots} is used
  i.e. for upper tail an outliers is if it is greater than Q_3+value*IQ}
   \item{hist.col}{the colour of lines of the histogram, if \code{plot.hist=TRUE}}
  \item{hist.fill}{the colour of the histogram, if \code{plot.hist=TRUE}}
  \item{dens.fill}{the color of the density plot, if \code{plot.hist=TRUE}}
  \item{plot.hist}{whether to use \code{y_dots()} or \code{y_hist()}  for the continuous variables}
  \item{plot}{whether to plot}
  \item{hist}{whether histiogram or dot plot}
  \item{max.levels}{excludes from plotting bucket plots for variables with less than \code{max.levels}, distinct values}
   \item{title}{title of the plot}
    \item{percentage}{if set, i.e. 0.50, plots a portotion of data  otherwise for big data sets greater than 50.000 observartions it plots a porpotion}
    \item{seed}{the \code{set.seed()} argument}
    \item{print.info}{whether to print infomation when cutting the data usinf  \code{data_cut()}}
   \item{\dots}{other arguments}
}
\details{
The function \code{data_xyplot()} it takes a data frame and plot all the explanarory variables against the response.

The function \code{data_plot()} it takes a data frame and plot all variables against the response. The continuous are plotted using \code{y_dots()} or \code{y_hist()} while the factors and integer as bar plots.

}
\value{
Plots of the data
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
Mikis Stasinopoulos
}



\seealso{
\code{\link{data_names}}
}
\examples{
da <- rent99[,-2]
data_xyplot(da, rent)
data_plot(da)
y_zscores(da$rent)
data_response(da, response=rent)
}

\keyword{regression}

