\name{data_part}
\alias{data_part}
\alias{data_cut}
\alias{data_part_list}


\title{
A function to partition a data frame
}
\description{
The function \code{data_part()} takes a data set and creates a new  identical data set with an extra factor called \code{partition} which  can be used to allocate data in different data sets. 

i) if \code{partition=2L} the factor has two levels \code{train}, and \code{test}.

ii)  if  \code{partition=3L} the factror  has three levels  \code{train}, for training data, \code{val} for validation data  and \code{test} for test data.


 iii) if  \code{partition > 4L} say \code{K} then the levela are "1", "2"..."K". The factor then cen be used for  \code{K} cross validation sets.
 
the function \code{data_part_list()} in does similar things like the function \code{data_part()} but instead of adding a factor to the data creates a \code{list} with ellements the training, test and validation data sets. Note that this function do not allow k-fold cross-validation or bootsrapping indexing.   
 
 
The function \code{data_cut()} takes a \code{data.frame} and selects randomly specified \% of the data. For \code{data.frames} with more than 50.000 observations is automatically select part of the data. The function  
\code{data_cut()} is used a lot in the plotting routines  to make sure that plots ate not take for ever.

 

}
\usage{
data_part(data, partition = 2L, probs, setseed = 123, ...)

data_part_list(data, partition = 2, probs, setseed = 123, ...)

data_cut(data, percentage, seed = 123)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a \code{data.frame}}
  \item{partition}{2, 3 or a number less than 20}
  \item{probs}{probabilities for the random selection}
  \item{setseed}{setting the sead so the proccess can be repeated}
    \item{percentage}{The percentage of data to keep. If set, i.e. \code{percentage=0.5} only 50\% are kept otherwise for large data set (more that 50.000) only percentage of data are kept. }
  \item{seed}{the \code{set.seed()} argument}
  \item{\dots}{extra arguments}
}

\value{
Both function produce a data frame. The function \code{data_part()} adds a factor  \code{partition} while \code{data_rm1val()}  removes variable with only one value.
}
\references{
Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}
\author{Mikis Stasinopoulos, Bob Rigby and Fernanda De Bastiani}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{data_str}}
}
\examples{

da <- data_part(rent)
head(da)
mosaicplot(table(da$partition))

da.train <- subset(da, partition=="train")
da.test <- subset(da, partition=="test")
dim(da.train)
dim(da.test)


allda <-  data_part_list(rent) 
dim(allda[[1]]) # training data
dim(allda[[2]]) # test data
}

\keyword{regression}
