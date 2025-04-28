\name{data_part}
\alias{data_part}
\alias{data_cut}
\alias{data_part_list}
\alias{data_Kfold_index}
\alias{data_Kfold_weights}
\alias{data_boot_index}
\alias{data_boot_weights}

\title{
A function to partition a data frame
}
\description{
The function \code{data_part()} takes a \code{data.frame} and creates a new  identical  \code{data.frame} with an extra factor called \code{partition} which  can be used to allocate data in different data sets. 

i) if the option of the function is \code{partition=2L} the factor has two levels \code{train}, and \code{test}.

ii)  if the option of the function is \code{partition=3L} the factror  has three levels  \code{train}, for training data, \code{val} for validation data  and \code{test} for test data.

 iii) if  the option of the function is \code{partition > 4L} say \code{K} then the levels of the factor are "1", "2"..."K". The factor then can be used to identify \code{K-fold} cross validation sets (up to K=20). 


 
The function \code{data_part_list()} in parform a similar  task like the function \code{data_part()} but instead of adding a factor to the data creates a \code{list} with ellements \code{data.frame}s. Note that this function do  allow K-fold cross-validation creation (up to 20 K-folds), see also the function \code{data_Kfold()}.   
 


The function \code{data_boot_index()} takes a \code{data.frame} and produces two list of length \code{K},  the in-bag list, \code{IB}, and the out-of-bag list, \code{OOB}.


The function \code{data_boot_weights()} takes a \code{data.frame} and produces  a matrix of dimensions  \code{n x K} which column of wich  can be used as prior weight in a regression situation.

The function \code{data_Kfold()} takes a \code{data.frame} and produces a matrix of indeces which then can be used to fit diffetent sections of the data for cross validation.


 
 
 
 
The function \code{data_cut()} takes a \code{data.frame} and selects randomly specified \% of the data. It is usually applied to graphical function =to reduce time for plotting; For \code{data.frame}s with more than 50.000 observations is automatically select part of the data. 

 

}
\usage{
data_part(data, partition = 2L, probs, setseed = 123, ...)

data_part_list(data, partition = 2L, probs, setseed = 123, ...)

data_boot_index(data, B = 10, setseed = 123)

data_boot_weights()(data, B = 10, setseed = 123)

data_Kfold_index()(data, K = 10, setseed = 123)

data_Kfold_weights()(data, K = 10, setseed = 123)

data_cut(data, percentage, seed = 123)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a \code{data.frame}}
  \item{partition}{2, 3 or a number less than 20}
   \item{K}{the number of partitions, (maximum 20 for CV)}
   \item{B}{the number of bootstrap samples}
  \item{probs}{probabilities for the random selection}
  \item{setseed}{setting the sead so the proccess can be repeated}
    \item{percentage}{The percentage of data to keep. If set, i.e. \code{percentage=0.5} only 50\% are kept otherwise for large data set (more that 50.000) only percentage of data are kept. }
  \item{seed}{the \code{set.seed()} argument}
  \item{\dots}{extra arguments}
}

\value{
The  functions \code{data_part()}, \code{data_part_list()}, \code{data_boot_index()},  \code{data_boot_weights()},  \code{data_Kfold()}  produce a \code{data.frame}, lists or matrices to be later used  for data partition during the fitting process. The function \code{data_cut()} randomly select  part of the data.
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
