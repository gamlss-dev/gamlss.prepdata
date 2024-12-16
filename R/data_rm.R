# removing data functions
################################################################################
# function 2 removing variables with only one value
################################################################################
################################################################################
################################################################################
################################################################################
#remove factors with only one level 
data_rm1val <- function(data)
{
  # what is the data
if (is(data, "list"))
  stop("the data is list  the function needs a data.frame")
if (is(data, "table"))
  stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array"))
  stop("the data is an array the function needs a data.frame")
    Names <- names(data)
       PP <- list()
for (i in 1:length(Names))
  {
  PP[[i]] <-  length(table(data[,Names[i]]))
  if( class(data[,Names[i]])[1]=="factor")  
  {
  PP[[i]] <- length(table(data[,Names[i]])[table(data[,Names[i]])!=0])
  } 
}
       pp <- unlist(PP)
names(pp) <- Names
if (any(pp==1)) 
  {
   w1val <-  which(pp==1)
 removed <- names(data)[w1val]
  data[,w1val] <- NULL
  cat("Removed the variable(s)", removed, "\n")
  }
  invisible(data)
}
################################################################################
################################################################################
################################################################################
################################################################################
# removes specified variables
data_rm <- function(data, vars)
{
if (is(data, "list"))
    stop("the data is list  the function needs a data.frame")
if (is(data, "table"))
    stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array"))
    stop("the data is an array the function needs a data.frame")
if (is.character(vars))
  {
    da <- subset(data, select=setdiff(names(data), vars))
  } else
  {
    da <- subset(data, select=setdiff(1:dim(data)[2], vars))
  }
  invisible(da)
}
################################################################################
################################################################################
################################################################################
################################################################################
# exclude columns  belonging to a specified class
data_exclude_class <- function(data, class="factor")
{
if (is(data, "list"))
    stop("the data is list  the function needs a data.frame")
if (is(data, "table"))
    stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array"))
    stop("the data is an array the function needs a data.frame")
  CData <- sapply(data,class)
     da <- subset(data, select=CData!=class)
invisible(da)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function
# get only the continuous variables in the data set
data_continuous <- function(data, response)
{
if (is(data, "list"))  stop("the data is list  the function needs a data.frame")
if (is(data, "table"))
    stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array"))
    stop("the data is an array the function needs a data.frame")
         Y <-  deparse(substitute(response))
if (any(!(Y %in%names(data)))) stop("the response should be in data")
     Names <- names(data)
       pos <- match(Y, Names)
      daTa <- data[,-pos] # data without response
class_Vars <- sapply(daTa,class)
      daTa <- daTa[,(class_Vars=="numeric")|(class_Vars=="integer")]
      # only numeric
  invisible(daTa)
}
################################################################################
################################################################################
################################################################################
################################################################################
