# removing data functions
################################################################################
# function 1; data_rm1val()
# function 2;  data_rm(data, vars)
# function 3; data_exclude_class(data, class.out="factor")
# function 4: data_continuous(data)
# function 5: data_continuous(data)
# function 6: data_classes()  (secrete)
################################################################################
################################################################################
################################################################################
################################################################################
# function 1 removing variables with only one value
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
# function 3
data_exclude_class <- function(data, class.out="factor")
{
if (is(data, "list"))
    stop("the data is list  the function needs a data.frame")
if (is(data, "table"))
    stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array"))
    stop("the data is an array the function needs a data.frame")
  CData <- sapply(data,function(x) class(x)[1])
     da <- subset(data, select=CData!=class.out)
invisible(da)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 4
# get only the continuous variables in the data set
data_continuous <- function(data)
{
if (is(data, "list"))  stop("the data is list  the function needs a data.frame")
if (is(data, "table"))
    stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array"))
    stop("the data is an array the function needs a data.frame")
#         Y <-  deparse(substitute(response))
#if (any(!(Y %in%names(data)))) stop("the response should be in data")
     Names <- names(data)
     #  pos <- match(Y, Names)
     # daTa <- data[,-pos] # data without response
class_Vars <-  sapply(data,function(x) class(x)[1])
      data <- data[,(class_Vars=="numeric")|(class_Vars=="integer")]
      # only numeric
  invisible(data)
}
################################################################################
################################################################################
################################################################################
################################################################################
data_classes <- function(df) 
  {
  t(
    as.data.frame(
      lapply(df, function(x) paste(class(x)[1], collapse = ','))
                 )
   )
}
#purrr::map_df(rent, class)
#as.data.frame(purrr::map_chr(rent, class))
#unlist(lapply(rent, function(x) class(x)))
################################################################################
################################################################################
################################################################################
################################################################################
# removes specified variables
data_select <- function(data, vars)
{
  if (is(data, "list"))
    stop("the data is list  the function needs a data.frame")
  if (is(data, "table"))
    stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array"))
    stop("the data is an array the function needs a data.frame")
  #browser()
  if (is.character(vars)) 
  {
    da <- subset(data, select=vars)
  } else
  {
    da <- subset(data, select=match(1:dim(data)[2], vars))
  }
  invisible(da)
}