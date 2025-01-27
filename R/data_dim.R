################################################################################
################################################################################
################################################################################
################################################################################
# DIM, NAMES 
# functions 
# 1)  data_dim()
# 2)  data_omit()
# 3)  data_which_na()
# 4)  data_names()
# 5)  data_shorter_names()
# 6)  data_rename()
################################################################################
################################################################################
################################################################################
################################################################################
# function 1
data_dim <- function(data)
{
  # what is the data
  if (is(data, "list"))  stop("the data is list  the function needs a data.frame") 
  if (is(data, "table")) 
    stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array")) 
    stop("the data is an array the function needs a data.frame") 
  # checking data 
  DIM <- dim(data)
  cat("**************************************************************",  "\n")  
  cat("**************************************************************",  "\n")  
  # dimensions  and missing   
  cat("the R class of the data is:",class(data)[1], "\n") 
  cat("the dimensions of the data are:",DIM[1],"by",DIM[2], "\n") 
  Data <- na.omit(data)
  DimOm <- dim(Data)
  dif1 <- DIM[1]-DimOm[1]
  dif2 <- DIM[2]-DimOm[2]
  cat("number of observations with missing values:", dif1, "\n" )   
  cat("% of NA's in the data:", (dif1/DIM[1])*100,"%", "\n")
  cat("**************************************************************",  "\n")
  cat("**************************************************************",  "\n") 
  invisible(data)  
} 
################################################################################
################################################################################
################################################################################
################################################################################ 
# function 2
data_omit <- function(data)
{
  # what is the data
  if (is(data, "list"))  stop("the data is list  the function needs a data.frame") 
  if (is(data, "table")) 
    stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array")) 
    stop("the data is an array the function needs a data.frame") 
  # checking data 
  DIM <- dim(data)
  cat("**************************************************************",  "\n")  
  cat("**************************************************************",  "\n")  
  # dimensions  and missing   
  cat("the R class of the data is:",class(data)[1], "\n") 
  cat("the dimensions of the data before omition are:",DIM[1],"x",DIM[2], "\n") 
  Data <- na.omit(data)
  DimOm <- dim(Data)
  dif1 <- DIM[1]-DimOm[1]
  dif2 <- DIM[2]-DimOm[2]
  cat("the dimensions of the data saved after omition are:",DimOm[1],"x",DimOm[2], "\n") 
  cat("the number of observations omited:", dif1, "\n" )   
  cat("**************************************************************",  "\n")
  cat("**************************************************************",  "\n") 
  invisible(Data)  
} 
################################################################################
################################################################################
################################################################################
################################################################################
# function 3
data_which_na <- function(data) 
{
  # what is the data
  if (is(data, "list"))  stop("the data is list  the function needs a data.frame") 
  if (is(data, "table")) stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data[1],"tible"))  data <- as.data.frame(data)
  if (is(data, "array")) stop("the data is an array the function needs a data.frame")  
  Names <- names(data)
  PP <- list()
  for (i in 1:length(Names))
  {
    PP[[i]] <- sum(is.na(data[,Names[i]]))
  }
  pp=unlist(PP)
  names(pp) <- Names
  print(pp)
  invisible(data) 
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 4
data_names <- function(data)
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
  # checking data 
  DIM <- dim(data)
  cat("**************************************************************",  "\n")
  cat("**************************************************************",  "\n")
  cat("the names of variables", "\n")
  Names <- names(data)
  print(Names)
  nCh <- nchar(Names)
  whichNames <- nCh>10
  if (any(whichNames))
  {
    cat("*********************************",  "\n")  
    cat("WARNING: the variables", "\n")
    cat(Names[whichNames], "\n")
    cat("have very long names","\n")
    cat("it is advisable to rename them using data_shorter_names()", "\n")  
  }
  cat("**************************************************************",  "\n")
  cat("**************************************************************",  "\n")
  invisible(data) 
}  
################################################################################
################################################################################
################################################################################
################################################################################
# function 5
data_shorter_names <- function(data, max=5, newnames)
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
  #cat("**************************************************************",  "\n")
  #cat("**************************************************************",  "\n")
  Names <- names(data)
  if (!missing(newnames)) 
  {
    if (length(newnames)!=length(Names)) 
      stop("the newdata should have the same length than the old one")
    names(data) <-  newnames
    
  }
  newnames <- Names
  for (i in 1:length(Names))  
  {
    if (nchar(Names[i])<max)
    {
      newnames[i] <- Names[i]  
    } else
    {
      pp  <- strsplit(Names[i], NULL)[[1]][1:max]
      newnames[i] <- paste0(pp, collapse ="")
    }     
  }
  names(data) <-  newnames
  #  cat("**************************************************************",  "\n")
  data_names(data)
  #  cat("**************************************************************",  "\n")
  invisible(data)
}
################################################################################
################################################################################
################################################################################
################################################################################
###############################
data_rename <- function(data, oldnames, newnames)
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
#cat("**************************************************************",  "\n")
#cat("**************************************************************",  "\n")
if (!is.character(oldnames)) stop("oldnames should be a character")
if (!is.character(newnames)) stop("oldnames should be a character")
if (length(oldnames)!=length(newnames))  stop("oldnames and newnames should have the same length")
     Names <- names(data)  
       pos <- match(oldnames, Names)
Names[pos] <- newnames
names(data) <- Names
invisible(data)
}
# END of DIM, NAMES functions 
################################################################################
################################################################################
################################################################################
################################################################################ 