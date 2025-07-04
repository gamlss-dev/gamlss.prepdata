################################################################################
################################################################################
# functions to check the data
################################################################################
################################################################################
################################################################################
################################################################################
#  1) y_distinct
#  2) data_distinct
#  3) data_str         : print the structure of the data
#  4) data_cha2fac     : transform character to factors (if levels are small)
#  5) data_few2fac     : takes any variable with few disctict values and make
#                        into a factor
#. 6) data_int2num     : take all integers with greater than 50 distinct values 
#. 7) data_fac2num     : the variable are specifies using vars
################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################
################################################################################
# STRUCTRURE functions
################################################################################
################################################################################
# function 1
y_distinct <- function(var)
{
  PP <- length(table(var))
  PP
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 2
data_distinct <- function(data, get.distinct=FALSE, print=TRUE )
{
  # what is the data
  if (is(data, "list"))  stop("the data is list  the function needs a data.frame")
  if (is(data, "table")) stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array")) stop("the data is an array the function needs a data.frame")
  Names <- names(data)
  PP <- list()
  for (i in 1:length(Names))
  {
    PP[[i]] <- length(table(data[,Names[i]]))
  }
  pp=unlist(PP)
  names(pp) <- Names
  if (any(pp==1)) warning("at least one variable has only 1 distinct value, please remove from data")
 if (print) print(pp)
  if (get.distinct) return(pp) else invisible(data)
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# function 3
data_str <- function(data, min.values=100, min.levels=10)
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
cat("**************************************************************",  "\n")
cat("**************************************************************",  "\n")
cat("the structure of the data", "\n")
    str(data)
         Names <- names(data)
            
    class_Vars <- sapply(data,function(x) class(x)[1]) 
  #  for (i in 1:length(class_Vars)) class_Vars[[i]] <- class_Vars[[i]][1]
cat("**************************************************************",  "\n")
cat("**************************************************************",  "\n")
cat("table of the class of variabes", "\n")
  tab <- table(unlist(class_Vars))
print(tab)
cat("**************************************************************",  "\n")
cat("**************************************************************",  "\n")
       nCh <- nchar(Names)
cat("distinct values in variables","\n")
      dist <- data_distinct(data, get.distinct=TRUE)
       out <- data.frame(class=class_Vars,  dist.values=dist, name.no.ch=nCh)
      Iint <- inherits(out$class,"integer")  &out$dist.values < min.levels
      Icha <- inherits(out$class,"character")&out$dist.values < min.levels
      Inum <- inherits(out$class,"numeric")  &out$dist.values < min.levels
      Ivar <- inherits(out$class,"integer")  &out$dist.values > min.values
# cat("**************************************************************",  "\n")
# cat("**************************************************************",  "\n")
if (any(Iint=="TRUE"))
{
  cat("consider to make those integer vectors into factors:", "\n")
  cat(Names[Iint], "\n" )
}
if (any(Icha=="TRUE"))
{
  cat("consider to make those characters vectors into factors:", "\n")
  cat(Names[Icha], "\n" )
}
if (any(Inum=="TRUE"))
{
  cat("consider to make those numeric vectors into factors:", "\n")
  cat(Names[Inum], "\n" )
}
if (any(Ivar=="TRUE"))
{
  cat("and those integer vectors into mumeric:", "\n")
  cat(Names[Ivar], "\n" )
}
cat("**************************************************************",  "\n")
cat("**************************************************************",  "\n")
invisible(data)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 4
# turning character vectors to factors  if  number of district values
# is less than 20
data_cha2fac <- function(data, show.str=FALSE)
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
if (is(data,"tbl_df")|is(data, "tbl")) 
{
  data <- data.frame(data)
  warning("the tibble was transform to data.frame")
}
# checking data
cat("**************************************************************",  "\n")
       Names <- names(data)
  class_Vars <- sapply(data,function(x) class(x)[1]) 
   which_cha <- class_Vars=="character"
     all_Cha <- Names[which_cha]
         ind <- length(all_Cha)
if (ind==0)
  {
  cat("not character vector was found", "\n")
  return(invisible(data))
  }
for (i in 1:ind)
 {
#if   (dval[i] < min.levels)
 # cat("class:", class_Vars[i], "\n"  )    
  data[,all_Cha[i]] <- factor(data[,all_Cha[i]])
}
if (show.str)
{
  str(data)
cat("**************************************************************",  "\n")
}
if (ind==1)
{
  cat(ind,"character vector transformed to factor", "\n")
}else
{
    cat(ind,"character vectors transformed to factors", "\n")
}
cat("**************************************************************",  "\n")
invisible(data)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 5
data_few2fac <- function(data, max.levels=5, show.str=FALSE)
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
if (is(data, "tbl_df")|is(data, "tbl")) 
  {
  data<- data.frame(data)
  
  warning("the tibble was transform to data.frame")
}
# checking data
cat("**************************************************************",  "\n")
       Names <- names(data)
  class_Vars <- sapply(data,function(x) class(x)[1]) 
      dvdata <- data_distinct(data, get.distinct=TRUE)
         ind <- 0
for (i in 1:length(Names))
  {
    if (dvdata[i] <= max.levels)
    {
  data[,Names[i]] <- factor(data[,Names[i]])
              ind <- ind + 1
    }
}
if ( show.str)
{
cat("**************************************************************",  "\n")
  str(data)
}
cat("**************************************************************",  "\n")
if (ind==1)
{
cat(ind,"vector with fewer number of values than",  max.levels,"was transformed to factor", "\n")
} else
{
cat(ind,"vectors with fewer number of values than",  max.levels,"were transformed to factors", "\n")
}
cat("**************************************************************",  "\n")
cat("**************************************************************",  "\n")
  invisible(data)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 6
data_int2num <- function(data,  min.values=50, show.str=FALSE)
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
cat("**************************************************************",  "\n")
      Names <- names(data)
 class_Vars <- sapply(data,function(x) class(x)[1]) 
  which_Int <- class_Vars=="integer"
if (!any(which_Int))  return(cat("no integer vector is found", "\n"))
    dis.val <- data_distinct(data, get.distinct=TRUE)
    all_Int <- Names[which_Int]
    dis.val <- dis.val[which_Int]
        ind <- 0
for (i in 1:length(all_Int))
  {
    if (dis.val[i] >  min.values)
    {
      data[,all_Int[i]] <- as.numeric(data[,all_Int[i]])
      ind <- ind+1
    }
  }
if (show.str)
{
  str(data)
cat("**************************************************************",  "\n")
}
if (ind==1)
{
cat(ind,"integer vector with more number of values than", min.values,"was transformed to numeric", "\n")
} else
{
cat(ind,"integer vectors with more number of values than", min.values,"were transformed to numeric", "\n")
}
cat("**************************************************************",  "\n")
invisible(data)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 7
data_fac2num <- function(data, vars) # or as character or numbers
{
if (is(data, "list"))
    stop("the data is list  the function needs a data.frame")
if (is(data, "table"))
    stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array"))
    stop("the data is an array the function needs a data.frame")
class_AVars <-  sapply(data,function(x) class(x)[1])
class_SVars <-  sapply(data[,vars],function(x) class(x)[1])
for (i in 1:length(vars))
{
  if (class_SVars[1]=="character"||class_SVars[1]=="factor")
    data[,vars[i]] <- as.numeric(data[,vars[i]])
  else 
    stop( "At least one of the vars is not character or factor")
}  
data <- data_omit(data)
invisible(data)
}
# data_fac2var  <-  function(data, vars)
################################################################################
################################################################################
################################################################################
################################################################################
# end of STR functions
################################################################################
################################################################################
################################################################################
################################################################################
