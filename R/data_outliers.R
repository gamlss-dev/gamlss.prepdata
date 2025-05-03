################################################################################
################################################################################
################################################################################
################################################################################
# OUTLIERS function 
################################################################################
################################################################################ 
# function 1
y_outliers <- function(var, value=4, family=SHASHo)
{
  if (!is(var,"numeric")) stop("the variable should be numeric")
  # the problem is integer is numeric so do not stop them
  if (any(var<0)) # if has negative values 
  {
    tvar <- var # do not look for power transformation 
  } else  # if only positive values look for tran=foarmation  
  {
    par  <- x_Ptrans(var)
    tvar <- if (abs(par) < 0.001) log(var) else var^par
  }  
  z.scores <- y_zscores(tvar, family=family, plot=FALSE)
      ival <- var[which(abs(z.scores)>value)]
      iind <- which(abs(z.scores)>value)
  names(ival) <- iind
  ival
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 2
data_outliers <- function(data, 
                          value = 4,
                          min.distinct = 50, 
                          family = SHASHo
                          )
{
  # what is the data
  if (is(data, "list"))  stop("the data is list  the function needs a data.frame")
  if (is(data, "table")) stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix")) data <- as.data.frame(data)
  if (is(data[1],"mts"))  data <- as.data.frame(data)
  if (is(data, "array")) stop("the data is an array the function needs a
                            data.frame")
  dimD <- dim(data)
  # checking data  
  if (is.null(dimD)) stop("only one variable in the data") 
  if (dimD[1]<20)   stop(cat("the size of the data set is too small", "\n",
                             "to detect non-linear correlations", "\n"))  
  sat.cont <- sapply(data,is.factor)|sapply(data,is.character)|
    data_distinct(data, get.distinct=TRUE) < min.distinct|
    sapply(data, function(x) is(x, class2="Date"))
  daTa <- subset(data,  select=!sat.cont)  
  #daTa <- subset(data,  select=ifelse(sapply(data,is.factor)|
  #            sapply(data,is.character)==TRUE, FALSE, TRUE))
  Dim <- dim(daTa)
  if (Dim[2]==0) stop("no variable is left after taking out the factors")         
  if (Dim[2]==1) stop("only one variable is left after taking out the factors")   
  Names <- names(daTa)
  class_Vars <- sapply(daTa,function(x) class(x)[1]) 
  PP <- list()
  for (i in 1:length(class_Vars))
  { 
    PP[[i]] <-  y_outliers(daTa[,i], value=value, family=family)
  }
  names(PP) <- Names       
   PP 
}
################################################################################
################################################################################
################################################################################
################################################################################
# functions 3 
# this is a hidden function only used by y_outliers
# it takes positive variables and check if they have high Jarque-Bera value
# Note that the  Jarque-Bera  test statistic is always non-negative. 
# If it is far from zero, it signals the data do not have a normal distribution.
# I think the idea here is that if the variable is positive it is transformed 
# to be make to have a less skew-kurtotic distribution 
# then it fits the SHASH (It wa suggested by  Bob)
# This prevent very highly observations in the right tail to be   
# 
x_Ptrans <- function(x, lim.trans = c(0, 1.5),  prof=FALSE,
                     step=0.01,    bucket=FALSE)
{
if (length(lim.trans)!=2) stop(" the limits of  p are not set properly")
  #  cat("*** Checking for transformation for x ***", "\n")
  cX <- class(x)
  if ((cX!="numeric")&(cX!="integer")) return(NA)
# define the functions 
  ptrans <- function(x, p) if (abs(p)<=0.001) log(x) else I(x^p)
      fn <- function(p) momentSK(ptrans(x,p))$jarque.bera.test

# if (prof) # profile dev
#   {
#     pp <- seq(lim.trans[1],lim.trans[2], step)
#     pdev <- rep(0, length(pp))
#     for (i in 1:length(pp))
#     {
#       pdev[i] <- fn(pp[i])
#       #   cat(pp[i], pdev[i], "\n")
#     }
#     GG <- ggplot(data.frame(pdev,pp), aes(x=pp, y=pdev))+
#       geom_point()+
#       geom_line()
#     #  points(pdev~pp,col="blue")
#     par <- pp[which.min(pdev)]
#     print(GG)
#     cat('*** power parameters ', par,"***"," \n")
#   } else
#   {
    par <- optimise(fn, lower=lim.trans[1], upper=lim.trans[2])$minimum
    # cat('*** power parameters ', par,"***"," \n")
  # }
  PP <-  moment_bucket(ptrans(x,par),x, text_to_show=c("+","*"))
  if (bucket) print(PP)
  # 1-pchisq(momentSK(ptrans(x,par))$jarque.bera.test, 2)
  # 
  # 1-pchisq(momentSK(ptrans(x,1))$jarque.bera.test, 2)
  # hist(ptrans(x,par))
  #  hist(ptrans(x,1))
  #  momentSK(ptrans(x,1))$jarque.bera.test
#  cat('*** power parameters ', par,"***"," \n")
  invisible(par)
  
}
################################################################################
################################################################################
################################################################################
################################################################################