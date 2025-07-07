################################################################################
################################################################################
################################################################################
################################################################################
# OUTLIERS function 
################################################################################
################################################################################ 
# function 1
y_outliers <- function(var, 
                      value, 
                     family = SHASHo, 
                       type = c("zscores","quantile"))
{
    ly <- length(var)
if (missing(value)) value <- abs(qnorm(1/(10*ly)))
if (is(var, "POSIXct")) var <- as.numeric(var) 
  type <- match.arg(type)
if (!is_numeric(var)) stop("the variable should be numeric")
if (any(var<0)) # if has negative values 
  {
    tvar <- var # do not look for power transformation 
  } else  # if only positive values look for transformation  
  {
    par  <- y_Ptrans(var)
    tvar <- if (abs(par) < 0.001) log(var) else var^par
  }  
if (type=="zscores")  
{
  z.scores <- y_zscores(tvar, family=family, plot=FALSE)
      iind <- which(abs(z.scores)>value)
  return(iind)
 } else 
 {
   out <-  gamlss.ggplots::y_dots(tvar, value=value, plot=FALSE) 
   return(out)
 }
} 
################################################################################
################################################################################
################################################################################
################################################################################
# function 
y_outliers_both <- function(var, 
                           value, 
                          family = SHASHo, 
                            method= c("intersect","union"))
{
  method<- match.arg(method)
    ly <- length(var)
if (missing(value)) value <- abs(qnorm(1/(10*ly)))  
  out1 <- y_outliers(var, value=value, type="quantile") 
  out2 <- y_outliers(var, value=value, family=family)  
   out <- if (method=="intersect") intersect(out1, out2) else  union(out1, out2) 
  out 
}  
################################################################################
################################################################################
################################################################################
################################################################################
# function 2
data_outliers <- function(data, 
                          value,
                   min.distinct = 50, 
                         family = SHASHo, 
                           type = c("zscores", "quantile")
                          )
{
  type <- match.arg(type)
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
   # data_distinct(data, get.distinct=TRUE) < min.distinct|
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
    ly <- length(daTa[,i])
if (missing(value)) value <- abs(qnorm(1/(10*ly)))  
    PP[[i]] <-  y_outliers(daTa[,i], value=value, family=family, type=type)
  }
if (!length(PP)==0)  names(PP) <- Names       
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
# then it fits the SHASH (It was suggested by  Bob)
y_Ptrans <- function(x, lim.trans = c(0, 1.5))
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
  # PP <-  moment_bucket(ptrans(x,par),x, text_to_show=c("+","*"))
  # if (bucket) print(PP)
  # 1-pchisq(momentSK(ptrans(x,par))$jarque.bera.test, 2)
  # 
  # 1-pchisq(momentSK(ptrans(x,1))$jarque.bera.test, 2)
  # hist(ptrans(x,par))
  #  hist(ptrans(x,1))
  #  momentSK(ptrans(x,1))$jarque.bera.test
#  cat('*** power parameters ', par,"***"," \n")
par
}
################################################################################
################################################################################
################################################################################
################################################################################
y_outliers_by <- function(var, 
                       fact,# a factor whicxh partition the data
                       family = SHASHo, 
                       type = c("zscores","quantile"))
{
if(!is(fact, "factor")) stop("the secong argument should br a factor")
       ly <- length(var)
       lx <- length(fact)
     type <- match.arg(type)
if (ly!=lx) stop("the y and factor should have the some length")
if (missing(fact)) stop("the factor should be set") 
  llev <- as.numeric(tapply(fact, fact, length))
 value <-abs(qnorm(1/(10*llev)))
if (is(var, "POSIXct")) var <- as.numeric(var)  
if (!is_numeric(var)) stop("the variable should be numeric")
# the problem is integer is numeric so do not stop them
# if (lb==1) fxvar <- cut_number(xvar, n = breaks) else
#   { 
# if (min(xvar) < breaks[1]||max(xvar) > breaks[lb]) stop("The end intervals are not correct")
#   fxvar <- cut(xvar, breaks=breaks)
# }
 # quantile(xvar, probs=seq(0,1, 0.25))
 # fxvar <- cut(xvar, breaks=breaks, include.lowest=FALSE) 
        ll <- levels(fact)
        nl <- nlevels(fact)
        ix <- seq(1,lx)
      iind <- list()  
  z.scores <- rep(0, lx)
if (any(var<0)) # if has negative values 
  {
    tvar <- var # do not look for power transformation 
  } else  # if only positive values look for transformation  
  {
    par  <- y_Ptrans(var)
    tvar <- if (abs(par) < 0.001) log(var) else var^par
  } 
 z <- list()  
for (i in 1:nl)
{
  z[[i]] <- y_outliers(var[fact==ll[i]], family=family, value=value[i], type=type) 
}
 # z <- tapply(var, fact, FUN=y_zscores, family=family, plot=FALSE, value=value, type=type)
  Z <- unlist(z)
  X <- abs(Z)>value
#pp <- tapply(X, fact, FUN=\(x){ix[x]})
  p <- ix[X]
  names(p) <-fact[X]
return(p)
} 
################################################################################
################################################################################
################################################################################
################################################################################
