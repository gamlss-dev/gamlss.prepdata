################################################################################
################################################################################
################################################################################
################################################################################
# OUTLIERS function 
################################################################################
################################################################################ 
# function 1
y_outliers <- function(x, 
                      value, 
                     family = SHASHo, 
                       type = c("zscores","quantile"),
                     transform=TRUE)
{
    ly <- length(x)
  type <- match.arg(type)
if (missing(value)) value <- abs(qnorm(1/(10*ly)))
if (is(x, "POSIXct")) x <- as.numeric(x) 
if (!is_numeric(x)) stop("the x variable should be numeric")
if (transform)
{
  if (any(x<0)) # if has negative values 
  {
    x <- x # do not look for power transformation 
  } else  # if only positive values look for transformation  
  {
    par  <- y_Ptrans(x)
    x <- if (abs(par) < 0.001) log(x) else x^par
    cat("the x was tansformed using the power", par, "\n") 
  }    
}
  names(x) <- 1:ly  
if (type=="zscores")  
{
  z.scores <- y_zscores(x, family=family, plot=FALSE)
      iind <- which(abs(z.scores)>value)
  return(iind)
 } else 
 {
   out <-  gamlss.ggplots::y_dots(x, value=value, plot=FALSE) 
   return(out)
 }
} 
################################################################################
################################################################################
################################################################################
################################################################################
# function 
y_outliers_both <- function(x, 
                           value, 
                          family = SHASHo, 
                            method= c("intersect","union"))
{
  method<- match.arg(method)
    ly <- length(x)
if (missing(value)) value <- abs(qnorm(1/(10*ly)))  
  out1 <- y_outliers(x, value=value, type="quantile") 
  out2 <- y_outliers(x, value=value, family=family)  
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
y_outliers_by <- function(x, 
                       by,# a factor which partition the data
                       family = SHASHo, 
                       type = c("zscores","quantile"))
{
if(!is(by, "factor")) stop("the secong argument should br a factor")
       ly <- length(x)
       lx <- length(by)
     type <- match.arg(type)
if (ly!=lx) stop("the y and factor should have the some length")
if (missing(by)) stop("the factor should be set") 
  llev <- as.numeric(tapply(by, by, length))
 value <- abs(qnorm(1/(10*llev)))
if (is(x, "POSIXct")) x <- as.numeric(x)  
if (!is_numeric(x)) stop("the variable should be numeric")
# the problem is integer is numeric so do not stop them
# if (lb==1) fxvar <- cut_number(xvar, n = breaks) else
#   { 
# if (min(xvar) < breaks[1]||max(xvar) > breaks[lb]) stop("The end intervals are not correct")
#   fxvar <- cut(xvar, breaks=breaks)
# }
 # quantile(xvar, probs=seq(0,1, 0.25))
 # fxvar <- cut(xvar, breaks=breaks, include.lowest=FALSE) 
        ll <- levels(by)
        nl <- nlevels(by)
        ix <- seq_len(lx)
      iind <- list()  
  z.scores <- rep(0, lx)
if (any(x<0)) # if has negative values 
  {
    tvar <- x # do not look for power transformation 
  } else  # if only positive values look for transformation  
  {
    par  <- y_Ptrans(x)
    tvar <- if (abs(par) < 0.001) log(x) else x^par
  } 
 z <- list()  
 names(x)<- seq_len(lx)
for (i in 1:nl)
{
  z[[i]] <- y_outliers(x[by==ll[i]], family=family, value=value[i], type=type, transform=FALSE) 
  z[[i]] <- as.numeric(names(z[[i]]))
}
 # z <- tapply(var, fact, FUN=y_zscores, family=family, plot=FALSE, value=value, type=type)
  Z <- unlist(z)
  #X <- abs(Z)>value
#pp <- tapply(X, fact, FUN=\(x){ix[x]})
#  p <- ix[X]
  names(z) <- levels(by)
return(z)
} 
################################################################################
################################################################################
################################################################################
################################################################################
y_outliers_loop <- function(x, 
                       value, 
                       family = SHASHo, 
                       type = c("zscores","quantile"),
                       transform=TRUE)
{
      ly <- length(x)
    type <- match.arg(type)
if (missing(value)) value <- abs(qnorm(1/(10*ly)))
if (is(var, "POSIXct")) var <- as.numeric(var) 
if (!is_numeric(x)) stop("the x variable should be numeric")
if (transform)
  {
        if (any(x<0)) # if has negative values 
        {
          x <- x # do not look for power transformation 
        } else  # if only positive values look for transformation  
        {
          par  <- y_Ptrans(x)
          x <- if (abs(par) < 0.001) log(x) else x^par
          cat("the x was tansformed using the power", par, "\n") 
        }    
  }
      names(x) <- seq_len(ly)  
if (type=="zscores")  
  {
     index_ <- 1
    w_index <- rep(1, length(x))
    l_index <- 0
while (index_)
    {
    z.scores <- y_zscores(x,weights=w_index, family=family, plot=FALSE)
     i_index <- as.numeric(names(which(abs(z.scores)>value))) 
  #   cat("i=", i_index,"\n")
w_index[i_index] <- 0
      index_ <- length(i_index)
    # l_index <- length(i_index)
   #   cat("i=",index_,"\n")
    }  
    return(which(w_index==0))
  } else 
  {
    out <-  gamlss.ggplots::y_dots(x, value=value, plot=FALSE) 
    out
  }
} 
################################################################################
################################################################################
################################################################################
################################################################################
y_outliers_z <- function(x,
                         by, # a factor which portioned the data 
                         value, 
                       family = SHASHo, 
                    transform = TRUE, 
                         loop = FALSE)
{
################################################################################  
       ly <- length(x)
if (!missing(by)&&!is(by, "factor")) stop("the by argument should br a factor")
if (is(x, "POSIXct")) x <- as.numeric(x) 
if (!is_numeric(x)) stop("the x variable should be numeric")
################################################################################
if (transform)
 {
    if (any(x<0)) # if has negative values 
    {
      x <- x # do not look for power transformation 
    } else  # if only positive values look for transformation  
    {
      par  <- y_Ptrans(x)                              # get parameter 
         x <- if (abs(par) < 0.001) log(x) else x^par  # get transormed x
  cat("the x was tansformed using the power", par, "\n") 
    }    
}
##################### NO LOOP ##################################################
if (missing(by))
{
  if (missing(value)) value <- abs(qnorm(1/(10*ly)))  
if (loop==FALSE)
 {
  names(x) <- seq_len(ly)  
  z.scores <- y_zscores(x, family=family, plot=FALSE)
  iind <- which(abs(z.scores)>value)
  return(iind) #######END NO LOOP ##############################################
 }  else 
 { ####################  START LOOP ############################################
   index_ <- 1
  w_index <- rep(1, length(x))
  l_index <- 0
while (index_)
  {
    z.scores <- y_zscores(x,weights=w_index, family=family, plot=FALSE)
     i_index <- as.numeric(names(which(abs(z.scores)>value))) 
   #   cat("i=", i_index,"\n")
w_index[i_index] <- 0
     index_ <- length(i_index)
  #  cat("i=",index_,"\n")
  }  
  return(which(w_index==0))  
 }############################# END LOOP #######################################
} else ######################## END BY MISSING #################################
{############################## START BY #######################################
        lx <- length(by)
if (ly!=lx) stop("the y and factor should have the some length")
      llev <- as.numeric(tapply(by, by, length))
     value <- abs(qnorm(1/(10*llev)))
if (loop==FALSE)
{############################## NO LOOP ######################################## 
      ll <- levels(by)
      nl <- nlevels(by)
      ix <- seq_len(lx)
       z <- list()  
names(x) <- seq_len(lx)
   Names <- tapply(x,by,names)
for (i in 1:nl)
{
       y <- x[by==ll[i]] 
names(y) <- Names[[i]]
  z[[i]] <- y_outliers(y, family=family, value=value[i], 
                                    transform=FALSE) 
  z[[i]] <-  as.integer( names(y)[z[[i]]])
}
names(z) <- levels(by)
return(z)
}############################## END NO LOOP ####################################
{############################## begging LOOP ###################################
  ll <- levels(by)
  nl <- nlevels(by)
  z.scores <- rep(0, lx)
  names(x) <- seq_len(lx)
  Names <- tapply(x,by,names)
  
  z <- list()
  for (i in 1:nl)
  {
    y <- x[by==ll[i]] 
    names(y) <- Names[[i]]
    z[[i]] <- y_outliers_loop(y, family=family, value=value[i], 
                         transform=FALSE) 
    z[[i]] <- as.integer( names(y)[z[[i]]])
  }
  names(z) <- levels(by)
  return(z)  
}############################## END LOOP #######################################
}############################## END BY #########################################  

}######################### END OF FUNCTION ##################################### 
################################################################################
################################################################################
################################################################################
################################################################################