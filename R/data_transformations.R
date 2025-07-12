################################################################################
################################################################################
# functions to check the data
################################################################################
################################################################################
# rm(list=ls())
# # checking for missing values
# function available
################################################################################
#  NOT THIS ONE 1) data_check       : will be fixed in the end
################################################################################   :
#  1) xy_Ptrans  :  takes x and y and find power best lambda in power 
#                 transformation
#  2) data_Ptrans_plot; 
#  3) data_Ptrans : it uses xy_Ptrans to fu=nd the best power transormation for 
#                   all continuous variables
#  4) y_factor; a factor to changed its reference level  
#  5) data_factor: all factors in data  changed reference level
#  6) time_dt2dh: take date-hours and create data and hours variables  
#  6) time_hour2num: takes time  and create a numeric 
#. 
################################################################################
################################################################################
################################################################################
################################################################################
# transformation functions
################################################################################
################################################################################
################################################################################
################################################################################
# function 1
# for find a single variable power transformation
xy_Ptrans <- function(x, y, data = NULL,  lim.trans = c(0, 1.5), prof=FALSE,
                     k=2,  c.crit = 0.01, step=0.1)
{
      cY <- class(y)
      cX <- class(x)
if (any( inherits(c(cY, cX),"numeric"))) return(NA)
  ptrans <- function(x, p) if (abs(p)<=0.0001) log(x) else I(x^p)
      fn <- function(p)  AIC(gam(y~s(I(ptrans(x,p)))), k=k)
       
if (prof) # profile dev
  {
     pp <- seq(lim.trans[1],lim.trans[2], step)
    pdev <- rep(0, length(pp))
    for (i in 1:length(pp))
    {
      pdev[i] <- fn(pp[i])
#   cat(pp[i], pdev[i], "\n")
    }
    Labx <-paste0("GAIC(", "k=", k, ")")
    plot(pdev~pp, type="l", ylab=Labx, xlab=expression(lambda))
    points(pdev~pp,col="blue")
    
    par <- pp[which.min(pdev)]
  } else
  {
     par <- optimise(fn, lower=lim.trans[1], upper=lim.trans[2])$minimum
  }
#    cat('*** power parameters ', par,"***"," \n")      
  invisible(par)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 2
# it tries 3 power transformations for all continuous variables
# non log  and sqrt  
data_Ptrans_plot <- function(data, response,
                             hist.col = "black",
                            hist.fill = "white",
                            dens.fill = "#FF6666",
                                alpha = 0.2,
                                 nrow = NULL,
                                 ncol = NULL,
                       plots.per.page = 9,
                           one.by.one = FALSE,
                          percentage, 
                                 seed = 123, 
                           print.info = TRUE, 
                            title,...)
{
  daTa <- mgcv <- NULL
if (is(data, "list"))
  stop("the data is list  the function needs a data.frame")
if (is(data, "table"))
  stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array")) stop("the data is an array the function needs a data.frame")
        Y <- deparse(substitute(response))
if (any(!(Y %in%names(data)))) stop("the response should be in data")
    Names <- names(data)
      pos <- match(Y, Names)
     daTa <- data[,-pos] # data without response
     daTa <- if (missing(percentage))
     {
            data_cut(daTa, seed=seed, print.info=print.info )
     } else data_cut(daTa,percentage=percentage, print.info=print.info)  
  class_Vars <- sapply(data,function(x) class(x)[1]) 
  daTa <- data_only_continuous(daTa) 
  #daTA <- daTa[,(inherits(class_Vars,"numeric")|inherits(class_Vars,"integer"))]
  # only numeric
  Namesnum <- names(daTa)
      daTa <- data[, c(Namesnum, Y) ]# new data with numeric + response
        PP <- list()
         I <- 1
for (i in Namesnum)
  {
        GG1 <-  daTa |> ggplot(aes(x = .data[[i]], .data[[Y]]))+geom_point()+
                ggtitle("no trans")
    PP[[I]] <- GG1
    I <- I + 1
    #  cat(I,"\n")
      GG2 <-  daTa |> ggplot(aes(x = sqrt(.data[[i]]), .data[[Y]]))+
               geom_point()+ggtitle("sqrt")
  PP[[I]] <- GG2
       I <- I + 1
#         cat(I,"\n")
     GG3 <-  daTa |> ggplot(aes(x = log(.data[[i]]), .data[[Y]]))+
               geom_point()+ggtitle("log")
PP[[I]] <- GG3
      I <- I + 1
    #           cat(I,"\n")
  }
  n.plots <- length(PP)
if (one.by.one)
  {
    oask <- devAskNewPage(one.by.one)
    on.exit(devAskNewPage(oask))
    for (i in 1:n.plots) print(PP[[i]])
  }
  else
  { # multiple plots
    #################################################################
    define_region <- function(row, col){
      viewport(layout.pos.row=row, layout.pos.col=col) }
    #################################################################
    if (n.plots>plots.per.page)
    {
      pages <- ceiling(n.plots/plots.per.page)
      page <- n.plots%/%plots.per.page
      ppp <- rep(plots.per.page,page)
      if (n.plots%%plots.per.page != 0) ppp <- c(ppp, n.plots%%plots.per.page)
      if (plots.per.page==9)
      {
        nc <- 3
        nr <- 3
        IJ <- expand.grid(j=1:nc, i=1:nr)
      } else
      {
        if (is.null(nrow)||is.null(nrow)) stop("the nrow and ncol need to be defined")
        if (plots.per.page> ncol*nrow) stop("the nrow or ncol has to increase")
        nc <- ncol
        nr <- nrow
        IJ <- expand.grid(j=1:nc, i=1:nr)
        IJ <- IJ[1:plots.per.page,]
      }
      start <- 1
      finish <- ppp[1]
      for (pa in 1:pages)
      {
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(nrow=nr,ncol=nc)))
        for (p  in start:finish)
        {
          print(PP[[p]], vp=define_region(IJ$i[p],IJ$j[p]))
        }
        start <- finish +1
        finish <- finish+ppp[pa+1]
        IJ <- rbind(IJ, IJ)
        oask <- devAskNewPage(ask=TRUE)
        on.exit(devAskNewPage(oask))
      }
    } else
    {
      pages <- 1
      ppp <- n.plots%/%pages
      nc  <- nr <- trunc(sqrt(ppp))
      if (nc < 1)        nr <- nc <- 1
      if (nc * nr < ppp) nc <- nc + 1
      if (nc * nr < ppp) nr <- nr + 1
      IJ <- expand.grid(j=1:nc, i=1:nr)
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(nrow=nr,ncol=nc)))
      for (p  in 1:n.plots)
      {
        print(PP[[p]], vp=define_region(IJ$i[p],IJ$j[p]))
      }
    }
  }
  on.exit( pushViewport(viewport(layout=grid.layout(nrow=1,ncol=1))))
  invisible(PP)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 3 #not finished yet
data_Ptrans <- function(data = NULL, response, 
                   lim.trans = c(0, 1.5), 
                        prof = FALSE,
                           k = 2,  
                  max.levels = 10,
                      c.crit = 0.01, 
                        step = 0.1,
                        seed = 123,
                  percentage)
{
  daTa <- mgcv <- NULL  
# what is the data
if (is(data, "list"))  stop("the data is list  the function needs a data.frame")
if (is(data, "table")) stop("the data is a table the function needs a data.frame")
if (is(data, "matrix")) data <- as.data.frame(data)
if (is(data[1],"mts"))  data <- as.data.frame(data)
if (is(data, "array")) stop("the data is an array the function needs a
                            data.frame")
        Y <- deparse(substitute(response))
if (any(!(Y %in%names(data)))) stop("the response should be in data")
       dv <- y_distinct(data[,Y])
if (dv < max.levels) stop("the response do not seems to have many distinct values")
        data <- if (missing(percentage))
        {
          data_cut(data,seed=seed)
        } else data_cut(data,percentage=percentage)    
       dimD <- dim(data)
# checking data  
if (is.null(dimD)) stop("only one variable in the data") 
if (dimD[1]<20)   stop(cat("the size of the data set is too small", "\n",
                             "to detect non-linear correlations", "\n"))  
       dat <- data_only_continuous(data)   
   #  da <- subset(dat, -Y)
#   sat.cont <- sapply(data,is.factor)|sapply(data,is.character)|
# data_distinct(data, get.distinct=TRUE) < min.distinct|
#     sapply(data, function(x) is(x, class2="Date"))
#       daTa <- subset(data,  select=!sat.cont)  
       Dim <- dim(dat)
if (Dim[2]==0) stop("no variable is left after taking out the factors")         
if (Dim[2]==1) stop("only one variable is left after taking out the factors")   
        pos <- match(Y, names(dat))
      nameS <- names(dat)[-pos]
#class_Vars <- sapply(daTa,function(x) class(x)[1]) 
        PP <- list()
for (i in 1:length(nameS))
  { # dv <- y_distinct(data[,Y])
    PP[[i]] <-  xy_Ptrans(dat[,nameS[i]], dat[,Y], lim.trans = lim.trans,
                          prof=FALSE,
                          k = k,  c.crit = c.crit, step = step )
}
  names(PP) <- nameS       
  PP 
}
################################################################################
################################################################################
################################################################################
################################################################################
# function  4
# it takes a factor and use the levels with lower (or higher) numbar 
# of observations as reference 
y_factor <- function(x, how = c("lower", "higher"))
{
how <- match.arg(how)  
if   (!is.factor(x)) stop("x is not a factor")
f <- if (how=="lower")  stats::relevel(x,ref=levels(x)[which.min(table(x))])
     else  relevel(x,ref=levels(x)[which.max(table(x))])
f
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# function 5
# It take a data frame and readjust the reference level of all 
#  factors in the data.frame to the level with "lower" or "higher" 
#  number of observations 
data_factor <- function(data, how = c("lower", "higher") )
{
 is_nominal <- function(x) class(x) %in% c("factor", "character")  
  how <- match.arg(how)    
  # what is the data
if (is(data, "list"))  stop("the data is list  the function needs a data.frame")
if (is(data, "table")) stop("the data is a table the function needs a data.frame")
if (is(data, "matrix")) data <- as.data.frame(data)
if (is(data[1],"mts"))  data <- as.data.frame(data)
if (is(data, "array")) stop("the data is an array the function needs a
                            data.frame")
  ind <- sapply(data, is_nominal)
  nam.fac <-  names(data)[ind]
for (i in nam.fac)
  {
  data[,i]  <-  y_factor(data[,i], how=how)  
  }
data
}  
################################################################################
################################################################################
################################################################################
################################################################################
# function 6
time_dt2dh <- function(datetime, format=NULL) 
{ 
            X <- t(as.data.frame(strsplit(datetime,' '))) 
  rownames(X) <- NULL 
  colnames(X) <- c("date", "time") 
         hour <- as.numeric(sub(":",".",X[,2])) 
         date <- as.Date(X[,1],format=format) 
  data.frame(date, hour) 
} 
################################################################################
################################################################################
################################################################################
################################################################################
# function 7 for time series
time_hour2num <- function(time, pattern=":")
{
    t <-  gsub(pattern, ".", time)
as.numeric(t)
}
################################################################################
################################################################################
################################################################################
################################################################################
# END of TIME functions
