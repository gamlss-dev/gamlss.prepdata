################################################################################
################################################################################
################################################################################
################################################################################
# permutation test for correlations
################################################################################
################################################################################
################################################################################
################################################################################
# it take two variables 
# it creates a data matrix Da with  v =v(x,y) and factor f
#  for i=1 to B
#require(ggplot2)
################################################################################
################################################################################
################################################################################
################################################################################
# the older function not sure if correctly calculates permutations 
# perm_test <- function(x, y,  
#                       data = NULL, 
#                          B = 1000, 
#                       seed = 123, 
#                       tail = c("one", "two"),
#                        fun = cor, ...)
# {
#   xname <- deparse(substitute(x))
#   yname <- deparse(substitute(y)) 
#   fname <- deparse(substitute(fun))
#    tail <- match.arg(tail)
# if (!is.null(data))
#   {
#     y <- data[,yname]
#     x <- data[,xname]
#   }   
# set.seed(seed)
#         lx <- length(x)
#         ly <- length(y)
#         px <- rep(1, lx)
#         py <- rep(2,ly)
#          F <- factor(c(px,py))
#         Da <- data.frame(v=c(x,y), f=F)
#          N <- dim(Da)[1]
#  hat_theta <- fun(x,y, ...) #diff(by(data=Da$v, INDICES=Da$f, FUN=fun, simplify=TRUE))
# star_theta <- foreach(i = 1 : B,  .combine = rbind)%dopar%
#     {
#       xy <- sample(Da$v, size=N,  replace=FALSE)
#       df <- data.frame(x=xy[Da$f==1], y=xy[Da$f==2])
#       dd <- fun(df$x, df$y, ...)
#       dd
#     }
# prFASL <-  switch(tail,
#  {"one"=if(sign(hat_theta)==-1) 1-sum(star_theta>hat_theta)/B
#            else sum(star_theta>hat_theta)/B},
#  "two"=sum(abs(star_theta) > abs(hat_theta))/B)
#   out <- list(ASL = prFASL, observed=hat_theta, sample=star_theta,
#               fname=fname, xname=xname, yname=yname)
#   class(out) <- "permutationTest"
#   out
# }
################################################################################
################################################################################
################################################################################
################################################################################
# the new function 
# takes two columns 
# use foreach to to permute x and calculate the value from the `fun` 
# which by default is Pearson's cor 
# (the thing is whether it work with other correlations?)
#  then depending on one or tow sides calculates the 
# hat_theta is the Achieved Confidence Level
# star_theta is the simulated correlation permutaion values  
cor_perm_test <- function(x, y, 
                          data = NULL, 
                             B = 1000, 
                          seed = 123, 
                          tail = c("one", "two"),
                           fun = cor, ...)
{
     xname <- deparse(substitute(x))
     yname <- deparse(substitute(y)) 
     fname <- deparse(substitute(fun))
      tail <- match.arg(tail)
if (!is.null(data))
  {
    y <- data[,yname]
    x <- data[,xname]
  }   
set.seed(seed)
        lx <- length(x)
        ly <- length(y)
        Da <- data.frame(x=x,y=y)
         N <- dim(Da)[1]
 hat_theta <- fun(x,y, ...) #diff(by(data=Da$v, INDICES=Da$f, FUN=fun, simplify=TRUE))
star_theta <- foreach(i = 1 : B,  .combine = rbind)%dopar%
    {
         x <- sample(Da$x, size=N,  replace=FALSE)
        df <- data.frame(x=x, y=Da$y)
        dd <- fun(df$x, df$y, ...)
        dd
    }
prFASL <-  if (tail =="one")
          {
           if(sign(hat_theta)==-1) 1-sum(star_theta>hat_theta)/B
            else sum(star_theta>hat_theta)/B
          } else 
          {
  sum((abs(star_theta) > abs(hat_theta))/B) 
          }  
  out <- list(ASL = prFASL, observed=hat_theta, sample=star_theta,
              fname=fname, xname=xname, yname=yname)
  class(out) <- "permutationTest"
  out
}
################################################################################
################################################################################
################################################################################
################################################################################
print.permutationTest <- function (x, digits = max(3, getOption("digits") - 3), ...) 
{   
  yname <- x$yname
  xname <- x$xname
  fname <- x$fname
  cat("Permutaion test between", yname, "and", xname, "\n")
  cat("Null hypothesis", fname, "is zero","\n")
  cat("Probability of ASL:", format(signif(x$ASL)), "\n")
}
################################################################################
################################################################################
################################################################################
################################################################################
plot.permutationTest <- function(x, y, ..., 
                        binwidth = (max(x$sample) - min(x$sample))/20,  
                        hist.col = "black", hist.fill="white",
                        points.col="steelblue4", points.size=1,
                        dens.fill = "#FF6666",
                       line.col="darkgray",line.size=1.5,
                       save.data=FALSE)
{
  y <- x <-  NULL
  yname <- x$yname
  xname <- x$xname
   maxx <- max(c(x$observed,x$sample))
   minx <-  min(c(x$observed,x$sample))
     d <- data.frame(y=x$sample) 
    gg <- ggplot(d, aes(x = y)) + geom_histogram(aes(y = ggplot2::after_stat(density)), 
             binwidth = binwidth, colour = hist.col, fill = hist.fill) + 
             xlim(minx, maxx) + 
             geom_density(alpha = 0.2,  fill = dens.fill) + 
             xlab("sample") + ylab("density") + 
             ggtitle(paste0(x$fname, " permutation test"))+
             geom_vline(aes(xintercept=x$observed), col="red") 
  gg
}
################################################################################
################################################################################
################################################################################
################################################################################
# this function bootsrap the x and y and create B samples for correlations 
cor_boot <- function(x,y, data = NULL, 
                                  B = 1000, 
                               seed = 123, 
                               tail = c("one", "two"),
                                fun = cor, ...)
{
  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y)) 
  fname <- deparse(substitute(fun))
   tail <- match.arg(tail)
  if (!is.null(data))
  {
    y <- data[,yname]
    x <- data[,xname]
  }   
  set.seed(seed)
  lx <- length(x)
  ly <- length(y)
  Da <- data.frame(x=x,y=y)
   N <- dim(Da)[1]
  hat_theta <- fun(x,y, ...) #diff(by(data=Da$v, INDICES=Da$f, FUN=fun, simplify=TRUE))
  star_theta <- foreach(i = 1 : B,  .combine = rbind)%dopar%
    {
       ii <- sample(N, N, replace = T)
      df <- data.frame(x=x, y=y)
      df <- df[ii, ]
      dd <- fun(df$x, df$y, ...)
      dd
    }
  meanCor <- mean(star_theta)
    sdCor <- sd(star_theta)
  prFASL <-  if (tail =="one")
  {
    if(sign(hat_theta)==-1) 1-sum(star_theta > hat_theta)/B
    else sum(star_theta > hat_theta) / B
  } else 
  {
    sum((abs(star_theta) > abs(hat_theta))/B) 
  }  
  out <- list(ASL = prFASL, observed=hat_theta, mean=meanCor,  sd=sdCor, 
              sample=star_theta,
              fname=fname, xname=xname, yname=yname)
  class(out) <- "cor_bootstrap"
  out
}
################################################################################
################################################################################
################################################################################
################################################################################
print.cor_bootstrap <- function (x, digits = max(3, getOption("digits") - 3), ...) 
{   
if (!is(x, "cor_bootstrap"))  stop( "the object has to be cor_bootstrap")
  yname <- x$yname
  xname <- x$xname
  fname <- x$fname
  bias <- x$observed-x$mean
  cat("correlation between", yname, "and", xname, "\n")
  cat("   observed:", format(signif(x$observed, digits=3)), "\n")
  cat("sample mean:", format(signif(x$mean,     digits=3)), "\n")
  cat("       bias:", format(signif(bias,       digits=3)), "\n")
  cat("  sample SD:", format(signif(x$sd,       digits=3)), "\n")
  cat("  sample size:", format(signif(length(x$sample),   digits=0)), "\n")
}
################################################################################
################################################################################
################################################################################
################################################################################
plot.cor_bootstrap <- function(x, y,...,   
                      binwidth = (max(x$sample) - min(x$sample))/20,  
                      hist.col = "black", hist.fill="white",
                    points.col = "steelblue4", points.size=1,
                     dens.fill = "#FF6666",
                      line.col = "darkgray",line.size=1.5,
                     save.data = FALSE)
{
  if (!is(x, "cor_bootstrap"))  stop( "the object has to be cor_bootstrap")
  x <- y <- NULL
  yname <- x$yname
  xname <- x$xname
   maxx <- max(c(x$observed,x$sample))
   minx <-  min(c(x$observed,x$sample))
      d <- data.frame(y = x$sample) 
     d1 <- data.frame(x = c(x$observed, x$mean), color=c("observed","mean"))
     gg <- ggplot(d, aes(x = y)) + geom_histogram(aes(y = ggplot2::after_stat(density)), 
            binwidth = binwidth, colour = hist.col, fill = hist.fill) + 
    xlim(minx, maxx) + 
    geom_density(alpha = 0.2,  fill = dens.fill) + 
    xlab("sample") + ylab("density") + 
    ggtitle(paste0(x$fname, " bootstrap dist."))+
    geom_vline(data=d1, aes(xintercept=x[1],  color = "Observed"))+
    geom_vline(data=d1, aes(xintercept=x[2],  color = "Sample mean"))+
    scale_color_manual(
       name = "type",
       values = c("Observed" = "red", "Sample mean" = "blue")
        )#       
  gg
}
################################################################################
################################################################################
################################################################################
################################################################################
cor_boot_test <- function(obj, null=0 )
{
if (!is(obj, "cor_bootstrap")) stop("the object should be cor_bootstrap object")
cat("tesing the null hypothesis association is 0", "\n")

}
################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################
################################################################################