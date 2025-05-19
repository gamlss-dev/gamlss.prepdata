# THE FUNCTIONS ARE DESIGN TO FIND EMPTY SPACES 
# I thin I found a solution to the problem 
################################################################################
################################################################################
################################################################################
################################################################################
# plot_void <- function(x,y, table.length = floor(sqrt(length(y))), plot=TRUE)
# {
#    xr <- range(x)
#    yr <- range(y)
#    xg <- cut(x, table.length) #seq(xr[1], xr[2], length.out=length+1))
#    yg <- cut(y, table.length) #seq(yr[1], yr[2], length.out=length+1))
#    df <- data.frame(x=xg, y=yg)
#    tt <- table(df$x,df$y)
# if (plot) image(tt)
#   cat("% void", per = (sum(tt==0)/(table.length^2)), "\n")
#   cat("table length =", table.length, "\n") 
#   cat("prob0 =", exp(-mean(tt)), "\n") 
#   cat("mean =", mean(tt), "\n") 
#   invisible(tt)
# }
################################################################################
################################################################################
################################################################################
################################################################################
# going through different lengths 
# get the mean (meantt) of the table and find the probability for zero (prob0)
# (perc) is the percentage of zero cells
# 
# find_void <- function(x,y, 
#     table.length = c(5,10,15,20,25,30, 35,40,45,50, 60, 70,80,90,100),
#             Plot = TRUE)
# {
################################################################################
#   emply_spaces <- function(x,y, table.length = 10)
#   {
#      xr <- range(x)
#     uxl <- (xr[2]-xr[1])/table.length
#      yr <- range(y)
#     uyl <- (yr[2]-yr[1])/table.length
#      xg <- cut(x, table.length) #seq(xr[1], xr[2], length.out=length+1))
#      yg <- cut(y, table.length) #seq(yr[1], yr[2], length.out=length+1))
#      df <- data.frame(x=xg, y=yg)
#      tt <- table(df$x,df$y)
#  meantt <- mean(tt)
#   prob0 <- exp(-meantt)
#    perc <- sum(tt==0)/(table.length^2)
#     pen <- abs(perc-(1-prob0))
#   c(pen=pen, perc=perc, prob0=prob0)
#   }
################################################################################  
#      lt <- length(table.length)
#   prob0 <- rep(0, lt)
#    pen <- rep(0, lt)
#   perc <- rep(0, lt)
#     ii <- 0
# for(i in table.length)
#   {
#     ii <- ii+1
#    CC <- emply_spaces(x,y, table.length=i)  
#   pen[ii] <- CC[1]
#  perc[ii] <- CC[2]
# prob0[ii] <- CC[3]
#   }
# if (Plot) 
#   {
#   op<- par(mfrow=c(1,2))
#   plot(perc~table.length)
#   points(prob0~table.length, col="red")
#   plot(pen~table.length)
#   par(op)
#   }  
# cat("void =", 1-perc[which.min(pen)], "\n") 
# cat("table length =", table.length[which.min(pen)], "\n") 
# invisible(list(perc,pen, prob0))
# } 
################################################################################
################################################################################
################################################################################
################################################################################   
# data 
# size<-c(100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,10000,20000,30000,40000,50000,100000, 500000,1000000)
# length<-c(6,8,10,12,13,15,16,17,18,19,26,32,37,41,58,82,100,118,129,190,420,580)
# plot(log(length)~log(size))
# m1=lm(log(length)~log(size))
# coef(m1)
#  -0.5165698   0.4983922 
#  
################################################################################
# THE FUNCTION VOID() i my latest idea of calculation  empty spaces in the x and 
# y direction for a given x and y 
# i) get the length of the variables , `n`,
# for the number of variables calculates the length, of the table `table.length`,
#  to use to calculate empty spaces
# ii( create a table of size `table.length*table.length`
#  and calculates the number of zeros in the table.
# the way  the length of the table is  calculated is like this 
# a) for a given `n` use the function fn() to calculate the table length `table.length`
# b) the coefficients in fn() are obtain by fitting a linear model to log(length)
# against log(size) of the data given above The way the data were calculated is as follows 
#  for given size create two runif(n) variables and calculate which values of 
#  `table.length gives a close to 0.05 value       
################################################################################
################################################################################
################################################################################
################################################################################     
# the function void() looks of % of empty spaces in the x and y direction
################################################################################ 
# I added the `table.length` as argument in case we want to overrule the method 
# for calculating it  using function fn()
void <-  function(x,y,  plot=TRUE, print=TRUE, table.length)
  {
     fn <- function(n) exp(-0.5165698+0.4983922*log(n) )  
      n <-length(x)
if (length(y)!=n) stop("the x and y should have the same length")  
table.length <- if (missing(table.length)) floor(fn(n)) 
              else table.length
    
    xr <- range(x)
    yr <- range(y)
    xn <- deparse(substitute(x))
    yn <- deparse(substitute(y))
    xg <- cut(x, table.length) #seq(xr[1], xr[2], length.out=length+1))
    yg <- cut(y, table.length) #seq(yr[1], yr[2], length.out=length+1))
    df <- data.frame(x=xg, y=yg)
    tt <- table(df$x,df$y, dnn= c(xn, yn))
    if (plot) image(tt, xlab=xn, ylab=yn)
    if (print)
    {
      cat("% void", (sum(tt==0)/(table.length^2)), "\n")
      cat("table length =", table.length, "\n") 
      cat("prob0 =", exp(-mean(tt)), "\n") 
      cat("mean =", mean(tt), "\n") 
    }
    invisible((sum(tt==0)/(table.length^2)))
}
################################################################################
################################################################################
################################################################################
################################################################################
#  empty spaces for a data.frame
# it check whether they are factor and only show the
#  empty spaces for continuous variables
# The version uses foreach
data_void <- function(data,   
                    digits = 3,
                      plot = TRUE,
                  diag.off = TRUE,
             lower.tri.off = FALSE,  
                    method = c("square", "circle"),
             outline.color = "gray",
                    colors = c("blue", "white", "red"),
              legend.title = "Void",
                    title,
                   ggtheme = ggplot2::theme_minimal(),
                    tl.cex = 12,
                    tl.col = "black", 
                    tl.srt = 45,
                       lab = TRUE, 
                   lab_col = "black", 
                  lab_size = 3,
               circle.size = 20,
                      seed = 123, 
                percentage,
                print.info = TRUE
             ) # c(1,15) maybe will do
{
################################################################################
################################################################################
#  require(foreach)
################################################################################
################################################################################
# local function matrix (table) to data frame  
  mat2df <- function(mat)
  {
     rna <- rownames(mat)
     cna <- colnames(mat)
    lrna <- length(rna)
    lcna <- length(cna)
   value <- as.vector(mat)
#if (length(mat)!=lrna*lcna) stop("incomatible lengths")   
    fac1 <- gl(lrna, 1, length = lrna*lcna, labels=rna)
    fac2 <- gl(lcna, lrna, length = lrna*lcna, labels=cna)
     daf <- na.omit(data.frame(fac1, fac2, value=value)) 
    daf
  }
################################################################################ 
################################################################################    
# data.frame missing 
     i <- j <- NULL
nameData <- deparse(substitute(data))
if (missing(data) || NROW(data) <= 1) 
    stop("nothing to do for this data frame")
# data obs na's
if (is(data, "list"))  stop("the data is list  the function needs a data.frame") 
if (is(data, "table")) stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array")) stop("the data is an array the function needs a data.frame")    
     dimD <- dim(data)
     data <- if (missing(percentage))
     {
       data_cut(data,seed = seed, print.info = print.info)
     }      else data_cut(data, seed = seed, percentage=percentage, 
                          print.info=print.info)   
if (any(is.na(data)))
  {
       l1 <- dim(data)[1]
     data <- na.omit(data)
       l2 <- dim(data)[1]
    warning(cat(l1-l2, "observations were omitted from the data", "\n"))
  }
#  if is a list or table
if (is.null(dimD)) stop("only one variable in the data") 
if (dimD[1]<20)   stop(cat("the size of the data set is too small", "\n",
                             "to detect empty spaces", "\n"))   
       TT <- lapply(data, unique)
     daTa <- subset(data, select=
              ifelse(sapply(data,is.factor) | sapply(data,is.character) |
                    sapply(data,is.integer)&lapply(TT, length)<10, FALSE, TRUE))
      Dim <- dim(daTa)
if (Dim[2]==0) stop("no variable is left after taking out the factors")             
if (Dim[2]==1) stop("only one variable is left after taking out the factors")  
      daTa |> as.data.frame() -> daTa # |> data_int2num()  -> daTa
 diffDim  <- dimD[2]-Dim[2]
if (diffDim > 0)
  {
    warning(cat(diffDim, 'factors have been omited from the data', "\n"))
}
   cnames <- names(daTa)
  lcnames <- length(cnames)
       CC <- matrix(0, ncol=lcnames, nrow=lcnames)
  #index1 <- outer(1:lcnames, 1:lcnames, "paste0")  
   #index <- index1[ upper.tri(index1)]
    #lind <- length(index)
#    ((lcnames*lcnames)-length(diag(CC)))/2
       # data_comb = expand.grid(names(data), names(data),  stringsAsFactors = F) |> 
       #   stats::setNames(c("X1", "X2"))
  i_index <- col(CC)[upper.tri(CC)] 
  j_index <- row(CC)[upper.tri(CC)]
    liind <- length(i_index)
    ljind <- length(j_index)
       Cv <- rep(0, length=liind)
################################################################################       
       Cv <- foreach(i=1:liind, .combine=cbind) %dopar% 
    {
           ii <- i_index[i]
           jj <- j_index[i]
           xi <- daTa[,ii]
           xj <- daTa[,jj]
        Cv[i] <- void(xi, xj, plot=FALSE, print=FALSE)
    }
################################################################################       
  Cv <- as.vector(Cv)     
# for vector 2 table 
# vec2tab <- function(vec, cha1, cha2)
# {
#   lvec <- length(vec)
#  if (length(cha1)*length(cha2)!=lvec) stop("the lenth of vec is not compatible"")
# }
## take the vector and create a diagonal matrix
         for (i in 1:liind)
  {
    ii <- i_index[i]
    jj <- j_index[i]
    CC[ ii, jj ] <- Cv[i]
         }
## get the full matrix (rather than diagonal)
   CC <- CC+t(CC)  
## put names back   
  rownames(CC) <- cnames
  colnames(CC) <- cnames
      diag(CC) <- 1
            CC <- base::round(x = CC, digits = digits)   
if (diag.off) diag(CC) <- NA
if (lower.tri.off) CC[lower.tri(CC)] <- NA
if (plot==FALSE) return(CC)
       method <- match.arg(method)
## from matrix to df for ggplots
         corr <- mat2df(t(CC))
     lowerLim <- 25-floor((range(corr$value)[2]-range(corr$value)[1])*20)
colnames(corr) <- c("var_1", "var_2", "value")
  txt.title <- if (missing(title))
    paste("Empty spaces from data:", nameData)
  else title
  corr$abs_corr <- abs(corr$value) * 10
  p <- ggplot2::ggplot(data = corr,
                       mapping = ggplot2::aes_string(x = "var_1", y = "var_2", 
                                                     fill = "value"))
  if (method == "square")
  {
    p <- p + ggplot2::geom_tile(color = outline.color)
  }
  else if (method == "circle") 
  {
    p <- p + geom_point(color = outline.color, shape = 21,
                        ggplot2::aes_string(size = "abs_corr")) +
      # scale_size(range = circle_scale_size_range) +
      scale_size_area(max_size = circle.size) +
      guides(size = "none")
  }
  label <- round(x = CC, digits = digits)
  p <- p + ggplot2::scale_fill_gradient2(low = colors[1], high = colors[3],
                                         mid = colors[2],  midpoint = 0.5, limit = c(0, 1),
                                   
                                               space = "Lab",
                                         name = legend.title)+ggplot2::ggtitle(txt.title)
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme
  }
  else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }
  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = tl.srt,
                                                              vjust = 1, size = tl.cex, colour=tl.col, hjust = 1),
                          axis.text.y = ggplot2::element_text(size = tl.cex, 
                                                              colour=tl.col)) +
    ggplot2::coord_fixed()
  label <- round(x = corr[, "value"], digits = digits)
  if (lab) {
    p <- p + ggplot2::geom_text(
      mapping = ggplot2::aes_string(x = "var_1", y = "var_2"),
      label = label, color = lab_col, size = lab_size)
  }
  p
}       
################################################################################
################################################################################
################################################################################
################################################################################
