# MIKIS STASINOPOULOS
# 25-09-2025
# functions 
# 1) data_scale  
# 2) data_vars2data
# 3) data_formulae 
# 4) data_form2X
# 5) y_scale0to1 (not exported)
# 6) y_unscale0to1 (not exported)
# 7) y_factor  
# 8) data_factor
################################################################################
################################################################################
################################################################################
################################################################################
# to do
# i) IT NEEDS CLEARING UP done
# ii) also to exclude variables from the data DONE
# iii) how to scale is important we should have options
 #       scale = c(z-scores", "0to1") DONE
# iv)    type  = c("main.effect", "first.order"), I need also 
#                "none"      
################################################################################
################################################################################
################################################################################
################################################################################
# the function   data_scale() creates a data frame with all continuous variables 
# standardise of as z-scores ot as 0 to 1 variables
# the function data_create() takes a data frame and creates a data frame 
# using a `main effects' or `first order` interactions formula
# the new data frame will have all factors as dummies rather in their original 
# scale 
# if scaling for the continuous variables is required the function have to used 
# recursively
# da |> data_stand(, response=...) |> data_create(, response=...) |> danew
################################################################################
################################################################################
################################################################################
################################################################################
# function 1
data_scale <- function(data, response, 
        position.response = NULL,
                 scale.to = c("z-scores", "0to1"),  
                   family = "NO", # or SHASH  if "z-scores" 
           scale.response = FALSE
                  )
{
# all the variable in data are used to create the data.frame
# the argument `data' is compulsory 
# the argument `response' is compulsory  
# scale.to  :  the type of scale  
#  i) "z-scores" using SHASH
#  ii) 0 to 1 
################################################################################
################################################################################
scale0to1 <- function(x)
  {
       x <- as.matrix(x)
      nc <- ncol(x)
    minX <- apply(x, 2L, min, na.rm=TRUE )
    maxX <- apply(x, 2L, max, na.rm=TRUE )
       Y <- sweep(x, 2L, minX, check.margin = FALSE)
       Y <- sweep(Y, 2L, maxX-minX, FUN="/",  check.margin = FALSE)
    attr(Y, "min") <- minX
    attr(Y, "max") <- maxX
    Y
  } 
################################################################################
  unscale0to1 <- function(x) 
  {
    minX <- attributes(x)$min
    maxX <- attributes(x)$max
    Y <- sweep(x, 2L, (maxX-minX), FUN="*", check.margin = FALSE)
    Y <- sweep(Y, 2L, minX, FUN="+", check.margin = FALSE)
    attr(Y, "min") <- NULL
    attr(Y, "max") <- NULL
    Y
  }
################################################################################
################################################################################
if (missing(data)) stop("the data frame is missing")  
if (missing(response)&&is.null(position.response)) stop("response (or its position) should be given")
# The response can be given both as R or "R"   
      response_t <- try(eval(response), silent=TRUE)
  if (any(class(response_t)%in%"try-error"))
  { 
    response_t <- deparse(substitute(response))   
  }
      scale.to <- match.arg(scale.to)   
if (is.null(position.response))
{
     pos_res <- match(response_t, names(data))  # position  
} else 
{
     pos_res <-  position.response
}  
        x_Names <- names(data)[-pos_res]           # the x names
            daT <- data[,x_Names]                  # only the data x's in data
  whetherFactor <- sapply(daT, is.factor)|         # checking for factors
                   sapply(daT, is.character)|
                   sapply(daT, is.logical)|
                   data_distinct(data[,-pos_res], get.distinct=TRUE) <10|
                   sapply(data[,-pos_res], function(x) is(x, class2="Date"))
     theFactors <- x_Names[whetherFactor]         # the names of factors
             pp <- unlist(sapply(theFactors, grep, names(data))) 
if (scale.to  =="z-scores")                          # if z-scores
  {
  if (scale.response)
  {
    suppressWarnings(DF1 <- data_zscores(data[,-c(pp)], 
                              plot=FALSE, family=family)) 
    data[,names(DF1)] <- DF1[,names(DF1)]
  }  else
  {
    suppressWarnings(DF1 <- data_zscores(data[,-c(pp, pos_res)], 
                                         plot=FALSE, family=family))
    data[,names(DF1)] <- DF1
  }
 
  }
if (scale.to  =="0to1")                              # if 0 to 1            
  {
  if (scale.response)
  {
    DF1 <- scale0to1(data[,-c(pp)])
    data[,colnames(DF1)] <- DF1[,colnames(DF1)]
  }  else
  {
    DF1 <- scale0to1(data[,-c(pp, pos_res)])
    data[,colnames(DF1)] <- DF1
  }
  }
################################################################################
## return the data frame
  return(data)
} 
################################################################################
################################################################################
################################################################################
################################################################################          
#  FUNCTION 2
# this function creates a new dataframe where the continuous variables are remain 
# the same or became polynomial ans the factors becomes dummy variables    
data_vars2data <- function(data, response, 
                         exclude = NULL,
                            type = c("main.effect", "first.order"),
                         weights = NULL,
                      nonlinear = FALSE, #for "main.effect" 
                          basis = "poly",
                            arg = 2 
)
{
################################################################################  
# all the variable in data are used to create the data.frame
# unless the 'exclude' is used
# the argument `data' is compulsory 
# the argument `response' is compulsory
################################################################################   
  if (missing(data)) stop("the data frame is missing")  
  if (missing(response)) stop("response should be given")
 response_t <- try(eval(response), silent=TRUE)
  if (any(class(response_t)%in%"try-error"))
   { 
    response_t <- deparse(substitute(response))   
   }  
   if (!is.null(exclude))  data <- data[, setdiff(names(data),exclude)]    
        type <- match.arg(type)  
  pos_res <- match(response_t, names(data))
  x_Names <- names(data)[-pos_res]  
  daT <- data[,x_Names]
  whetherFactor <- sapply(daT, is.factor)|   # checking for factors
    sapply(daT, is.character)|
    sapply(daT, is.logical)|
    data_distinct(data[,-pos_res],get.distinct=TRUE) <10|
    sapply(data[,-pos_res], function(x) is(x, class2="Date"))
  theFactors <- x_Names[whetherFactor]
  pp <- unlist(sapply(theFactors, grep, names(data)))
  ## get the formula according to type 
  if (type=="main.effect")
  {
    if (nonlinear)
    {
      daC <- data[,-c(pp, pos_res)]#      take out factors and response  
      ndc <- names(daC)#                  names of the continuous vars 
      aa <- deparse(substitute(arg))#.    get the argument for nonlinear
      newn <- paste0(basis, "(", ndc, ",", aa, ")") # get the right function of
                                                    # non linear 
      lndc <- length(ndc) #                how many continuous vars exist  
      mindex <- match(ndc,x_Names)#        their index in the data
      qq <- character(length=lndc)#.       creating a character vector  
      for (i in 1:lndc) qq[i] <- gsub(ndc[i], newn[i], x_Names[mindex[i]])
      x_Names[mindex] <- qq
      formula <- as.formula(paste(paste0(response_t,"~"), 
                                  paste(x_Names, collapse='+'))) 
      XX <- model.matrix(formula, weights=weights, data=data)[,-1]
      for (i in 1:lndc) colnames(XX)[grep(ndc[i], colnames(XX))] <- paste0(ndc[i], 1:aa)
      dXX <- data.frame(data[, response_t], as.data.frame(XX))# the data frame
      names(dXX)[1] <- response_t
      #dXX[, response_t] <- data[, response_t]#.         add the response  
      return(dXX)   
    } else
    { # only linear terms for everything here 
      formula <- as.formula(paste(paste0(response_t,"~"), 
                                  paste(x_Names, collapse='+'))) 
      XX <- model.matrix(formula, weights=weights, data=data)[,-1]
      dXX <- data.frame(data[, response_t], as.data.frame(XX))# 
      names(dXX)[1] <- response_t
      #dXX[, response_t] <- data[, response_t]# add the response
      return(dXX)   
    }
  }  
  if (type=="first.order")  
  {
    if (nonlinear)
    {
      daC <- data[,-c(pp, pos_res)]#       take out factors and response  
      ndc <- names(daC)#                   names of continuous  
      aa <- deparse(substitute(arg))#.    get the argument  on nonlinear
      newn <- paste0(basis, "(", ndc, ",", aa, ")") # get the right function for nn 
      lndc <- length(ndc) #                 how many continuous vars exist  
      mindex <- match(ndc,x_Names)#           their index in the data
      qq <- character(length=lndc)#.      creating a character vector  
      for (i in 1:lndc) qq[i] <- gsub(ndc[i], newn[i], x_Names[mindex[i]])
      x_Names[mindex] <- qq
      formula <- as.formula(paste(paste0(response_t,"~"), 
                                  paste(x_Names, collapse='+'))) 
      XX <- model.matrix(formula, weights=weights, data=data)[,-1]
      for (i in 1:lndc) colnames(XX)[grep(ndc[i], colnames(XX))] <- paste0(ndc[i], 1:aa)
      x_Names <- colnames(XX)    
      dXX <- data.frame(data[, response_t], as.data.frame(XX))# the data frame
      names(dXX)[1] <- response_t
      formula <- as.formula(paste(paste0(response_t,"~"), 
                                  paste0(paste0("(",paste(x_Names, collapse='+')), ")^2"))) 
      XX <- model.matrix(formula, weights=weights, data=dXX)[,-1] 
      d2 <- dim(XX)[2]
      Names <- character(d2)
      for (i in 1:d2) 
      {
        Names[i] <-  gsub(":", ".", colnames(XX)[i])
      }
      colnames(XX) <- Names
      dXX <-  as.data.frame(XX)
      dXX <- data.frame(data[, response_t], as.data.frame(XX))# 
      names(dXX)[1] <- response_t
      return(dXX)        
    } else
    {
      formula <- as.formula(paste(paste0(response_t,"~"), 
                                  paste0(paste0("(",paste(x_Names, collapse='+')), ")^2"))) 
      XX <- model.matrix(formula, weights=weights, data=data)[,-1] 
      d2 <- dim(XX)[2]
      Names <- character(d2)
      for (i in 1:d2) 
      {
        Names[i] <-  gsub(":", ".", colnames(XX)[i])
      }
      colnames(XX) <- Names
      dXX <-  as.data.frame(XX)
      ## in order to have a complete data frame we need also the response 
      dXX <- data.frame(data[, response_t], as.data.frame(XX))# 
      names(dXX)[1] <- response_t
      #  dXX[, response_t] <- data[, response_t]
      return(dXX)    
    }  
  }
}  
################################################################################
################################################################################
################################################################################
################################################################################
# function 3
# from data to formula
data_formulae <- function(data, response)
{
if (is(data, "list"))  
    stop("the data is list  the function needs a data.frame") 
if (is(data, "table")) 
    stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array")) 
    stop("the data is an array the function needs a data.frame")
if (missing(response)) stop("the response should be set")
  # The response can be given both as R or "R"   
      Y <- try(eval(response), silent=TRUE)
if (any(class(Y)%in%"try-error"))
{ 
      Y <- deparse(substitute(response))   
}
    pos <- match(Y, names(data))
  nameS <- names(data)[-pos]
     PP <- list() 
   actY <- data[,Y]
     cY <- class(actY) 
      I <- 0
     f1 <- formula(paste(paste0(Y,"~"),paste0(nameS, collapse='+')), 
                data=data, env=.GlobalEnv)#.GlobalEnv
     f2 <- formula( paste0(paste0(Y,"~"), 
                        paste0("(",paste0(nameS, collapse='+'),")^2")), 
                 data=data, env=.GlobalEnv)#.GlobalEnv
     f3 <- formula(paste("~",paste0(nameS, collapse='+')), 
                data=data,      env=.GlobalEnv)#.GlobalEnv
     f4 <- formula( paste0(paste0("~"), 
                        paste0("(",paste0(nameS, collapse='+'),")^2")), 
                 data=data,      env=.GlobalEnv)#.GlobalEnv
  return(list(main=f1, inter=f2, no_res_main=f3, no_res_inter=f4))         
}

################################################################################
################################################################################
################################################################################
################################################################################
# from  formula to the X matrix
# get data and a formula 
################################################################################ 
# function 4
data_form2X <- function(data, formula, response, 
                          scale.to = c("no", "z-scores", "0to1"),  
                         family = NO) 
{
################################################################################  
# get the formula
# if standardise= TRUE  standardised continuous variable
# model.matrix(formula, data) 
# the argument `data' is compulsory 
# the argument `formula' is compulsory
################################################################################  
if (missing(data)) stop("the data frame is missing")  
    scale.to <- match.arg(scale.to)   
#if (missing(formula)&missing(response)) stop("response or formula should be given")
#  if (!is.null(exclude))  data <- data[, setdiff(names(data),exclude)]    
#  type <- match.arg(type) 
    if (missing(response)) stop("the response should be set")
    Y <- try(eval(response), silent=TRUE)
    if (any(class(Y)%in%"try-error"))
    { 
      Y <- deparse(substitute(response))   
    }
if (missing(formula))  
{
     pos_res <- match(Y, names(data))
    if (pos_res!=1) stop("if no formula is given the response should be first in data")  
     formula <- stats::formula(data) 
} else
{
    response <- formula[[2]]
          Y  <- deparse(substitute(response)) 
     pos_res <- match(Y, names(data))
}  
    if (scale.to!="no")
    {
      data <- data_scale(data, position.response= pos_res, family=family,
                         scale.to=scale.to)
    }
        
          MF <- model.frame(formula, data)
          X  <- model.matrix(formula, MF)
          X  <- X[,-1]
          return(X)
}  
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# function 5
y_scale0to1 <- function(x)
{
      x <- as.matrix(x)
    nc <- ncol(x)
  minX <- apply(x, 2L, min, na.rm=TRUE )
  maxX <- apply(x, 2L, max, na.rm=TRUE )
      Y <- sweep(x, 2L, minX, check.margin = FALSE)
      Y <- sweep(Y, 2L, maxX-minX, FUN="/",  check.margin = FALSE)
attr(Y, "min") <- minX
attr(Y, "max") <- maxX
  Y
} 
################################################################################
################################################################################
################################################################################
################################################################################
# function 6
y_unscale0to1 <- function(x) 
{
  minX <- attributes(x)$min
  maxX <- attributes(x)$max
     Y <- sweep(x, 2L, (maxX-minX), FUN="*", check.margin = FALSE)
     Y <- sweep(Y, 2L, minX, FUN="+", check.margin = FALSE)
attr(Y, "min") <- NULL
attr(Y, "max") <- NULL
  Y
}
################################################################################
################################################################################
################################################################################
################################################################################
# function  7
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
# function 8
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