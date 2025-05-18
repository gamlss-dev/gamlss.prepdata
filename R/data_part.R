################################################################################
################################################################################
################################################################################
################################################################################
# PARTITION OF DATA
################################################################################
################################################################################
# function 1 saving the data with a partition factor
# the data_part creates data set with an extra factor called partition
################################################################################
################################################################################
################################################################################
################################################################################ 
# function 1
data_part <- function(data, partition=2L, probs, setseed=123, ...)
{
  set.seed(setseed)
if (partition<=1L) stop("data partition should be greater that one","\n")
if (partition>=21L) stop("data partition should be less or equal that 20","\n")
if (partition==2L)
{
  cat("data partition into two sets", "\n")
   probs <-   if (missing(probs)) c(0.6,0.4) 
             else probs
if (abs(sum(probs)-1)>1.0e-10) stop("probs should add up to 1")   
if (length(probs)>2||length(probs)<=0) stop("the length of probs should be 2")
    rand <- sample(2, dim(data)[1], replace=TRUE, prob=probs)
    rand <- factor(rand, labels=c("train", "test"))
     out <- data.frame(data, partition=rand)
invisible(return(out))
}
if (partition==3L)
{
  cat("data partition into three sets", "\n")
    probs <- if  (missing(probs)) c(0.6,0.2,0.2)
             else probs
if (abs(sum(probs)-1)>1.0e-10) stop("probs should add up to 1")    
if (length(probs)>4||length(probs)<=0) stop("the length of probs should be  3")
    rand <- sample(3, dim(data)[1], replace=TRUE, prob=probs)
    rand <- factor(rand, labels=c("train", "valid", "test"))
     out <- data.frame(data, partition=rand)
    invisible(return(out))              
}  
if (partition>=4L)
  cat( paste0(partition,"-fold data partition"), "\n")
    probs <- rep(1/partition, partition) 
    if (abs(sum(probs)-1)>1.0e-10) stop("probs should add up to 1")    
    rand <- sample(partition, dim(data)[1], replace=TRUE, prob=probs)
    rand <- factor(rand)
    out <- data.frame(data, partition=rand)
    invisible(return(out))    
}
#mosaicplot(table(da$partition))
################################################################################
################################################################################
################################################################################
################################################################################
# This function creates a list with different partions of the same data 
# no more than 3 partitions
################################################################################
################################################################################
################################################################################
################################################################################
# Function 2
data_part_list <- function(data, partition=2L, probs, setseed=123, ...)
{
  set.seed(setseed)
  if (partition<=1L) stop("data partition should be greater that one","\n")
  if (partition >= 21L) stop("data partition should be less or equal that 20","\n")
 # if (partition!=2&&partition!=3) stop("partition should be 2 or 3")
  if(missing(probs))
  {
    probs <- if (partition==2)  c(0.6,0.4) 
             else c(0.6,0.2,0.2)
  } else probs <- probs
  
  if (length(probs)>4||length(probs)<=0) stop("the length of probs should be  2 o 3")
  if (sum(probs)!=1) stop("probs should add up to 1")
  if 
  (partition==2)
  {
     rand <- sample(2, dim(data)[1], replace=TRUE, prob=probs)
    train <- subset(data, rand==1)
     test <- subset(data, rand==2)
      out <- list(train=train , test=test)
      invisible(return(out))
  }
  if (partition==3)
  {
     rand <- sample(3, dim(data)[1], replace=TRUE, prob=probs)
    train <- subset(data, rand==1)
    valid <- subset(data, rand==2)
     test <- subset(data, rand==3)
      out <- list(train=train, valid=valid, test=test)
      invisible(return(out))              
  }  
  if (partition>=4L)
  {
    cat( paste0(partition,"-fold data partition"), "\n")
      out <- list()
    probs <- rep(1/partition, partition)
    if (abs(sum(probs)-1)>1.0e-10) stop("probs should add up to 1")    
     rand <- sample(partition, dim(data)[1], replace=TRUE, prob=probs)
     rand <- factor(rand)
for (i in 1:partition)
    {
      out[[i]] <- subset(data, rand==i)  
}
    names(out) <- paste(1:partition) 
    invisible(return(out))   
  } 
}
################################################################################
################################################################################
################################################################################
################################################################################
# I am not sure about that at the moment I may have to go out
# data_part_index <- function(data, K=10, bootstrap=FALSE)
# {
#   dD <- dim(data)
#   N <- dim(data)[1]
#   if (bootstrap)  
#   {
#     mm <-    lapply(
#                as.data.frame(
#                t(
#                  sapply(
#                    sample(rep_len(1:N, length.out=N), replace=TRUE),
#                    "!=",1:K)
#                  )
#                             ), 
#                         which)    
#   } else 
#   {
#     mm <-    lapply(
#               as.data.frame(
#                 t(
#                   sapply(
#                     sample(rep_len(1:K, length.out= N), replace=FALSE)
#                         ,"!=", 1:K )
#                   )
#                           ), 
#               which ) 
#   } 
#   mm
# }
################################################################################
################################################################################
################################################################################
################################################################################
# I was not sure if this function is working so it is not documented in the package 
data_Kfold_weights <- function(data, K = 10, setseed=123)
{
  set.seed(setseed)
  n <- dim(data)[1]
  # folds for cross-validation 
  CVfolds <-
      t(
        sapply(
          sample(rep_len(1:K, length.out=n),replace=FALSE)
          ,"!=", 1:K)
              )*1 #if you want it numeric
  return(CVfolds)
}
################################################################################
################################################################################
################################################################################
################################################################################
# I was not sure if this function is working so it is not documented in the package 
data_Kfold_index <- function(data, K = 10, setseed=123)
{
  set.seed(setseed)
  n <- dim(data)[1]
  # folds for cross-validation 
  CVfolds <-
    t(
      sapply(
        sample(rep_len(1:K, length.out=n),replace=FALSE)
        ,"!=", 1:K)
    )*1 #if you want it numeric
  index <- CVfolds*(1:n)
  return(index)
}

################################################################################
################################################################################
################################################################################
################################################################################
# I was not sure if this function is working so it is not documented in the package 
# Mikis 22-4-25 It look OK to me 
# get a data frame and creates a K bootstrap indices as a matrix or a list
data_boot_index <- function(data, B=10, setseed=123 )
{
set.seed(setseed)
     nfolds <- B
          n <- dim(data)[1]
  BOOTfolds <- lapply( 
     as.data.frame(
         matrix(
        sample(1:n, nfolds*n,replace=TRUE)
        , n)
    ),
    sort) 
  BOOTnotfolds <- list()
  for (i in 1:B)
    BOOTnotfolds[[i]] <-  setdiff(1:n,BOOTfolds[[i]]) 
 return(list(IB=BOOTfolds,OOB=BOOTnotfolds))  
}
################################################################################
################################################################################
################################################################################
################################################################################
data_boot_weights <- function(data, B=10, setseed=123)
{
  
set.seed(setseed)
   nfolds <- B
        n <- dim(data)[1]
BOOTfolds <- lapply( 
    as.data.frame(
      matrix(
        sample(1:n, nfolds*n,replace=TRUE)
        , n)
    ),
    sort) 
BOOT <- list()
for (i in 1:B)
{ 
      boot <- rep(0,n)
       tab <- table(BOOTfolds[[i]])
      tab1 <- as.numeric(unclass(tab))
       ind <- as.numeric(unlist(attributes(table(BOOTfolds[[i]]))$dimnames[[1]]))
 boot[ind] <- tab1
 BOOT[[i]] <- boot
}
  return(matrix(unlist(BOOT),ncol=B))  
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
data_cut <- function(data,percentage, seed=123, print.info=TRUE)
{
  dm <- dim(data)
  nobs <- dm[1]
  if (missing(percentage)) 
  {
    percentage <- ifelse(nobs<50000,1,         # all data
                         ifelse(nobs<100000,.5,# 50% of data
                                ifelse(nobs<1000000,.2, # 20% of data
                                       ifelse(nobs>1000000,.1))))  # 10% of data
    
  }
  set.seed(seed)
  ind <- sample(nobs, floor(percentage*nobs))
  data <- data[ind,]
if (print.info)
{
  cat("",  percentage*100,"% of data are saved,", "\n")
  cat("that is,", floor( percentage*nobs),"observations.", "\n")
}  
  invisible(data)
}
################################################################################
################################################################################
################################################################################
################################################################################


#                 END
################################################################################
################################################################################
################################################################################
################################################################################



#CVfolds<- lapply(as.data.frame(t(sapply(sample(rep_len(1:nfolds,length=n),replace=FALSE)
#                                        ,"!=", 1:nfolds))), which)  
#BOOTfolds<- lapply(as.data.frame(matrix(sample(1:n, nfolds*n, replace=TRUE), n)),sort)  

#BOOTfolds<- lapply(as.data.frame(matrix(sample(1:10, 10*10, replace=TRUE), 19)),sort) 
#matrix(unlist(BOOT),nrow=10)
########

#data_rm(rent99, c("rentsqm", "district"))
#data_rm(rent99, c(2, 9))


