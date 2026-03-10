################################################################################
################################################################################
################################################################################
################################################################################
############## function 1 ######################################################
# matrix to data.frame 
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
#  names(daf) <- c(cna, rna, "value")
  daf
}
################################################################################
################################################################################
################################################################################
################################################################################
table2df <- function(T)  as.data.frame(T)
################################################################################
################################################################################
################################################################################
################################################################################
######### functions 2 ########################################################## 
is_nominal <- function(x) class(x) %in% c("factor", "character")
################################################################################
################################################################################
################################################################################
################################################################################
######### function 3 ###########################################################  
is_numeric <- function(x) { is.numeric(x) || is.integer(x) || is(x, "double")}
 #is_numeric <- function(x) { class(x) %in% c("numeric", "integer", "double")}
################################################################################    
################################################################################
#is_douple <- function(x)  is(x, "double")
################################################################################
################################################################################
######## time series ###########################################################
# datetime2datehour <- function(datetime, format=NULL) 
# { 
#   X <- t(as.data.frame(strsplit(datetime,' '))) 
#   rownames(X) <- NULL 
#   colnames(X) <- c("date", "time") 
#   hour <- as.numeric(sub(":",".",X[,2])) 
#   date <- as.Date(X[,1],format=format) 
#   data.frame(date, hour) 
# } 
################################################################################
################################################################################
################################################################################
################################################################################
# time2num <- function(time, pattern=":") 
# { 
#   t <- gsub(pattern, ".", time) 
#   as.numeric(t) 
# } 
################################################################################
################################################################################
################################################################################
################################################################################
# EXAMPLES
# From data to table 
# T<- table(rent$H, rent$loc, dnn=c("H","loc"))
# T
# TT <- xtabs(~H+loc, data=rent)
# TT
# ################################################################################
# # from table to data 
# table2df(T)
# table2df(TT)
# 
# mat2df(T)
# mat2df(TT)
# ################################################################################
# # from matrix to df
# pp<-cor(rent[1:3])
# class(pp)
# mat2df(pp)
# table2df(pp)# it produce the same pp not working
# 
# 
# tabulate(rent$R)
# tabulate(rent$R, 5)
# tabulate(rent$H)
# ftable()# for flat table contigency table
# ftable(Titanic, row.vars = 1:3)
# ftable(Titanic, row.vars = 1:2, col.vars = "Survived")
# 
################################################################################
################################################################################
################################################################################
################################################################################
# it take a formula and data and creates a data.frame with the variables in
# the formula  
Formulae2Data <- function(formula = list(), data=NULL, weights=NULL, subset=NULL, 
                          na.action, print = TRUE  )
{
  if (is(formula,"list"))
  {
    lenList <- length(formula)
    if (lenList==0) stop("no formula detected")
    if (lenList==1) 
    {
      ff <- deparse(formula[[1]])
    } else
    {
      # the first formula  
      form <- formula(formula[[1]])
      # create y~x+   
      f1 <- paste(paste(form[[2]],form[[1]]), deparse(form[[3]]), "+")
      # now add the of the formulae    
      for (i in 2:lenList)
      {
        ff <- if (i==lenList) paste(f1, deparse(formula[[i]][[2]]))
        else paste(f1,            deparse(formula[[i]][[2]]),"+")
      } 
    }
  } else if (is(formula,"formula")) {ff  <- deparse(substitute(formula))}
  else stop("The formula argument should be a formula or a list") 
  if (!is.null(weights)) 
  {
    # formula(paste(ff[[3]], collapse = " "))
    ff <- paste(ff, deparse(substitute(weights)), sep="+")
    # ff[[3]] <- paste(ff[[3]],deparse(substitute(weights)), sep="+")
  }
  environment(ff) <- globalenv()    # do I need this
  all.vars <- get_all_vars(ff, data=data)
  mm <- match( names(all.vars),names(data),0)
  #oo <- which(mm==0)# I was not sure why I put this here in the first place 
  #all.vars <- all.vars[,-oo]
  if (!is.null(data)&&!inherits(data,"data.frame")) warning("data is not a data frame class attributes will be lost")
  M <- dim(all.vars)[1]
  ## subsetting             
  if (!is.null(subset)) {
    r <- if (!is.null(data))  eval(substitute(subset), data,  parent.frame())
    else eval(substitute(subset),  parent.frame())
    if (!is.logical(r)) stop("'subset' must be logical")
    all.vars <- all.vars[r,]
    M <- dim(all.vars)[1]
    if (print) cat( M, "observations left after subsetting \n" )           
  }
  # it need a futher warning here      N <- dim(all.vars)[1]  
  # na.omit   
  all.vars <- na.omit(all.vars)     # clear NA's
  N <- dim(all.vars)[1]     
  if (print) {if (M-N > 0) cat(M-N, "rows with NAs are deleted", "\n" )}
  if (print) cat( N, "observations with", dim(all.vars)[2], "variables \n")    
  attr(all.vars, "formula") <- ff
  all.vars
}
################################################################################
################################################################################
################################################################################
################################################################################
# it takes up to four formulas and create one formula in gamlss2 style
Formulae2one <- function(formula, sigma=~1, nu=~1, tau=~1, data )
{
  form <- formula(formula)
  nform <- paste(paste(form[[2]],form[[1]]), deparse(form[[3]]), "|",  
                 deparse(sigma[[2]]),"|",
                 deparse(nu[[2]]),"|",
                 deparse(tau[[2]]))[1]
  ff<- formula(paste(nform, collapse = " "))
  environment(ff) <- globalenv()
  ff
}
################################################################################
################################################################################
################################################################################
################################################################################