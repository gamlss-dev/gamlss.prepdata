################################################################################
################################################################################
################################################################################
# correlations coefficients from  a data.frame
# it check whether they are factor and only show the
# correlation for continuous variables
# i) data_cor(): produce the correlation of all continuous variable in the data 
# ii) data_hight_corr; It takes a data frame and identify all pairs with hight
#     Pearson's correlation 
# iii) high_cor(): it takes a correlation matrix and identify the hight values
# iv) lower_cor(): it takes a correlation matrix and identify the lower values
#  v)  cor_vars(): it takes  a correlation matrix it performs the algorithm from 
#      the `caret` package to identify which variables could be excluded.
       
#
################################################################################
################################################################################
################################################################################
#require(igraph)
#require(networkD3)
################################################################################
################################################################################
################################################################################
################################################################################
# function 1
data_cor <- function(data,
                     digits = 3,
                       plot = TRUE,
                   diag.off = TRUE,
              lower.tri.off = FALSE,
                     method = c("square", "circle"),
                       type = c("pearson", "kendall", "spearman"),
              outline.color = "gray",
                     colors = c("blue", "white", "red"),
               legend.title = "Corr",
                      title,
                    ggtheme = theme_minimal(),
                     tl.cex = 12,
                     tl.col = "black",
                     tl.srt = 45,
                        lab = TRUE,
                    lab_col = "black",
                   lab_size = 3,
                circle.size = 20,
                       seed = 123,
              percentage,
              print.info = TRUE)
{
################################################################################
################################################################################
  type <- match.arg(type)
# data.frame missing
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
if (dimD[1] < 20)   stop(cat("the size of the data set is too small", "\n",
                                "to detect non-linear correlations", "\n"))
        daTa <- subset(data,  select=ifelse(sapply(data,is.factor)|
                sapply(data,is.character)==TRUE, FALSE, TRUE))
        Dim <- dim(daTa)
if (Dim[2]==0) stop("no variable is left after taking out the factors")
if (Dim[2]==1) stop("only one variable is left after taking out the factors")
        diffDim  <- dimD[2]-Dim[2]
if (any(is.na(data)))
        {
            l1 <- dim(data)[1]
          data <- na.omit(data)
            l2 <- dim(data)[1]
          warning(cat(l1-l2, "observations were omitted from the data", "\n"))
        }
if (is.null(dimD)) stop("only one variable in the data")
if (diffDim > 0)
               {
               warning(cat(diffDim, 'factors have been omited from the data', "\n"))
}
              CC <- cor(daTa, method=type)
              CC <- base::round(x = CC, digits = digits)
if (plot==FALSE) return(CC)
if ( diag.off) diag(CC) <- NA
if (lower.tri.off)  CC[lower.tri(CC)]<-NA
              txt.title <- if (missing(title))
                paste(type,"correlations, data",nameData)
              else title
          method <- match.arg(method)
            corr <- mat2df(CC)
            var_1 <-  var_2 <-  value <- NULL
  colnames(corr) <- c("var_1", "var_2", "value")
       txt.title <- if (missing(title))
                paste(type,"correlations, data",nameData)
  else title
   corr$abs_corr <- abs(corr$value) * 10
               p <- ggplot2::ggplot(data = corr,
                ggplot2::aes(x = .data[["var_1"]], y = .data[["var_2"]],
                                        fill = .data[["value"]]))
if (method == "square") {
               p <- p + ggplot2::geom_tile(color = outline.color)
  }
else if (method == "circle") {
    p <- p + ggplot2::geom_point(color = outline.color, shape = 21,
             ggplot2::aes(size = .data[["abs_corr"]])) +
            # scale_size(range = c(5, 20)) +
      ggplot2::scale_size_area(max_size = circle.size) +
      ggplot2::guides(size = "none")
}
      label <- round(x = CC, digits = digits)
  p <- p + ggplot2::scale_fill_gradient2(low = colors[1], high = colors[3],
            mid = colors[2], midpoint = 0, limit = c(-1, 1), space = "Lab",
            name = legend.title)+
            ggplot2::ggtitle(txt.title)
  if (inherits(ggtheme[[1]], "function")) {
    p <- p + ggtheme
  }
  else if (inherits(ggtheme[[1]],"theme")) {
    p <- p + ggtheme
  }
  p <- p + ggplot2::theme(axis.text.x = element_text(angle = tl.srt,
                vjust = 1, size = tl.cex, , colour=tl.col, hjust = 1),
                axis.text.y = element_text(size = tl.cex, colour=tl.col)) +
                ggplot2::coord_fixed()
  label <- round(x = corr[, "value"], digits = digits)
  if (lab) {
    p <- p + ggplot2::geom_text(mapping = ggplot2::aes(x = .data[["var_1"]],
                                                     y = .data[["var_2"]]),
                  label = label, color = lab_col, size = lab_size)
  }
  p
}
################################################################################
################################################################################
################################################################################
################################################################################
#-------------------------------------------------------------
# function 2
data_high_cor <- function(data, r=.90, digits=3, plot=FALSE, igraph=TRUE)
{
if (abs(r)>=1||abs(r)<=0) stop("r should be greater than  0 and lass than 1")
  daTa <- subset(data,  select=ifelse(sapply(data,is.factor)|sapply(data,is.character)==TRUE, FALSE, TRUE))
   Dim <- dim(daTa)
    CC <- cor(daTa)
    CC <- base::round(x = CC, digits = digits)
   CCC <- CC-diag(rep(1,Dim[2]))
if (is.null(colnames(daTa))) colnames(daTa) <- paste0("X", seq(1:dim(data)[2]))
if (!any(which(abs(CCC)>r))) return(cat("no correlation above", r, "\n"))
    mm <- which(abs(CCC)>r, arr.ind=T)
    nn <- mm[mm[,1]< mm[,2],]
if (is.vector(nn))
  {
    name1 <- colnames(data)[nn[1]]
    name2 <- colnames(data)[nn[2]]
    corrs <- CCC[nn[1],nn[2]]
  } else
  {
   name1 <- colnames(data)[nn[,1]]
   name2 <- colnames(data)[nn[,2]]
   corrs <- CCC[nn]
  }
M <-  cbind(name1, name2, corrs)
if (plot)
{
 # dd <- which.Data.Corr(InfMort, r=0.5)[,c(1,2)]
#  network <- graph_from_data_frame(d=dd, directed=F)
  # plot it
#  plot(network)
#  InfMort |> which.Data.Corr( r=0.5) |>  as.data.frame() |>
#    simpleNetwork(CC, height="100px", width="100px")
  dd <- as.data.frame(M)
 if (igraph) plot(igraph::graph_from_data_frame(d=dd, directed=F))
  else{
    p <- networkD3::simpleNetwork(dd, height="100px", width="100px")
    print(p)
  }
} else
  return(M)
}
################################################################################
################################################################################
################################################################################
################################################################################
high_val <- function(table, val=.90, digits=3, plot=FALSE, igraph=TRUE)
{
  if (abs(val)>=1||abs(val)<=0) stop("val should be greater than  0 and lass than 1")
  mm <- which(abs(table)>val, arr.ind=T)
  nn <- mm[mm[,1]< mm[,2],]
  if (is.vector(nn))
  {
    name1 <- colnames(table)[nn[1]]
    name2 <- colnames(table)[nn[2]]
    corrs <- table[nn[1],nn[2]]
  } else
  {
    name1 <- colnames(table)[nn[,1]]
    name2 <- colnames(table)[nn[,2]]
    corrs <- table[nn]
  }
  M <-  cbind(name1, name2, corrs)
  if (plot)
  {
    # dd <- which.Data.Corr(InfMort, r=0.5)[,c(1,2)]
    #  network <- graph_from_data_frame(d=dd, directed=F)
    # plot it
    #  plot(network)
    #  InfMort |> which.Data.Corr( r=0.5) |>  as.data.frame() |>
    #    simpleNetwork(CC, height="100px", width="100px")
    dd <- as.data.frame(M)
    if (igraph) plot(igraph::graph_from_data_frame(d=dd, directed=F))
    else{
      p <- networkD3::simpleNetwork(dd, height="100px", width="100px")
      print(p)
    }
  } else
    return(M)
}
################################################################################
################################################################################
################################################################################
################################################################################
low_val <- function(table, val=.05, digits=3, plot=FALSE, igraph=TRUE)
{
  if (abs(val)>=1||abs(val)<=0) stop("val should be greater than  0 and lass than 1")
  mm <- which(abs(table)< val, arr.ind=T)
  nn <- mm[mm[,1]< mm[,2],]
  if (is.vector(nn))
  {
    name1 <- colnames(table)[nn[1]]
    name2 <- colnames(table)[nn[2]]
    corrs <- table[nn[1],nn[2]]
  } else
  {
    name1 <- colnames(table)[nn[,1]]
    name2 <- colnames(table)[nn[,2]]
    corrs <- table[nn]
  }
  M <-  cbind(name1, name2, value=corrs)
  if (plot)
  {
    dd <- as.data.frame(M)
    if (igraph) plot(igraph::graph_from_data_frame(d=dd, directed=F))
    else{
      p <- networkD3::simpleNetwork(dd, height="100px", width="100px")
      print(p)
    }
  } else
    return(M)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 5 (identical to `carer` function findCorrelation())
cor_vars <- function (x, cutoff = 0.9, verbose = FALSE) 
{
# identical to `caret` function `findCorrelation()`   
if (any(!complete.cases(x))) 
        stop("The correlation matrix has some missing values.")
     averageCorr <- colMeans(abs(x))
     averageCorr <- as.numeric(as.factor(averageCorr))
x[lower.tri(x, diag = TRUE)] <- NA
combsAboveCutoff <- which(abs(x) > cutoff)
     colsToCheck <- ceiling(combsAboveCutoff/nrow(x))
     rowsToCheck <- combsAboveCutoff%%nrow(x)
   colsToDiscard <- averageCorr[colsToCheck] > averageCorr[rowsToCheck]
  rowsToDiscard <- !colsToDiscard
  if (verbose) {
    colsFlagged <- pmin(ifelse(colsToDiscard, colsToCheck, 
                               NA), ifelse(rowsToDiscard, rowsToCheck, NA), na.rm = TRUE)
    values <- round(x[combsAboveCutoff], 3)
    cat("\n", paste("Combination row", rowsToCheck, "and column", 
                    colsToCheck, "is above the cut-off, value =", values, 
                    "\n \t Flagging column", colsFlagged, "\n"))
  }
  deletecol <- c(colsToCheck[colsToDiscard], rowsToCheck[rowsToDiscard])
  deletecol <- unique(deletecol)
  deletecol
}
################################################################################
################################################################################
################################################################################
################################################################################
data_index_cor <- function (data, response, cutoff = 0.9,  
                            type = c("pearson", "kendall", "spearman"),
                            percentage = 1, print.info=FALSE)
{
 type <- match.arg(type)
  CC <- data_cor(data, type=type, plot=FALSE, percentage=percentage,  print.info = print.info) 
  #CC <- cor(daTa, method=type)
  index <- cor_vars(CC, cutoff=cutoff)
  return(index)
}

################################################################################
################################################################################
################################################################################
################################################################################
data_index_association <- function (data, response, cutoff = 0.9,  
                                 percentage = percentage , print.info=FALSE )
{
  CC <- data_association(data, plot=FALSE, percentage=percentage,  print.info = print.info) 
  index <- cor_vars(CC, cutoff=cutoff)
  return(index)
}
################################################################################
################################################################################
################################################################################
################################################################################
