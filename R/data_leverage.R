################################################################################
################################################################################
################################################################################
################################################################################
# function 3
data_leverage <- function(data, response, weights,
                   quan.val = 0.99,
                   annotate = TRUE,
                   line.col = "steelblue4",
                  point.col = "steelblue4",
                  annot.col = "darkred",
                        plot = TRUE,
                        title, percentage, seed=123,
                  ...)
{
########################################################################
# local functions
gamlss_prep_data <- function (hat, weights, quan.val)
  {
    value <-  quantile(hat, quan.val)
      obs <- seq_len(length(hat))
      hat <- hat[weights!=0]
      out <- data.frame(obs = obs, hat = hat)
out$color <- ifelse((out$hat >= value),
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels =
                               c("normal", "outlier"))
  out$txt <- ifelse(out$color == "outlier", out$obs, NA)
  out$value <- value
    return(out)
  }
#####################################################################
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
  data_cut(data,seed=seed)
} else data_cut(data,percentage=percentage)
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

# if (is(data, "list"))
#     stop("the data is list  the function needs a data.frame")
# if (is(data, "table"))
#     stop("the data is a table the function needs a data.frame")
# if (is(data, "matrix"))    data <- as.data.frame(data)
# if (is(data[1],"mts"))     data <- as.data.frame(data)
# if (is(data, "array"))
#     stop("the data is an array the function needs a data.frame")
          Y <- deparse(substitute(response))
# if (any(!(Y %in%names(data)))) stop("the response should be in data")
if (missing(weights)) weights <- rep(1, dim(data)[1])
        pos <- match(Y, names(daTa))
      nameS <- names(data)[-pos] ## get out the response
         f1 <- formula(paste(paste0(Y,"~"),paste0(nameS, collapse='+')),
                    data=data,      envir=globalenv())#.GlobalEnv
         m1 <- lm(f1, weights=weights, data=data)
        lev <- hatvalues(m1)
          r <- m1$rank
          N <- dim(data)[1]
          d <- gamlss_prep_data(lev,weights=weights, quan.val=quan.val )
     lev
txt.title <- if (missing(title))  paste("Linear leverage of data",
                                        deparse(substitute(data)))
             else title
        obs <-  value <- txt <-  NULL
          f <- d[d$color == "outlier", c("obs", "hat")]
colnames(f) <- c("observation", "quan_resid")
         gg <- ggplot2::ggplot(d, ggplot2::aes(x = obs, y = hat, label = txt,
                                               ymin = 0, ymax = hat)) +
           ggplot2::geom_linerange(colour = line.col ) +
           ggplot2::geom_point(shape = 1, colour = point.col  ) +
           ggplot2::xlab("Observation number") + # working  with facet_wrap
           ggplot2::ylab("linear leverage") + # working  with facet_wrap
           ggplot2::ggtitle(txt.title) +  # working  with facet_wrap
           ggplot2::geom_text(hjust = -0.2, nudge_x = 0.15, size = 3,
                         family = "serif",
                 fontface = "italic", colour = annot.col, na.rm = TRUE)

     p <- gg +
       ggplot2::geom_hline(yintercept=2*(r/N), col=annot.col)+
       if (annotate) ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.5,
                                       vjust = d$value,
                  family = "serif", fontface = "italic", colour = annot.col,
                              label = paste0("Threshold: abs(", d$value, ")"))
     if (plot) {
       suppressWarnings(return(p))
     }
     else {
       return(list(leverage = d$hat, index=d$obs, threshold = d$value))
     }
}
################################################################################
################################################################################
################################################################################
################################################################################
