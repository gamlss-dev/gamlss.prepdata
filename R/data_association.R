################################################################################
################################################################################
################################################################################
################################################################################
#require(tidyverse)
#require(rcompanion)
################################################################################
################################################################################
################################################################################
################################################################################
# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
# data_assoc = function(data, cor_method="spearman", adjust_cramersv_bias=TRUE)
#   {
#   data_comb = expand.grid(names(data), names(data),  stringsAsFactors = F) |> 
#     stats::setNames(c("X1", "X2"))
# ######### local functions. #####################################################  
#   is_nominal <- function(x) class(x) %in% c("factor", "character")
# ################################################################################  
#   is_numeric <- function(x) { is.integer(x) || is_double(x)}
# ################################################################################  
#   f = function(xName,yName) 
#     {
#     
#     x =   data[,xName] # pull(data, xName) #
#     y =   data[,yName] # pull(data, yName) #
#     
#     result = if(is_nominal(x) && is_nominal(y)){
#       # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
#       cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
#       data.frame(xName, yName, assoc=cv, type="cramersV")
#       
#     }else if(is_numeric(x) && is_numeric(y)){
#       correlation = cor(x, y, method=cor_method, use="complete.obs")
#       data.frame(xName, yName, assoc=correlation, type="correlation")
#       
#     }else if(is_numeric(x) && is_nominal(y)){
#       # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
#       r_squared = summary(lm(x ~ y))$r.squared
#       data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
#       
#     }else if(is_nominal(x) && is_numeric(y)){
#       r_squared = summary(lm(y ~x))$r.squared
#       data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
#       
#     }else {
#       warning(paste("unmatched column type combination: ", class(x), class(y)))
#     }
#     
#     # finally add complete obs number and ratio to table
#     result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
#   } # end of function 
# ################################################################################  
# # apply function to each variable combination
#   map2_df(data_comb$X1, data_comb$X2, f)
# }
#da<-data_association(rent)
#head(da)
# da |>
#   ggplot(aes(x,y))+
#   geom_tile()+
#   geom_text(aes(x,y,label=cramV))+
#   scale_fill_gradient(low="red", high="yellow")+
#   theme_classic()

################################################################################
################################################################################
################################################################################
################################################################################
# require(corrr)
# 
# msleep %>%
#   select(- name) %>%
#   mixed_assoc() %>%
#   select(x, y, assoc) %>%
#   spread(y, assoc) %>%
#   column_to_rownames("x") %>%
#   as.matrix %>%
#   as_cordf %>%
#   network_plot()
# 
# rent99 %>%
#   select(- c(rentsqm, district)) %>%
#   mixed_assoc() %>%
#   select(x, y, assoc) %>%
#   spread(y, assoc) %>%
#   column_to_rownames("x") %>%
#   as.matrix %>%
#   as_cordf %>%
#   network_plot()
# 
 #   rent99 %>%
 #   select(- district) %>%
 #   data_association() %>%
 #   select(x, y, assoc) %>%   
 #  spread(y, assoc) %>% ##   pivot_wider(y, assoc)
 #  column_to_rownames("x") %>%
 #   as.matrix %>%
 #   as_cordf %>% rplot()
 # #  network_plot()
# 
 
# rent %>%
#   select(- c(Sp, Sm)) %>%
#   mixed_assoc() %>%
#   select(x, y, assoc) %>%
#   spread(y, assoc) %>%
#   column_to_rownames("x") %>%
#   as.matrix %>%
#   as_cordf %>%
#   network_plot()
# 
# ################################################################################
# ################################################################################
# ################################################################################
# ################################################################################
# # example dataframe
# df <- data.frame(
#   group = c('A', 'A', 'A', 'A', 'A', 'B', 'C'),
#   student = c('01', '01', '01', '02', '02', '01', '02'),
#   exam_pass = c('Y', 'N', 'Y', 'N', 'Y', 'Y', 'N'),
#   subject = c('Math', 'Science', 'Japanese', 'Math', 'Science', 'Japanese', 'Math')
# ) 
# 
# library(tidyverse)
# library(lsr)
# 
# # function to get chi square p value and Cramers V
# f = function(x,y) {
#   tbl = df %>% select(x,y) %>% table()
#   chisq_pval = round(chisq.test(tbl)$p.value, 4)
#   cramV = round(cramersV(tbl), 4) 
#   data.frame(x, y, chisq_pval, cramV) }
# 
# # create unique combinations of column names
# # sorting will help getting a better plot (upper triangular)
# df_comb = data.frame(t(combn(sort(names(df)), 2)), stringsAsFactors = F)
# 
# # apply function to each variable combination
# df_res = map2_df(df_comb$X1, df_comb$X2, f)
# 
# # plot results
# df_res %>%
#   ggplot(aes(x,y,fill=chisq_pval))+
#   geom_tile()+
#   geom_text(aes(x,y,label=cramV))+
#   scale_fill_gradient(low="red", high="yellow")+
#   theme_classic()
# 
################################################################################
################################################################################
################################################################################
################################################################################
# require(tidyverse)
# require(rcompanion)
# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
################################################################################
################################################################################
################################################################################
################################################################################ 
# data_assoc_df = function(df, cor_method="spearman",
#             adjust_cramersv_bias = TRUE)
#   {
#   df_comb = expand.grid(names(df), names(df),
#       stringsAsFactors = F) |> set_names("X1", "X2")
# ################################################################################
#   is_nominal = function(x) class(x) %in% c("factor", "character")
#   # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
#   # https://github.com/r-lib/rlang/issues/781
#   #is_numeric <- function(x) { is.integer(x) || is_double(x)}
# ################################################################################
#   f = function(xName,yName)
#     {
#        x <-  pull(df, xName)
#        y <-  pull(df, yName)
# 
#     result = if(is_nominal(x) && is_nominal(y)){
#     # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
#       cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
#       names(cv) <- NULL
#       data.frame(xName, yName, assoc=cv, type="cramersV")
# 
#     }else if(is.numeric(x) && is.numeric(y)){
#       correlation = cor(x, y, method=cor_method, use="complete.obs")
#       data.frame(xName, yName, assoc=correlation, type="correlation")
# 
#     }else if(is.numeric(x) && is_nominal(y)){
#       # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
#       r_squared = summary(lm(x ~ y))$r.squared
#       data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
# 
#     }else if(is_nominal(x) && is.numeric(y)){
#       r_squared = summary(lm(y ~x))$r.squared
#       data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
# 
#     }else {
#       warning(paste("unmatched column type combination: ", class(x), class(y)))
#     }
# 
#     # finally add complete obs number and ratio to table
#     result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
#   }

  # apply function to each variable combination
#   map2_df(df_comb$X1, df_comb$X2, f)
# }
################################################################################
################################################################################
################################################################################
################################################################################
# library(corrr)
# #install.packages("corrr")
# dd <- mixed_assoc(rent)
# 
# rent |>
#   select(- R) |>
#   mixed_assoc() |>
#   select(x, y, assoc) |>
#   spread(y, assoc) |>
#   column_to_rownames("x") %>%
#   as.matrix %>%
#   as_cordf %>%
#   network_plot()
# 
# rent99 |>
#   select(- rent) |>
#   mixed_assoc() |>
#   select(x, y, assoc) |>
#   spread(y, assoc) |>
#   column_to_rownames("x") %>%
#   as.matrix %>%
#   as_cordf %>%
#   network_plot()

 # da |>
 #    data_association() |>
 #    select(x, y, assoc) |>
 #    spread(y, assoc) |>
 #    column_to_rownames("x") %>%
 #    as.matrix %>%
 #    as_cordf %>%
 #    network_plot()
 #  
# ################################################################################
# # example dataframe
# df <- data.frame(
#   group = c('A', 'A', 'A', 'A', 'A', 'B', 'C'),
#   student = c('01', '01', '01', '02', '02', '01', '02'),
#   exam_pass = c('Y', 'N', 'Y', 'N', 'Y', 'Y', 'N'),
#   subject = c('Math', 'Science', 'Japanese', 'Math', 'Science', 'Japanese', 'Math')
# ) 
# df
# ################################################################################
# library(tidyverse)
# library(lsr)
# 
# # function to get chi square p value and Cramers V
# f = function(x,y) {
#   tbl = df %>% select(x,y) %>% table()
#   chisq_pval = round(chisq.test(tbl)$p.value, 4)
#   cramV = round(cramersV(tbl), 4) 
#   data.frame(x, y, chisq_pval, cramV) }
# 
# # create unique combinations of column names
# # sorting will help getting a better plot (upper triangular)
# df_comb = data.frame(t(combn(sort(names(df)), 2)), stringsAsFactors = F)
# 
# # apply function to each variable combination
# df_res = map2_df(df_comb$X1, df_comb$X2, f)
#                                                         
# # plot results
# df_res %>%
#   ggplot(aes(x,y,fill=chisq_pval))+
#   geom_tile()+
#   geom_text(aes(x,y,label=cramV))+
#   scale_fill_gradient(low="red", high="yellow")+
#   theme_classic()
################################################################################
################################################################################
################################################################################
################################################################################   
association <-  function(x, y, xname=NULL, yname=NULL, data,
              cor_method="spearman", adjust_cramersv_bias=TRUE)
{
######### local functions 1 ####################################################
  is_nominal <- function(x) class(x) %in% c("factor", "character")
######### local  function 2 ####################################################
#  is_numeric <- function(x) { is.integer(x) || is_double(x)}
################################################################################
if (is.null(xname))  xname <- deparse(substitute(x))
if (is.null(yname))  yname <- deparse(substitute(y))
if (!missing(data))
{
  x <- data[,xname] #
  y <- data[,yname] #
}
  result <- if(is_nominal(x) && is_nominal(y)){
           cv  <-  cramer_phi(as.character(x), as.character(y), 
                           bias.correct = adjust_cramersv_bias)
     names(cv) <- NULL
  data.frame(xname, yname, assoc=cv, type="cramersV")
  }else if(is.numeric(x) && is.numeric(y)){
    correlation = abs(cor(x, y, method=cor_method, use="complete.obs"))
    data.frame(xname, yname, assoc=correlation, type="correlation")
  }else if(is.numeric(x) && is_nominal(y)){
    r_squared = summary(lm(x ~ y))$r.squared
    data.frame(xname, yname, assoc=sqrt(r_squared), type="anova")
  }else if(is_nominal(x) && is.numeric(y)){
    r_squared = summary(lm(y ~x))$r.squared
    data.frame(xname, yname, assoc=sqrt(r_squared), type="anova")
  }else {
    warning(paste("unmatched column type combination: ", class(x), class(y)))
  }
result
} # end of function
################################################################################
################################################################################
################################################################################
################################################################################ 
data_association <- function(data,
                      digits = 3,
                        plot = TRUE,
                    diag.off = TRUE,
               lower.tri.off = FALSE,
                      method = c("square", "circle"),
               outline.color = "gray",
                      colors = c("blue", "white", "red"),
                legend.title = "Assoc",
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
                  percentage
)
{
################################################################################
################################################################################
#  require(foreach)
################################################################################
    method <- match.arg(method)
 #   colors <- match.arg(colors)
# data.frame missing
    i <- j <- NULL
  nameData <- deparse(substitute(data))
if (missing(data) || NROW(data) <= 1)
    stop("nothing to do for this data frame")
if (is(data, "list"))  stop("the data is list  the function needs a data.frame")
if (is(data, "table")) stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array")) stop("the data is an array the function needs a data.frame")
     dimD <- dim(data)
     data <- if (missing(percentage))
  {
    data_cut(data,seed=seed)
  }      else data_cut(data,percentage=percentage)
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
     data |> as.data.frame() -> daTa #
   cnames <- names(daTa)
  lcnames <- length(cnames)
       CC <- matrix(0, ncol=lcnames, nrow=lcnames)
  i_index <- col(CC)[upper.tri(CC)]
  j_index <- row(CC)[upper.tri(CC)]
    liind <- length(i_index)
    ljind <- length(i_index)
       Cv <- foreach(i=1:liind, .combine=rbind) %do%
    {
      ii <- i_index[i]
      jj <- j_index[i]
      ni <- names(daTa)[ii]
      nj <- names(daTa)[jj]
      xi <- daTa[,ni]
      xj <- daTa[,nj]
      Cv <- association(xi, xj, xname=ni, yname=nj)
    }
    # Cv <- as.data.frame(Cv)
    CCC <- as.vector(Cv$assoc)
for (i in 1:liind)
{
          ii <- j_index[i]
          jj <- i_index[i]
CC[ ii, jj ] <- CCC[i]
}
   CC <- CC+t(CC)  # to get the full matrix (rather than diagonal)
     diag(CC) <- 1
 rownames(CC) <- cnames
 colnames(CC) <- cnames
## The matrix CC can be exported now
 CC <- base::round(x = CC, digits = digits)
if (plot==FALSE) return(CC)
if ( diag.off) diag(CC) <- NA
if (lower.tri.off)  CC[lower.tri(CC)]<-NA
 txt.title <- if (missing(title))
         paste("Associations from data", nameData)
 else title
    method <- match.arg(method)
      corr <- mat2df(CC)
     var_1 <-  var_2 <-  value <- NULL
 colnames(corr) <- c("var_1", "var_2", "value")
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
                mid = colors[2], midpoint = 0.5, limit = c(0, 1), space = "Lab",
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
cramer_phi <- function (x, y, 
                    digits = 4, 
              bias.correct = FALSE, 
          ...) 
{
    CV <- NULL
if (is.factor(x)) 
  { x <- as.vector(x)}
if (is.factor(y)) 
  { y <- as.vector(y)}
if (is.vector(x) & is.vector(y)) 
  {
             N <- length(x)
        Chi.sq <- suppressWarnings(chisq.test(x, y, correct = FALSE, 
                                                     ...)$statistic)
           Phi <- Chi.sq/N
           Row <- length(unique(x))
             C <- length(unique(y))
            CV <- sqrt(Phi/min(Row - 1, C - 1))
  }
        PhiOrg <- Phi
          VOrg <- CV
        PhiNew <- NA
          VNew <- NA
if (bias.correct) 
{
                Phi <- max(0, Phi - ((Row - 1) * (C - 1)/(N - 1)))
                CC <- C - ((C - 1)^2/(N - 1))
                RR <- Row - ((Row - 1)^2/(N - 1))
                CV <- sqrt(Phi/min(RR - 1, CC - 1))
            PhiNew <- Phi
              VNew <- CV
            PhiNew <- max(0, Phi - ((Row - 1) * (C - 1)/(N - 1)))
                CC <- C - ((C - 1)^2/(N - 1))
                RR <- Row - ((Row - 1)^2/(N - 1))
                CV <- sqrt(Phi/min(RR - 1, CC - 1)) 
}
               CV <- signif(as.numeric(CV), digits = digits)
         names(CV) <- "Cramer V"
        return(CV)
       
}