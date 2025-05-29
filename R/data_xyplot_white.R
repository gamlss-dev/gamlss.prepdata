################################################################################
################################################################################
################################################################################
################################################################################
# function 3
data_xyplot_white <- function(data, response,
                  point.size = 0.5,
                        nrow = NULL,
                  print.info = TRUE,
                        ncol = NULL,
                  percentage, # for big data take % of data
                        seed = 123,
                  max.levels = 10,
              plots.per.page = 9,
                  one.by.one = FALSE,
                        title, 
                text.x.angle = 0, 
                ...)
{
################################################################################
y_distinct <- function(var)
  {
    PP <- length(table(var))
    PP
  }
################################################################################
if (is(data, "list"))
    stop("the data is list  the function needs a data.frame")
if (is(data, "table"))
    stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data[1],"tible"))   data <- as.data.frame(data)
if (is(data, "array"))
    stop("the data is an array the function needs a data.frame")
       Y <- deparse(substitute(response))
if (any(!(Y %in%names(data)))) stop("the response should be in data")
      dv <- y_distinct(data[,Y])
if (dv < max.levels) stop("the response do not seems to have many distinct values")
     data <- if (missing(percentage))
     {
       data_cut(data,seed=seed, print.info=print.info)
     } else data_cut(data,percentage=percentage)
     class_Vars <- sapply(data,function(x) class(x)[1]) 
if (any(class_Vars=="character"))
  {
    chr.pos <- match("character",class_Vars)
       data <- data[,-chr.pos]
  }
        pos <- match(Y, names(data))
      nameS <- names(data)[-pos]
         PP <- list()
       actY <- data[,Y]
         cY <- class(actY)
          I <- 0
text.x.angle <- rep( text.x.angle, length.out=length(nameS))
for (i in nameS)
  {
       I <- I + 1
if (inherits(data[,i],"factor"))
    {
      PP[[I]] <- data|>ggplot2::ggplot(aes(x=.data[[i]],.data[[Y]]))+
        ggplot2::geom_boxplot()+
        ggplot2::theme(axis.text.x = element_text(angle = text.x.angle[I], vjust = 1, hjust=1),
                       panel.background = element_rect(fill = "white"),
                       plot.background = element_rect(fill = "white"),
                       panel.grid.major = element_line(color = "grey80"),
                       panel.grid.minor = element_blank())
    } else
    {
      PP[[I]] <- data|> ggplot2::ggplot(aes(x = .data[[i]], .data[[Y]]))+
        ggplot2::geom_point(size=point.size)+ggplot2::geom_smooth() +
        ggplot2::theme(axis.text.x = element_text(angle = text.x.angle[I], vjust = 1, hjust=1),
                       panel.background = element_rect(fill = "white"),
                       plot.background = element_rect(fill = "white"),
                       panel.grid.major = element_line(color = "grey80"),
                       panel.grid.minor = element_blank())
    }
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
################################################################################
    define_region <- function(row, col){
      viewport(layout.pos.row=row, layout.pos.col=col) }
################################################################################
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
