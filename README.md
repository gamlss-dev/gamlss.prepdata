

<!-- README.md is generated from README.qmd via: quarto render README.qmd --to gfm -->

# gamlss.prepdata: Preparing Data for Distributional Regression

## Overview

The purpose of this package is to provide functions to facilitate the
creation of `data.frame`’s suitable for statistical modeling analysis
and especially distributional regression models using
[`gamlss`](https://cran.R-project.org/package=gamlss) and.
[`gamlss2`](https://github.com/gamlss-dev/gamlss2) packages.

There is a lot of information which can be gain using a preliminary data
analysis. One could seeking information about the *variables* in the
data set themselves, on *outliers*, on *associations* between variables,
what *type* of relationships exist between the response and the
explanatory variables (linear or not linear) and possible *interactions*
between the explanatory variables. In addition, one, at this stage,
could decide appropriate *partitions* of to maximize statistical
inference. While answers to those questions are not necessarily final,
at this pre-modeling stage, they could help at the next stage of the
fitting process. All functions in the package can be used before the
functions or are used for modeling.

## Installation

The package is not yet on CRAN but can be installed from
[R-universe](https://gamlss-dev.R-universe.dev/).

``` r
install.packages("gamlss.prepdata", repos = "https://gamlss-dev.R-universe.dev")
```

## Functions

The functions for manipulation of variables are shown below

| Functions                | Usage                                  |
|:-------------------------|:---------------------------------------|
| `data_dim()`             | Dimensions & % of omitted observations |
| `data_names()`           | Names of the data                      |
| `data_distrinct()`       | Distinct values in variables           |
| `data_which(()`          | NA’s in variables                      |
| `data_str()`.            | The class of variables etc.            |
| `data_omit()`            | Omit all the `NA`s                     |
| `data_char2fac()`        | From characters to factors             |
| `data_few2fac()`         | From few distinct obs. to factors      |
| `data_int2num()`         | From integers to numeric               |
| `data _rm()`             | Remove variables                       |
| `data _rm1val()`         | Remove factors with one level          |
| `data _rename()`         | Rename variables                       |
| `data _renamove()`       | Remove variables                       |
| `data _select()`         | Select variables                       |
| `data _exclude_class()`  | Exclude a specified class              |
| `data _only_continous()` | Includes only continuous               |
| `data_rmNAvars()`        | Remove variables with NA values        |
| `data_fac2num()`         | Make factor numeric                    |

The function for graphics are show below

| Functions           | Usage                                                |
|:--------------------|:-----------------------------------------------------|
| `data_plot()`       | Univariate plots of all variables                    |
| `data_xyplot()`     | Pairwise plots of the response against all others    |
| `data_bucket()`     | Bucket plots of all variables                        |
| `data_cor(()`       | Pairwise correlations                                |
| `data_pcor(()`      | Pairwise partial correlations                        |
| `data_void()`       | Pairwise % of empty spaces                           |
| `data_inter()`      | Pairwise interactions                                |
| `data_response()`   | Response variable plots                              |
| `data_zscores()`    | Univariate plots using z-scores                      |
| `data_outliers()`   | Univariate detection of outliers                     |
| `data_leverage()`   | Detection of outliers in the x’s space               |
| `data_scale()`      | Univariate scaling the x’s                           |
| `data_trans_plot()` | Checking for univivariate transformations in the x’s |

## Graphics

Here some of the graphical functions examples

``` r
library("gamlss")
library("gamlss2")
library("ggplot2")
library("gamlss.ggplots")
library("gamlss.prepdata")
library("dplyr") 
packageVersion("gamlss.prepdata")
## [1] '0.1.5'
da <- data_rm(rent99, c(2, 9)) 
dim(rent99)
## [1] 3082    9
dim(da)
## [1] 3082    7
```

## `data_plot()`

The function `data_plot` plots all the variable of the data
individually. It plots;

- the continuous variable as *histograms* with a density plots
  superimposed, (see the plots for `rent` and `yearc` below).
  Alternatively a *dot plots* can be requested, (see the example in
  <a href="#sec-data_xyplot" class="quarto-xref">Section 1.6</a>).

- the integers as *needle* plots, (see the plot for `area` below).

- the categorical variables, as *bar* plots, (see the plots for
  `location`, `bath` `kitchen` and `cheating` below).

The message `100 % of data are saved` below is the result of the
function `data_cut()` which is use before any `ggplot2` plot.

``` r
da |> data_plot()
##  100 % of data are saved, 
## that is, 3082 observations.
```

<img src="man/figures/README-data_plot-1.png" data-fig-align="center" />

The function could saves the `ggplot2` figures.

## `data_xyplot()`

The functions `data_xyplot()` plots the response variable against each
of the independent explanatory variables. It plots the continuous
against continuous as *scatter* plots and continuous variables against
categorical as *box* plot.

> [!WARNING]
>
> At the moment there is no provision for categorical response
> variables.

``` r
da |> data_xyplot(response=rent )
##  100 % of data are saved, 
## that is, 3082 observations.
```

<img src="man/figures/README-data_xyplot-1.png"
data-fig-align="center" />

The output of the function saves the `ggplot2` figures.

## `data_bucket()`

The function `data_bucket` can be used to identifies hight skewness and
kurtosis on continuous variables in the data. Note that if the
continuous variable is normaly distribueterd looking should be in the
center of the figure.

``` r
data_bucket(da, response=rent )
##  100 % of data are saved, 
## that is, 3082 observations. 
##     rent     area    yearc location     bath  kitchen cheating 
##     2723      132       68        3        2        2        2
```

<img src="man/figures/README-data_bucket-1.png"
data-fig-align="center" />
