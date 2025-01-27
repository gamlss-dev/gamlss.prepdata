# Introduction

The package `gamlss.prepdata` is a `R` package preparing data to be used to fit the Generalised Additive Models for Location, Scale and Shape (GAMLSS) of Rigby and Stasinopoulos (2005),  Appl. Statist., 54,  pp. 507-554). It supplements the packages `gamlss`,  `gamlss2` and `gamlss.ggplots`.

There are three book available for more information about GAMLSS; 

 1) "Flexible Regression and Smoothing: Using GAMLSS in R" 
explaining how the models can be used in R.

2) 
"Distributions for modeling location, scale and shape: Using GAMLSS in R" 
explaining the explicit and generated distributions available in the 
package gamlss.dist  

3)  
"Generalized Additive Models for Location Scale and Shape: A distributional 
regression  approach with applications" 
explaining the different method for fitting GAMLSS i.e. penalised Likelihood, Bayesian and Boosting.  
 
More more information about books and papers related to GAMLSS can be found in
<https://www.gamlss.com/>.
 
 
The GitHub repository is now hosted under the new `gamlss-dev` organization:
  <https://github.com/gamlss-dev/gamlss/>.

# Version 0.1-3

* the functions `data_str()` and `data_exclude_class()`  are modified to work with `POSIXct` class. 

* the functions `data_dim()` and `data_omit()` are separated. 

* function `data_cut()` is added. This function insures that for very large data sets the plotting function like `data_plot()` and `data_xyplot()` only use a random part of the data so they are are working fast. 

* the functions `void()` and `data_void()` are in.

* the function `data_part_list()` is added.

* the function `data_rename()` is added.


# Version 0.1-2

* the partial correlation functiom is `data_pcor()` is added to the package 25--3-24

* replacement  of the use of `class()==` with `inherits()` is done 

* a bug in the function `data_re1val()` when the variable was a factor has amended.

