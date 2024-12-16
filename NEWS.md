# Introduction

The package `gamlss.prepdata` is a `R` package preparing data to be used to fit the Generalised Additive Models for Location, Scale and Shape (GAMLSS) of Rigby and Stasinopoulos (2005),  Appl. Statist., 54,  pp. 507-554). It supplements the packages `gamlss`,  `gamlss2` and `gamlss.ggplots`.

There are three book available for more information about in general GAMLSS; 

 1) "Flexible Regression and Smoothing: Using GAMLSS in R" 
explaining how the models can be used in R.

2) 
"Distributions for modeling location, scale and shape: Using GAMLSS in R" 
explaining the explicit and generated distributions available in the 
package gamlss.dist  

3)  
"Generized Additive Models for Location Scale and Shape: A distributional 
regression  approach with applications" 
explaining the different method for fitting GAMLSS i.e. penalised Likelihood, Bayesian and Boosting.  
 
More more information about books and papers related to GAMLSS can be found in
<https://www.gamlss.com/>.
 
 
The GitHub repository is now hosted under the new `gamlss-dev` organization:
  <https://github.com/gamlss-dev/gamlss/>.


# Version 0.1-2

* `data_pcor()` is added to the package 25--3-24

* replacement  of the use of `class()==` with `inherits()` is done 

* a bug in the function `data_re1val()` when the variable was a factor has amended.

