
# # gamlss.prepdata 0.1-5

*  the functions `data_part()`, `data_part_list()`, `data_Kfold()`, `data_boot_index()` and `data_boot_weights()` are  added. 

# gamlss.prepdata 0.1-4

* Function `data_leverage()` added.


# gamlss.prepdata 0.1-3

* Functions `data_str()` and `data_exclude_class()` are modified to work with `POSIXct` class.

* Functions `data_dim()` and `data_omit()` are separated.

* Function `data_cut()` added. This function insures that for very large data sets the plotting function like `data_plot()` and `data_xyplot()` only use a random part of the data so they are are working fast.

* Functions `void()` and `data_void()` added.

* Function `data_part_list()` added.

* Function `data_rename()` added.


# gamlss.prepdata 0.1-2

* Partial correlation functiom `data_pcor()` added.

* Replacement of the use of `class()==` with `inherits()`.

* Bug fix in the function `data_re1val()` when the variable was a factor.

# gamlss.prepdata 0.1-8


* the package vignette can be found in <https://mstasinopoulos.github.io/Functions-from-packages/.>    