#' @importFrom Rcpp loadModule
#' @useDynLib metabcpp
NULL

loadModule(
   module = "mod_metab",
   what = TRUE
)
