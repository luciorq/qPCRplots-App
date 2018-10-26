#' \code{qPCRplots} package
#'
#' Shiny app intended in helping process and visualize output from quantitative polymerase chain reaction experiment
#'
#' See the README on
#' \href{https://github.com/luciorq/qPCRplots#readme}{GitHub}
#'
#' @docType package
#' @name qPCRplots
#' @importFrom dplyr %>%
#'
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## reference: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))