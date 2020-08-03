#' Normality test for a data frame.
#'
#' This function performs the Shapiro Wilk test for all the numerical variables of a given data frame. Each test can be accompanied by a graphical presentation  (density and qqplot) giving a piece of visual information.
#'
#' @param df data.frame to be tested
#' @param output if 'none', nothing is displayed, if 'figure', a figure is displayed, if 'message' a message is displayed in the console and if 'all' a message and a figure are displayed.
#'
#' @return A list with the following elements:
#' * `pvalues` a vector of the p-values obtained with the Shapiro Wilk test for all numerical variables in the data frame `df`.
#' * `results` a text (printed onto the console, if wanted) describing the results of the performed tests.
#'
#' @examples
#' normality.df(iris)
#'
#' ## install.packages("kohonen")
#' data(wines, package = "kohonen")
#' normality.df(wines, "all")
#'
#'
#' @export
#'
#'

normality.df <- function(df, output = c("none", "figure", "message", "all")) {
  try(df<-data.frame(df), silent = TRUE)
  if (!exists("df", mode = "list")) stop("Please enter a valid data.frame!")
  output <- match.arg(output)
  pvalues <- c()
  results <- c()
  for (i in 1:ncol(df)) {
    res <- test.normality(df[, i], output = output, var.name = names(df)[i])
    pvaluei <- res$pval
    resulti <- res$result
    names(pvaluei) <- names(df)[i]
    names(resulti) <- names(df)[i]
    pvalues <- c(pvalues, pvaluei)
    results <- c(results, resulti)
  }
  return(list(pvalues = pvalues, results = results))
}
