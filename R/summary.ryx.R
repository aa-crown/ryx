#'@title Gives a summary of results from ryx function.
#'@description Prints a summary of results from ryx function detailing the median absolute correlation value with ranges, as well as how many variables were significant at the p < 0.05 level.
#'@export
#'@param results results from ryx()
#'@param ... not used
#'@returns A string of summarized results from ryx, including variables used, median absolute correlation value and range, and how many significant variables were found in the analysis.
#'@examples
#'x <- ryx(mtcars, y="mpg", x=c("hp", "wt", "disp", "cyl", "am", "gear"))
#'summary(x)

summary.ryx <- function(results, ...) {
  if (!inherits(results, "ryx")) {
    stop("This function requires an object created by ryx")
  }

  #List the variables correlated with the response variable
  variable_list <- paste(results$df$variable, collapse = " ")

  #Compute the median absolute correlation
  median_corr <- median(abs(results$df$r))

  #Compute the range of absolute correlations
  corr_range <- range(abs(results$df$r))

  #Count the number of significant variables  @ p < 0.05
  significant_count <- sum(results$df$p != " " & as.numeric(results$df$p) < 0.05)

  #Print the summary
  cat("Correlating", results$y, "with", variable_list, "\n")
  cat("The median absolute correlation was", round(median_corr, 3),
      "with a range from", round(corr_range[1], 3), "to", round(corr_range[2], 3), "\n")
  cat(significant_count, "out of", nrow(results$df), "variables were significant at the p < 0.05 level.\n")
}
