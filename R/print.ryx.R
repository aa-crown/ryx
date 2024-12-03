#'@title Printing Class ryx
#'@description Prints a table of correlations between a dependent variable y and one or more independent x variables using output from ryx() function from ryx package.
#'@export
#'@param results results from ryx()
#'@returns A nice table of correlation values between a dependent variable y and one or more independent x variables with variable names, r values, p values, and significance levels.
#'@examples
#'ryx(mtcars, y="mpg", x=c("hp", "wt", "disp", "cyl", "am", "gear"))
#'
#'x <- ryx(mtcars, y="mpg", x=c("hp", "wt", "disp"))
#'print(x)

print.ryx <- function(results, ...) {
  if (!inherits(results, "ryx")) {
    stop("This function requires an object created by ryx")
  }

  # Make a copy of the results to modify the p-values for printing
  df <- results$df

  # Format p-values: if less than 2e-16, show "< 2e-16"
  df$p <- sapply(df$p, function(pval) {
    if (pval < 2e-16) {
      return("< 2e-16")
    } else {
      return(format(pval, scientific = TRUE))
    }
  })

  # Re-calculate the significance stars, as the p-values are modified
  df$sigif <- ifelse(df$p == "< 2e-16", "***",  # Handle "< 2e-16" separately
                     ifelse(as.numeric(df$p) < .001, "***",
                            ifelse(as.numeric(df$p) < .01, "**",
                                   ifelse(as.numeric(df$p) < .05, "*", " "))))

  # Print the summary message
  cat("Correlations of", results$y, "with\n")
  # Print the modified data frame with formatted p-values
  print(df, row.names=FALSE)
}

