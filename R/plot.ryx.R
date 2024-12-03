#'@title Plotting Class ryx
#'@description Plots correlations between a dependent variable y and one or more independent x variables using output from ryx function from ryx package.
#'@export
#'@param results results from ryx
#'@returns A barbell plot of absolute value of the correlation values, with color representing the direction of the correlation.
#'@import ggplot2
#'@examples
#'x <- ryx(mtcars, y="mpg", x=c("hp", "wt", "disp", "cyl", "am", "gear"))
#'plot(x)

plot.ryx <- function(results,...){
  library(ggplot2)

  if (!inherits(results, "ryx")) {
    stop("This function requires an object created by ryx")
  }

  df <- results$df

  # Create a new column for absolute correlations
  df$abs_r <- abs(df$r)

  # Create a new column for the color of the barbell lines
  df$color <- ifelse(df$r < 0, "Negative", "Positive")  # Red for negative, Blue for positive

  # Create the barbell plot using ggplot2
  p <- ggplot(df,
              aes(x = abs_r,
                  y = reorder(variable, abs_r))) +
    geom_segment(aes(x = 0,
                     xend = abs_r,
                     yend = variable,
                     color = color),
                 size = 1.5) +  # Barbell lines
    geom_point(aes(color = color), size = 3) +  # Dots representing the correlation
    scale_color_manual(values = c("Negative" = "red", "Positive" = "blue"),
                       name = "Direction",  # Legend title
                       labels = c("Negative", "Positive")) +  # Legend labels
    labs(title = paste("Correlations with", results$y),
         x = "Correlation (absolute value)",
         y = "Variables") +
    theme_minimal() +
    theme(legend.position = "right")

  # Print the plot
  print(p)
}
