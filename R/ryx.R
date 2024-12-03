#'@title RYX: Correlation Table
#'@description Prints a table of correlations between a dependent variable y and one or more independent x variables. Uses only base R functions like cor.test(), for(), and rbind().
#'@param data a data frame
#'@param y the name of a numeric variable in the data frame
#'@param x a character vector with the names of one or more numeric variables in the data frame
#'@param digits an integer. Indicates the number of decimal places to print (3 is the default)
#'@returns A list with values $y, dependent variable, $x, dependent variables, $df, a data frame with correlation values; r values, p-values and significance levels, of class "ryx".
#'@export
#'@examples
#'ryx(mtcars, y="mpg", x=c("hp", "wt", "disp", "cyl", "am", "gear"))
#'ryx(mtcars, y="mpg", x=c("hp", "wt", "disp"))

ryx <- function(data, y, x){
  if(missing(x)){
    x <- names(data)[sapply(data, class)=="numeric"]
    x <- setdiff(x, y)
  }
  df <- data.frame()
  for (var in x){
    res <- cor.test(data[[y]], data[[var]])
    df_temp <- data.frame(variable = var,
                          r = res$estimate,
                          p = res$p.value)
    df <- rbind(df, df_temp)
    df <- df[order(-abs(df$r)),]
  }

  df$sigif <- ifelse(df$p < .001, "***",
                     ifelse(df$p < .01, "**",
                            ifelse(df$p < .05, "*", " ")))
  results <- list(y=y, x=x, df=df)
  class(results) <- "ryx"
  return(results)
}
