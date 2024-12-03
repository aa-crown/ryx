## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=F, message = F--------------------------------------------
#install.packages("remotes")
#remotes::install_github("https://github.com/aa-crown/ryx")
library(knitr)
library(ryx)

## ----warning = F--------------------------------------------------------------
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

## -----------------------------------------------------------------------------
x <- ryx(mtcars, y="mpg", x=c("hp", "wt", "disp", "cyl", "am", "gear"))
print(x)

## -----------------------------------------------------------------------------
x <- ryx(mtcars, y="mpg", x=c("hp", "wt", "disp", "cyl", "am", "gear"))
#Compute the median absolute correlation
median_corr <- median(abs(x$df$r))

#Compute the range of absolute correlations
corr_range <- range(abs(x$df$r))

## -----------------------------------------------------------------------------
x <- ryx(mtcars, y="mpg", x=c("hp", "wt", "disp", "cyl", "am", "gear"))
summary(x)

## -----------------------------------------------------------------------------
x <- ryx(mtcars, y="mpg", x=c("hp", "wt", "disp", "cyl", "am", "gear"))
plot(x)

