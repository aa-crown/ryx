% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.ryx.R
\name{print.ryx}
\alias{print.ryx}
\title{Printing Class ryx}
\usage{
\method{print}{ryx}(results, ...)
}
\arguments{
\item{results}{results from ryx()}
}
\value{
A nice table of correlation values between a dependent variable y and one or more independent x variables with variable names, r values, p values, and significance levels.
}
\description{
Prints a table of correlations between a dependent variable y and one or more independent x variables using output from ryx() function from ryx package.
}
\examples{
ryx(mtcars, y="mpg", x=c("hp", "wt", "disp", "cyl", "am", "gear"))

x <- ryx(mtcars, y="mpg", x=c("hp", "wt", "disp"))
print(x)
}
