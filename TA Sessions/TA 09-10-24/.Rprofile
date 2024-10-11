#A welcome message
message("The profile loads libraries here, tidyverse, rio")

# Customise the R prompt that prefixes every command
# (use " " for a blank prompt)
options(prompt = "proj_example> ", digits = 6)

#local({
#  r = getOption("repos")           
#  r["CRAN"] = "https://cran.fiocruz.br/"
#  options(repos = r)
#})

###  The interactive() function tests whether R is being used interactively in a terminal. The fortune() function is called within try(). If the fortunes package is not available, we avoid raising an error and move on. By using :: we avoid adding the fortunes package to our list of attached packages.
if (interactive())
  try(fortunes::fortune(), silent = TRUE)

###  Dot! hidden function. wont be deleted with rm(list=ls())
.hh  <-  function(d) d[1:5, 1:5]
.d  <- function(x) dim(x)
.impsum <- function(x) {
  z <- import(x)
  if (dim(z)[2]<=10) {print(head(z,10))}
  if (dim(z)[2]>10) {print(names(z))}
  cat("Data object dimensions:", dim(z), "\n")
  return(z)
}
.r <- function() {rm(list=ls())}

###  The .Renviron file is used to store system variables. Like APIs or library paths
library(pacman)
p_load(
  here,
  tidyverse,
  rio
)
