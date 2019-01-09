library(DBI)
library(RMySQL)
library(dplyr)

source("io.R")

#### Defining functions for statistical tests.

# This implements a single sided t-test with difference between means (du) from
# the null and alternative hypotheses, the standard deviation of the distribution (sd)
# and the sample size (n).

ptval <- function(du, sd, n) {
  # Returns both p and t values.
  t = du / (sd / sqrt(n))
  p = pt(q = t, df = (n - 1), lower.tail = F)
  res = c(t, p)
  return(res)
}

tval <- function(du, sd, n) {
  # Returns just t values.
  t = du / (sd / sqrt(n))
  p = pt(q = t, df = (n - 1), lower.tail = F)
  res = c(t, p)
  return(t)
}

pval <- function(du, sd, n) {
  # Returns just p values.
  t = du / (sd / sqrt(n))
  p = pt(q = t, df = (n - 1), lower.tail = F)
  res = c(t, p)
  return(p)
}

#### Manipulating the data frame.
getPTValues <- function() {
  # Read data from view into 'sample.data' data frame.
  sample.data = getSampleData()

  # Removing nonsense, control, and rinse rows.
  filt.sample.data <- filter(sample.data, solid_conc > 0, SD > 0)

  ## Grouping by element into a list.

  #elem <- c("Al", "As", "Ba", "Cd", "Cu", "K", "Mn", "Ni", "Pb", "Se", "Zn") Are these the ones Frank said were relevant? I can't recall.
  elem <- c("Al", "As", "Ba", "Ca", "Cd", "Cu", "Co", "Cr", "Cu", "Fe", "K", "Mg", "Mn", "Mo", "Ni", "Pb", "Se", "Sr", "Y", "Zn") # This is all available elements.
  samp.elem <- list()
  regex <- c()

  for (i in 1:length(elem)) {
    regex[i] <- paste("^", elem[i], ".*", sep = '')
    samp.elem[[i]] <- data.frame(filt.sample.data[grep(regex[i], filt.sample.data[, 2],
      perl = TRUE),])
  }
  names(samp.elem) <- elem
  return(samp.elem)
}