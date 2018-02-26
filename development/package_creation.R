install.packages("devtools")
library(devtools)
devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("../Documents/")
create("significativity.bar")

setwd("significativity.bar/")
document()


