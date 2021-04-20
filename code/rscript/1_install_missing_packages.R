# PACKAGES REQUIRED TO RUN SCRIPTS

required.packages <- c(
  "tidyverse", 
  "here", 
  "gridExtra",
  "raster",
  "cowplot",
  "scales",
  "parallel",
  "doSNOW",
  "foreach",
  "tibble",
  "nlrx",
  "digest",
  "future",
  "fields",
  "reldist",
  "reshape2", 
  "rlang",
  "jpeg",
  "shiny", 
  "GGally",
  "stringi",
  "ggthemes")

#INSTALL MISSING PACKAGES
install.packages(required.packages[!required.packages %in% installed.packages()])
