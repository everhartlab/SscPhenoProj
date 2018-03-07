install.packages("tidyverse")   # data wrangling and rectangling + ggplot2
install.packages("readxl")      # read excel files
install.packages("plotrix")     # std.error() function
install.packages("cowplot")     # multi-panel plotting
install.packages("agricolae")   # LSD test
install.packages("lmerTest")    # random effects ANOVA
install.packages("here")        # to burn setwd() to the ground
install.packages("sessioninfo") # to know where we stand
install.packages("pillar") # kludge to get this installed correctly by checkpoint
writeLines("R_LIBS_USER=/srv/rlibs", "~/.Renviron") # workaround for binder to work
