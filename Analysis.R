#################### 
if (!requireNamespace("devtools")) install.packages("devtools", repos = "https://cran.rstudio.com")
devtools::install()
library(readxl)
library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(PMCMR)

################# Reading raw data from Excel file ############################
ssc_summary <- read_excel("Brazilian agressiveness_raw_data.xlsx", sheet="Summary", na = c("", "NA"), col_names = FALSE)
#%>% write_csv("Results-single.csv")
colnames(ssc_summary) <- c("sheetid", "projdesc")
ssc_summary
aproj <- read_excel("Brazilian agressiveness_raw_data.xlsx", sheet="A",na = c("", "NA"))
bproj <- read_excel("Brazilian agressiveness_raw_data.xlsx", sheet="B",na = c("", "NA")) #trim last col
cproj <- read_excel("Brazilian agressiveness_raw_data.xlsx", sheet="C",na = c("", "NA"))
dproj <- read_excel("Brazilian agressiveness_raw_data.xlsx", sheet="D",na = c("", "NA"))
eproj <- read_excel("Brazilian agressiveness_raw_data.xlsx", sheet="E",na = c("", "NA"))
fproj <- read_excel("Brazilian agressiveness_raw_data.xlsx", sheet="F",na = c("", "NA"))
gproj <- read_excel("Brazilian agressiveness_raw_data.xlsx", sheet="G",na = c("", "NA"))
hproj <- read_excel("Brazilian agressiveness_raw_data.xlsx", sheet="H",na = c("", "NA")) #trim last three cols
iproj <- read_excel("Brazilian agressiveness_raw_data.xlsx", sheet="I",na = c("", "NA")) #trim last 4 cols


