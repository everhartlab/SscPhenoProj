#################### 
if (!requireNamespace("devtools")) install.packages("devtools", repos = "https://cran.rstudio.com")
devtools::install()
library(readxl)
library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(PMCMR)
library(plotrix)
################# Reading raw data from Excel file ############################
ssc_summary <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="Summary", na = c("", "NA"), col_names = FALSE)
#%>% write_csv("Results-single.csv")
colnames(ssc_summary) <- c("sheetid", "projdesc")
ssc_summary
#### Evaluations of isolates:
aproj <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="A",na = c("", "NA"))
bproj <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="B",na = c("", "NA"), range="A1:F385") #trim last col
cproj <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="C",na = c("", "NA"))
dproj <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="D",na = c("", "NA"))
#### Evaluations of cultivars:
eproj <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="E",na = c("", "NA"))
fproj <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="F",na = c("", "NA"))
gproj <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="G",na = c("", "NA"))
hproj <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="H",na = c("", "NA"), range="A1:F323") #trim last three cols
iproj <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="I",na = c("", "NA"), range="A1:D286") #trim last 4 cols

################# Analysis of aggressiveness (variation by isolate) ########################################
### 70 isolaves vs. Dassel soybean in detached leaf assay  ######### may need to go back to here and remove outliers per isolate
asum <- aproj %>% group_by(Isolate) %>% summarize(n = n(), mean = mean(Area), min = min(Area), max = max(Area), sd = sd(Area), se = std.error(Area))
(a.plot <- asum %>%
    ggplot(mapping = aes(x = Isolate, y = mean)) + 
    geom_point(na.rm = TRUE) +
    geom_jitter(width = 0.1) +
    theme_bw(base_size = 14) +
    scale_x_discrete(limits= asum$Isolate[sort(asum$mean, index.return=T)$ix]))
a.plot

(a.plot2 <- asum %>%
    ggplot(mapping=aes(x=1, y = mean)) +
    geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .2))
#+
    #geom_dotplot(binwidth=.2) +
 #   geom_dotplot(bins=length(mean)))

ggplot2.dotplot(data=df, xName='dose',yName='len',
                addBoxplot=TRUE,notch=TRUE)

asum %>% ggplot(mapping = aes(x = 1, y=mean)) + geom_boxplot()

### 29 isolates vs. dry bean IAC Alvorada in detached leaf bioassay
csum <- cproj %>% group_by(Isolate) %>% summarize(n = n(), mean = mean(Area), min = min(Area), max = max(Area), sd = sd(Area))

################# Analysis of cultivar performance (variation in cultivars) ################################




################ stopped here ################


(a.plot <- asum %>%
    ggplot(mapping = aes(x = Isolate, y = mean)) + 
    geom_point(na.rm = TRUE) +
    geom_jitter(width = 0.1) +
    theme_bw(base_size = 14) +
    scale_x_discrete(limits= asum$Isolate[sort(asum$mean, index.return=T)$ix]))
a.plot

asum %>% ggplot(mapping = aes(x = 1, y=mean)) + geom_boxplot()







  
  res$EC[grep("<=",x = res$EC)] <- NA
res$EC <- as.numeric(res$EC)
colnames(res)[8] <- "Antimicrobial"

res2 <- res %>%
  group_by(Antimicrobial) %>% 
  mutate(outlier = boxplot.stats(EC)$stats[5]) %>% # the fifth element is going to be the maximum outlier
  select(outlier, everything()) %>% 
  filter(EC < outlier)

res3 <- res2 %>%
  group_by(Inoculum, Antimicrobial) %>%
  summarize(n(), avg = mean(EC))  # obtaining the average EC-50 per isolate prior to summarizing below

res4 <- res3 %>%
  group_by(Antimicrobial) %>% 
  summarize(n(), mean(avg), min(avg), max(avg), sd(avg))

(res.plot <- res3 %>%
    ggplot(mapping = aes(x = Antimicrobial, y = avg)) + 
    geom_point(na.rm = TRUE) +
    geom_jitter(width = 0.1) +
    theme_bw(base_size = 14))  #### This produces a dot plot within the plots tab now




