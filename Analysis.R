#################### 
# if (!requireNamespace("devtools")) install.packages("devtools", repos = "https://cran.rstudio.com")
# library(devtools)
# library(ggpubr)
library(readxl)
library(tidyverse)
library(plotrix)
# library(dplyr)
# library(magrittr)
# library(readr)
# library(ggplot2)
# library(PMCMR)
# library(plotrix)
# library(Hmisc)    
library(grid)
library(gridExtra)
library(agricolae)
###################### Special note ###########################################
# From net: "So with hmisc installed I have to do use dplyr::summarise. Unloading hmisc allowed dplyr:summarize to work."
# Loading Hmisc after dplyr requires spelling summarize as summarise in script below
# The error that this produces is "Error in summarize( ...) argument "by" is missing..."
################# Reading raw data from Excel file ############################
ssc_summary <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="Summary", na = c("", "NA"), col_names = FALSE)
#%>% write_csv("Results-single.csv")
colnames(ssc_summary) <- c("sheetid", "projdesc")
ssc_summary
#### Evaluations of isolates:
# A	70 isolates vs Dassel - soybean       ## partially resistant
# B	Straw test_32 isolates_dry bean_G122  ## partially resistant
# C	29 isolates vs IAC_Alvorada_DLB
# D	Straw test_28_isolates_IAC_Alvorada_Brazil

aproj <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="A",na = c("", "NA"))
bproj <- read_excel("Brazilian agressiveness_raw_data-final2.xlsx", sheet="B",na = c("", "NA"), range="A1:F385") #trim last col
bproj$`8 dai (cm)` <- as.numeric(bproj$`8 dai (cm)`)
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
asum <- aproj %>% group_by(Isolate) %>% summarise(n = n(),  mean = mean(Area), min = min(Area), max = max(Area), sd = sd(Area), se = std.error(Area))
### 70 isolaves vs. Dassel soybean in detached leaf assay
asum <- aproj %>% group_by(Isolate, Collection) %>% summarise(n = n(), mean = mean(Area), min = min(Area), max = max(Area), sd = sd(Area))
bsum <- bproj %>% group_by(Isolate) %>% summarise(n = n(), mean = mean(`8 dai (cm)`), min = min(`8 dai (cm)`), max = max(`8 dai (cm)`), sd = sd(`8 dai (cm)`))
csum <- cproj %>% group_by(Isolate, Collection) %>% summarise(n = n(), mean = mean(`48 horas`), min = min(`48 horas`), max = max(`48 horas`), sd = sd(`48 horas`))
dsum <- dproj %>% group_by(Isolate) %>% summarise(n = n(), mean = mean(Score), min = min(Score), max = max(Score), sd = sd(Score))

agg <- tibble::add_column(asum, proj = rep(c("a1","a2","a3"), (length(asum$mean)/3))) %>% select(-Collection) %>% as.data.frame() ## added to distinguish each rep bc they're sig dif
bgg <- cbind(bsum, proj = rep("b", length(bsum$sd)))
cgg <- cbind(csum, proj = rep("c", length(csum$mean)))
dgg <- cbind(dsum, proj = rep("d", length(dsum$mean)))


dlb <- bind_rows(agg, cgg)
st <- bind_rows(bgg, dgg)

p2 <- dlb %>%
  ggplot(mapping=aes(x=proj, y = mean)) +
  geom_jitter(width = .1, height = 0, shape=21, color="black", fill="orange", size=3.5, alpha=3/4) +
  ###  change fill to be equal to the origin of the isolate ==== Brazil or USA
  stat_summary(fun.y=mean, geom="point", shape=95, size=12, color="black") + 
  ## there are several labeling features within labs() -- check help for more options
  theme_minimal() +
#  coord_fixed(ratio = 0.2) +
  labs(y = "Detached leaf bioassay") +
  scale_x_discrete(labels=c("a1" = "Dassel.1","a2" = "Dassel.2","a3" = "Dassel.3", "c" = "IAC-Alvorada")) +
  theme(axis.title.x = element_blank())

p3 <- st %>%
  ggplot(mapping=aes(x=proj, y = mean)) +
  geom_jitter(width = .1, height = 0, shape=21, color="black", fill="orange", size=3.5, alpha=3/4) +
  stat_summary(fun.y=mean, geom="point", shape=95, size=12, color="black") + 
  theme_minimal() +
  scale_y_continuous(position = "right") +
  labs(y = "Straw test rating") +
  scale_x_discrete(labels=c("b" = "G122", "d" = "IAC-Alvorada")) +
  theme(axis.title.x = element_blank())
grid.newpage()
#grid.draw(cbind(ggplotGrob(p2), ggplotGrob(p3), size = "last"))
#(arrangeGrob(p2,p3,ncol=2,widths=c(.8,.3)))

grid.arrange(p2, p3, nrow = 1, widths = c(2,1))


######## need to decide what kind of plot to use and obtain same data for the other isolates


#### Evaluations of isolates:
# A	70 isolates vs Dassel - soybean
# B	Straw test_32 isolates_dry bean_G122
# C	29 isolates vs IAC_DLB
# D	Straw test_28_isolates_IAC_Alv_Brazil

### LSD Test and ANOVA ######################################################

### Performed here using 10 observations per isolate and compared:
# moda <- aov(Area~Collection, data=aproj)
# outa <- LSD.test(moda, "Collection", p.adj="bonferroni")
# plot(outa)
# anova(moda)

### Performed here using 1 mean observation per isolate and compared:

modaa <- aov(mean~Isolate, data=asum)
  outaa <- LSD.test(modaa, "Isolate", p.adj="bonferroni")
  plot(outaa)
  anova(modaa)

aproj1 <- filter(aproj, Collection == "first")
  modaa <- aov(Area~Isolate, data=aproj1)
  outaa <- LSD.test(modaa, "Isolate", p.adj="bonferroni")
  plot(outaa)
  anova(modaa)

aproj2 <- filter(aproj, Collection == "second")
  modaa <- aov(Area~Isolate, data=aproj2)
  outaa <- LSD.test(modaa, "Isolate", p.adj="bonferroni")
  plot(outaa)
  anova(modaa)

aproj3 <- filter(aproj, Collection == "third")
  modaa <- aov(Area~Isolate, data=aproj3)
  outaa <- LSD.test(modaa, "Isolate", p.adj="bonferroni")
  plot(outaa)
  anova(modaa)




# ### Performed here using 10 observations per isolate and compared:
# modc <- aov(`48 horas`~Collection, data=cproj)
# outc <- LSD.test(modc, "Collection", p.adj="bonferroni")
# plot(outc)
# anova(modc)            ### No significant difference

# ### Performed here using 1 mean observation per isolate and compared:
# modcc <- aov(mean~Collection, data=csum)
# outcc <- LSD.test(modcc, "Collection", p.adj="bonferroni")
# plot(outcc)
# anova(modcc)
# 
# model2<-aov(Area~Name, data=eproj)
# out2 <- LSD.test(model2,"Name", p.adj="bonferroni")
# plot(out2)
# 
# model3 <- aov(`8 dai (cm)`~Isolate, data=bproj)
# out3 <- LSD.test(model3, "Isolate", p.adj="bonferroni")
# plot(out3)

### ANOVA to compare groups:# 
# model<-aov(yield~virus, data=sweetpotato)
# cv.model(model)
# anova(model)

#+
    #geom_dotplot(binwidth=.2) +
 #   geom_dotplot(bins=length(mean)))

# ZNK: This doesn't exist in any of the packages listed above
# ggplot2.dotplot(data=df, xName='dose',yName='len',
#                 addBoxplot=TRUE,notch=TRUE)

# ZNK: Maybe you want a histogram or density plot with a rug?
asum %>% ggplot(mapping = aes(x = 1, y=mean)) + geom_boxplot()

asum %>% ggplot(mapping = aes(x = 4, y=mean)) + geom_boxplot()

asum %>% ggplot(mapping = aes(x = 1, y=mean)) +
    geom_dotplot(stackdir = "center", binaxis = "y", binwidth = .1 , dotsize = 1, position = position_jitter(height=0, width=.02))


### 29 isolates vs. dry bean IAC Alvorada in detached leaf bioassay
csum <- cproj %>% group_by(Isolate) %>% summarize(n = n(), mean = mean(AUMPD, na.rm = TRUE), min = min(AUMPD, na.rm = TRUE), max = max(AUMPD, na.rm = TRUE), sd = sd(AUMPD, na.rm = TRUE))

### Not sure why summarize is not able to report sd or se for these data ## still working on this 2/21/2018
### ZNK: Because Area is not a column in these data. You want AUMPD.


################# Analysis of cultivar performance (variation in cultivars) ################################
################ stopped here ################




