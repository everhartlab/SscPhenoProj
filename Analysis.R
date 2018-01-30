#################### 
if (!requireNamespace("devtools")) install.packages("devtools", repos = "https://cran.rstudio.com")
library(devtools)
library(ggpubr)
library(readxl)
library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(PMCMR)
library(plotrix)
library(Hmisc)    
library(grid)
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
asum <- aproj %>% group_by(Isolate) %>% summarise(n = n(), mean = mean(Area), min = min(Area), max = max(Area), sd = sd(Area))
bsum <- bproj %>% group_by(Isolate) %>% summarise(n = n(), mean = mean(`8 dai (cm)`), min = min(`8 dai (cm)`), max = max(`8 dai (cm)`), sd = sd(`8 dai (cm)`))
csum <- cproj %>% group_by(Isolate) %>% summarise(n = n(), mean = mean(`48 horas`), min = min(`48 horas`), max = max(`48 horas`), sd = sd(`48 horas`))
dsum <- dproj %>% group_by(Isolate) %>% summarise(n = n(), mean = mean(Score), min = min(Score), max = max(Score), sd = sd(Score))

agg <- cbind(asum, rep("a", length(asum$sd)))
bgg <- cbind(bsum, rep("b", length(bsum$sd)))
cgg <- cbind(csum, rep("c", length(csum$mean)))
dgg <- cbind(dsum, rep("d", length(dsum$mean)))

colnames(agg)[7] <- "proj"
colnames(bgg)[7] <- "proj"
colnames(cgg)[7] <- "proj"
colnames(dgg)[7] <- "proj"
dlb <- rbind(agg,cgg)
st <- rbind(bgg,dgg)

p2 <- dlb %>%
  ggplot(mapping=aes(x=proj, y = mean)) +
  geom_jitter(width = .1, height = 0, shape=21, color="black", fill="orange", size=3.5, alpha=2/3) +
  ###  change fill to be equal to the origin of the isolate ==== Brazil or USA
  stat_summary(fun.y=mean, geom="point", shape=95, size=12, color="black") + 
  ## there are several labeling features within labs() -- check help for more options
  theme_minimal() +
  labs(y = "Detached leaf assay") +
  scale_x_discrete(labels=c("a" = "ST:Dassel", "c" = "DLB:Alvorada")) +
  theme(axis.title.x = element_blank())

p3 <- st %>%
  ggplot(mapping=aes(x=proj, y = mean)) +
  geom_jitter(width = .1, height = 0, shape=21, color="black", fill="orange", size=3.5, alpha=2/3) +
  stat_summary(fun.y=mean, geom="point", shape=95, size=12, color="black") + 
  theme_minimal() +
  scale_y_continuous(position = "right") +
  labs(y = "Straw test rating") +
  scale_x_discrete(labels=c("b" = "DLB:G122", "d" = "DLB:Alvorada")) +
  theme(axis.title.x = element_blank())
grid.newpage()
grid.draw(cbind(ggplotGrob(p2), ggplotGrob(p3), size = "last"))





######## need to decide what kind of plot to use and obtain same data for the other isolates


### LSD Test --

model2<-aov(Area~Name, data=eproj)
out2 <- LSD.test(model2,"Name", p.adj="bonferroni")
plot(out2)

model3 <- aov(`8 dai (cm)`~Isolate, data=bproj)
out3 <- LSD.test(model3, "Isolate", p.adj="bonferroni")
plot(out3)


#+
    #geom_dotplot(binwidth=.2) +
 #   geom_dotplot(bins=length(mean)))

ggplot2.dotplot(data=df, xName='dose',yName='len',
                addBoxplot=TRUE,notch=TRUE)

asum %>% ggplot(mapping = aes(x = 1, y=mean)) + geom_boxplot()

asum %>% ggplot(mapping = aes(x = 4, y=mean)) + geom_boxplot()

asum %>% ggplot(mapping = aes(x = 1, y=mean)) +
    geom_dotplot(stackdir = "center", binaxis = "y", binwidth = .1 , dotsize = 1, position = position_jitter(height=0, width=.02))


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




