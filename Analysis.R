# Analyses of variance for DLB and Straw Test results ------------------------
# 
# This script was written by Sydney E. Everhart and Zhian N. Kamvar. 
# 
# Loading packages (and installing if needed) -----------------------------
#
# The checkpoint package is a fantastic package that will ensure reproducible
# research by scanning your project for packages and then installing them to 
# a temporary library from a specific date. This way you get non-invasive 
# reproducibility (as long as MRAN continues to run).

if (!require("checkpoint")) {
  install.packages("checkpoint", repos = "https://cran.rstudio.com")
  library("checkpoint")
}
dir.create(".checkpoint")
checkpoint(snapshotDate = "2018-02-22", checkpointLocation = ".")

# Some of the output you can expect to see:
# library("checkpoint")
#>
#> # checkpoint: Part of the Reproducible R Toolkit from Microsoft
#> # https://mran.microsoft.com/documents/rro/reproducibility/
#
# checkpoint("2018-02-22")
#> Can I create directory~/.checkpointfor internal checkpoint use?
#>   
#>   Continue (y/n)? y
#> Scanning for packages used in this project
#> - Discovered 10 packages
#> Installing packages used in this project 
#> - Installing ‘agricolae’
#> agricolae
#> - Installing ‘gridExtra’
#> gridExtra
#
# ...
#
#> checkpoint process complete
#> ---

library("tidyverse")
library("readxl")
library("plotrix")
library("grid")
library("gridExtra")
library("agricolae")
library("here")
library("sessioninfo")
dir.create(here("clean_data"))

# Reading raw data from Excel file ----------------------------------------
excel_nas   <- c("", "NA", ".", "#VALUE!")
data_path   <- here("Brazilian agressiveness_raw_data-final2.xlsx")
ssc_summary <- read_excel(data_path, sheet = "Summary", na = excel_nas, col_names = FALSE)
colnames(ssc_summary) <- c("sheetid", "projdesc")
ssc_summary

# Evaluation of isolates --------------------------------------------------
# A       70 isolates vs Dassel - soybean       ## Partially resistant
# B       Straw test_32 isolates_dry bean_G122  ## Partially resistant
# C       29 isolates vs IAC_DLB                
# D       Straw test_28_isolates_IAC_Alv_Brazil 

aproj <- read_excel(data_path, sheet = "A", na = excel_nas) %>%
  readr::write_csv(path = here("clean_data", "A_DLB_SoyBean_Dassel.csv"))
bproj <- read_excel(data_path, sheet = "B",na = excel_nas, range = "A1:F385") %>%
  readr::write_csv(path = here("clean_data", "B_ST_DryBean_G122.csv"))
cproj <- read_excel(data_path, sheet = "C", na = excel_nas) %>%
  readr::write_csv(path = here("clean_data", "C_DLB_DryBean_IAC-Alvorada.csv"))
dproj <- read_excel(data_path, sheet = "D", na = excel_nas) %>%
  readr::write_csv(path = here("clean_data", "D_ST_DryBean_IAC-Alvorada.csv"))


## Evaluation of cultivars -------------------------------------------------
# E       Soybean cultivars                                   
# F       First exp_rep_ DLB_dry bean cultivars_2B and 2D     
# G       Second exp_re_DLB_dry bean cultivars_2B             
# H       First exp_rep_strawtest_dry bean cultivars_2B and 2D
# I       Second exp_rep_ strawtest_dry bean cultivars_2D 


eproj <- read_excel(data_path, sheet = "E", na = excel_nas) %>%
  readr::write_csv(path = here("clean_data", "E_DLB_Soybean_Cultivars.csv"))
fproj <- read_excel(data_path, sheet = "F", na = excel_nas) %>%
  readr::write_csv(path = here("clean_data", "F_DLB_DryBean_Cultivars-1.csv"))
gproj <- read_excel(data_path, sheet = "G", na = excel_nas) %>%
  readr::write_csv(path = here("clean_data", "G_DLB_DryBean_Cultivars-2.csv"))
hproj <- read_excel(data_path, sheet = "H",na = excel_nas, range = "A1:F323")  %>% #trim last three cols
  readr::write_csv(path = here("clean_data", "H_ST_DryBean_Cultivars-1.csv"))
iproj <- read_excel(data_path, sheet = "I",na = excel_nas, range = "A1:D286") %>% #trim last 4 cols
  readr::write_csv(path = here("clean_data", "I_ST_DryBean_Cultivars-2.csv"))

# Analysis of aggressiveness (variation by isolate) -----------------------

### 70 isolaves vs. Dassel soybean in detached leaf assay
asum <- aproj %>%
  group_by(Isolate, Collection) %>%
  summarise(
    n = n(),
    mean = mean(Area, na.rm = TRUE),
    min = min(Area, na.rm = TRUE),
    max = max(Area, na.rm = TRUE),
    sd = sd(Area, na.rm = TRUE),
    se = plotrix::std.error(Area, na.rm = TRUE)
  )
bsum <- bproj %>%
  group_by(Isolate) %>%
  summarise(
    n = n(),
    mean = mean(`8 dai (cm)`, na.rm = TRUE),
    min = min(`8 dai (cm)`, na.rm = TRUE),
    max = max(`8 dai (cm)`, na.rm = TRUE),
    sd = sd(`8 dai (cm)`, na.rm = TRUE),
    se = plotrix::std.error(`8 dai (cm)`, na.rm = TRUE)
  )
csum <- cproj %>%
  group_by(Isolate, Collection) %>%
  summarise(
    n = n(),
    mean = mean(`48 horas`, na.rm = TRUE),
    min = min(`48 horas`, na.rm = TRUE),
    max = max(`48 horas`, na.rm = TRUE),
    sd = sd(`48 horas`, na.rm = TRUE),
    se = plotrix::std.error(`48 horas`, na.rm = TRUE)
  )
dsum <- dproj %>%
  group_by(Isolate) %>%
  summarise(
    n = n(),
    mean = mean(Score, na.rm = TRUE),
    min = min(Score, na.rm = TRUE),
    max = max(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    se = plotrix::std.error(Score, na.rm = TRUE)
  )

agg <- tibble::add_column(asum, proj = rep(c("a1","a2","a3"), (length(asum$mean)/3))) %>% 
  select(-Collection) %>% 
  as.data.frame() ## added to distinguish each rep bc they're sig dif
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


# LSD Test and ANOVA ------------------------------------------------------
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

### 29 isolates vs. dry bean IAC Alvorada in detached leaf bioassay
  csum <-
    cproj %>% group_by(Isolate) %>% summarize(
      n = n(),
      mean = mean(AUMPD, na.rm = TRUE),
      min = min(AUMPD, na.rm = TRUE),
      max = max(AUMPD, na.rm = TRUE),
      sd = sd(AUMPD, na.rm = TRUE),
      se = plotrix::std.error(AUMPD, na.rm = TRUE)
    )


# Session Information -----------------------------------------------------

  
.libPaths() # R library location
session_info()