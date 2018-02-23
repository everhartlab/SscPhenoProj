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
checkpoint(snapshotDate = "2018-02-23", checkpointLocation = ".")

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


# Packages for analysis and graphing --------------------------------------
library("tidyverse")   # data wrangling and rectangling + ggplot2
library("readxl")      # read excel files
library("plotrix")     # std.error() function
library("cowplot")     # multi-panel plotting
library("agricolae")   # LSD test
library("lmerTest")    # random effects ANOVA

# Packages of convenience -------------------------------------------------
library("here")        # to burn setwd() to the ground
library("sessioninfo") # to know where we stand

dir.create(here("clean_data"))
dir.create(here("figures"))

# Reading raw data from Excel file ----------------------------------------
# 
# To read in the excel data, we have to ignore four possible missing values.
# Additionally, we are enforcing column types in these data so isolate and
# cultivar numbers are represented as character data instead of numbers. 
# 
# Moreover, because of floating point conversion issues, all number are rounded
# to three decimal places as this is how they are represented in the 
# spreadsheet. 
excel_nas   <- c("", "NA", ".", "#VALUE!")
data_path   <- here("Brazilian agressiveness_raw_data-final2.xlsx")
ssc_summary <- read_excel(data_path, sheet = "Summary", na = excel_nas, col_names = FALSE)
colnames(ssc_summary) <- c("sheetid", "projdesc")
ssc_summary

# Evaluation of isolates --------------------------------------------------
# 
# A       70 isolates vs Dassel - soybean       ## Partially resistant
# B       Straw test_32 isolates_dry bean_G122  ## Partially resistant
# C       29 isolates vs IAC_DLB                
# D       Straw test_28_isolates_IAC_Alv_Brazil 

aproj <- read_excel(data_path, sheet = "A", na = excel_nas, 
                    col_types = c("text", "text", "text", "text", "text", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  readr::write_csv(path = here("clean_data", "A_DLB_SoyBean_Dassel.csv"))

bproj <- read_excel(data_path, sheet = "B",na = excel_nas, range = "A1:F385",
                    col_types = c("text", "text", "numeric", "numeric", 
                                  "numeric", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  dplyr::group_by(Isolate) %>%
  dplyr::mutate(Rep = seq(n())) %>%
  dplyr::ungroup() %>%
  readr::write_csv(path = here("clean_data", "B_ST_DryBean_G122.csv"))

cproj <- read_excel(data_path, sheet = "C", na = excel_nas,
                    col_types = c("text", "text", "text", "text", "text", 
                                  "numeric", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  readr::write_csv(path = here("clean_data", "C_DLB_DryBean_IAC-Alvorada.csv"))

dproj <- read_excel(data_path, sheet = "D", na = excel_nas, 
                    col_types = c("text", "text", "numeric", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  readr::write_csv(path = here("clean_data", "D_ST_DryBean_IAC-Alvorada.csv"))


# Evaluation of cultivars -------------------------------------------------
# E       Soybean cultivars                                   
# F       First exp_rep_ DLB_dry bean cultivars_2B and 2D     
# G       Second exp_re_DLB_dry bean cultivars_2B             
# H       First exp_rep_strawtest_dry bean cultivars_2B and 2D
# I       Second exp_rep_ strawtest_dry bean cultivars_2D 

eproj <- read_excel(data_path, sheet = "E", na = excel_nas, 
                    col_types = c("text", "text", "text","numeric", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  readr::write_csv(path = here("clean_data", "E_DLB_Soybean_Cultivars.csv"))

fproj <- read_excel(data_path, sheet = "F", na = excel_nas, range = "A1:N277",
                    col_types = c("text", "text", "text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", "numeric", 
                                  "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  readr::write_csv(path = here("clean_data", "F_DLB_DryBean_Cultivars-1.csv"))

gproj <- read_excel(data_path, sheet = "G", na = excel_nas, range = "A1:I277",
                    col_types = c("text", "text", "text", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  readr::write_csv(path = here("clean_data", "G_DLB_DryBean_Cultivars-2.csv"))

hproj <- read_excel(data_path, sheet = "H",na = excel_nas, range = "A1:E323", 
                    col_types = c("text", "text", "text", "text", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  readr::write_csv(path = here("clean_data", "H_ST_DryBean_Cultivars-1.csv"))


iproj <- read_excel(data_path, sheet = "I",na = excel_nas, range = "A1:D286", 
                    col_types = c("text", "text", "text", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  dplyr::mutate(Cultivar = case_when(
    Cultivar == "IPR139" ~ "IPR 139",
    TRUE                 ~ Cultivar
  )) %>% 
  readr::write_csv(path = here("clean_data", "I_ST_DryBean_Cultivars-2.csv"))

# Analysis of aggressiveness (variation by isolate) -----------------------
# 
# In this part, we will summarize values for each replicate and then use these
# to create a strip chart.
# 
### 70 isolates vs. Dassel soybean in detached leaf assay
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

# We want to create a single plot that contains both the results from the
# detached leaf bioassay AND the straw test per isolate (sheets A-D). 


dlb <- bind_rows(a = asum, c = csum, .id = "proj") %>%
  mutate(proj = case_when(
    proj == "a" & Collection == "first"  ~ "Dassel (21 dae)",
    proj == "a" & Collection == "second" ~ "Dassel (28 dae)",
    proj == "a" & Collection == "third"  ~ "Dassel (35 dae)",
    proj == "c" & Collection == "first"  ~ "IAC-Alvorada (21 dae)",
    proj == "c" & Collection == "second" ~ "IAC-Alvorada (28 dae)",
    proj == "c" & Collection == "third"  ~ "IAC-Alvorada (35 dae)"
  ))
st  <- bind_rows(G122 = bsum, `IAC-Alvorada` = dsum, .id = "proj")

sydney_theme <- theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, angle = 45, color = "black"))
  
p2 <- dlb %>%
  ggplot(mapping=aes(x=proj, y = mean)) +
  geom_jitter(width = .1, height = 0, shape=21, color="black", fill="orange", size=3.5, alpha=3/4) +
  stat_summary(fun.y=mean, geom="point", shape=95, size=12, color="black") + 
  labs(y = "Detached leaf bioassay") +
  sydney_theme
  
p2
p3 <- st %>%
  ggplot(mapping = aes(x = proj, y = mean)) +
  geom_jitter(width = .1, height = 0, shape=21, color="black", fill="orange", size=3.5, alpha=3/4) +
  stat_summary(fun.y=mean, geom="point", shape=95, size=12, color="black") + 
  scale_y_continuous(position = "right") +
  labs(y = "Straw test rating") +
  sydney_theme

aggressive_plot <- cowplot::plot_grid(p2, p3, labels = "AUTO", align = "h", 
                                      rel_widths = c(2.75, 1),
                                      label_size = 16, 
                                      label_fontfamily = "Helvetica", 
                                      label_x = c(0.13, 0.045),
                                      label_y = c(0.975, 0.975))
aggressive_plot
cowplot::ggsave(filename = here("figures", "DAB-ST-stripplot.pdf"), 
                plot = aggressive_plot,
                width = 178, 
                height = 178*(0.621),
                units = "mm")
cowplot::ggsave(filename = here("figures", "DAB-ST-stripplot.png"), 
                plot = aggressive_plot,
                dpi = 600,
                width = 178, 
                height = 178*(0.621),
                units = "mm")
cowplot::ggsave(filename = here("figures", "DAB-ST-stripplot.tiff"), 
                dpi = 900,
                plot = aggressive_plot,
                width = 178, 
                height = 178*(0.621),
                units = "mm")

# LSD Test and ANOVA ------------------------------------------------------
# 
# We are using a random effects model due to the presence of blocks and leaf
# age. This is implemented in the lmerTest package, which wraps lme4
# 
# By default, R treats the first sample as the control and creates the ANOVA
# model trying to find differences from the control. In our case, we want to
# use orthoganal contrasts:
op <- options(contrasts = c("contr.helmert", "contr.poly"))

#' Custom Least Significant Difference
#' 
#' Because LSD.test from agricolae only uses lm and aov models, I have to do
#' some wrangling to get it to work for lmerTest objects. This helper function
#' will do that for me. 
#'
#' @param response a vector of response variables used to build the model
#' @param trt a vector with the treatment variable to be assessed
#' @param model a model returned from lmer
#' @param ...   arguments to be passed to LSD.test()
#' @param plot  an argument of whether or not to plot the results (default: TRUE)
#'
#' @return
#' 
#' an object of class "group" from agricolae
#' 
myLSD <- function(response, trt, model, ..., plot = TRUE){
  DFE <- df.residual(model) 
  MSE <- deviance(model, REML = FALSE)/DFE
  res <- LSD.test(y = response, trt = trt, DFerror = DFE, MSerror = MSE, ...)
  plot(res, variation = "SE")
  res
}

# Test DLB by Isolate -----------------------------------------------------
# 
# We want to assess whether or not there is a difference between isolates in
# our assay. Since there are different leaf ages, we also want to include that 
# in the model to confirm that there is no difference due to this factor.
# 
# Here we are analyzing the data sets for Dassel and IAC-Alvorada. Because we
# want to test if there are differences between isolates themselves, but want to
# account for the effects of Collection and the interaction of Collection and 
# Isolate, we will code these as random effects by specifying 
# (1 | Collection/Isolate), which indicates that Isolate is nested within 
# Collection. 

# Dassel by Isolate -------------------------------------------------------
Dassel_model <- lmer(Area ~ Isolate + (1 | Collection/Isolate), data = aproj)
anova(Dassel_model)
Dassel_LSD <- myLSD(aproj$Area, aproj$Isolate, Dassel_model, p.adj = "bonferroni")

# From this, we can see that Isolate is significantly different. However, we
# noticed earlier that there was a stark contrast between the collection times.
# Here we can add collection time as a fixed effect in our model and see if it
# is significant.

Dassel_model2 <- lmer(Area ~ Isolate + Collection + (1 | Collection:Isolate), data = aproj)
anova(Dassel_model2)

# Indeed it is significant

Dassel_LSD2 <- myLSD(aproj$Area, aproj$Isolate, Dassel_model2, p.adj = "bonferroni")
myLSD(aproj$Area, aproj$Collection, Dassel_model2, p.adj = "bonferroni")

asum %>% 
  group_by(Collection) %>% 
  summarize(mean = mean(mean))


# IAC-Alvorada by Isolate -------------------------------------------------
# Here, we are performing the same analysis with the IAC-Alvorada data. We don't
# expect Collection to be significant in this model.
IAC_model <- lmer(`48 horas` ~ Isolate + (1 | Collection/Isolate), data = cproj)
anova(IAC_model)
IAC_LSD <- myLSD(cproj$`48 horas`, cproj$Isolate, IAC_model, p.adj = "bonferroni")

# Again, because we saw the difference in Dassel if we considered leaf age, we
# will set that as a fixed effect and test it here. 
IAC_model2 <- lmer(`48 horas` ~ Isolate + Collection + (1 | Collection:Isolate), data = cproj)
anova(IAC_model2)
#
# Based on the results here, we can conclude that leaf age does not have a 
# significant effect on lesion area for IAC-Alvorada.

csum %>%
  group_by(Collection) %>%
  summarize(mean = mean(mean, na.rm = TRUE))

# ## Straw Test: Isolates
# 
# Straw tests are not performed on varying tissue age, so we need only compare
# by isolate here. We are treating each replicate as a random effect 


# G133 by Isolate ---------------------------------------------------------
G133_model <- lmer(`8 dai (cm)` ~ Isolate + (1 | Rep), data = bproj)
anova(G133_model)
G133_LSD <- myLSD(bproj$`8 dai (cm)`, bproj$Isolate, G133_model)
# Isolate is significant

# IAC-Alvorada by Isolate: Straw Test -------------------------------------
IAC_ST_model <- lmer(Score ~ Isolate + (1 | Rep), data = dproj)
anova(IAC_ST_model)
ISC_ST_LSD <- myLSD(dproj$Score, dproj$Isolate, IAC_ST_model)
# Isolate is significant, however, this is largely driven by one 
# under-performing isolate.




# Session Information -----------------------------------------------------

  
.libPaths() # R library location
session_info()
library("pillar") # kludge to get this installed correctly by checkpoint

