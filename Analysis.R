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
#
# This first if statement is asking whether or not we are inside a binder 
# session. The binder session allows the analysis to be re-run interactively
# in the cloud. If joyvan is run, the checkpoint package is not needed.
if (Sys.getenv("USER") != "jovyan") {
  if (!require("checkpoint")) {
    install.packages("checkpoint", repos = "https://cran.rstudio.com")
    library("checkpoint")
  }
  dir.create(".checkpoint")
  checkpoint(snapshotDate = "2018-02-23", checkpointLocation = ".")
}
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
library("agricolae")   # LSD test  ## Emerson recommends emmeans package instead
library("lmerTest")    # random effects ANOVA
library("lubridate")   # for converting stupid datetime values from excel

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
#
# Because all the 97X isolates have the 97 part removed, I'm creating a little
# function to add it in so that the data can be combined later on. 
fix_isolate_name <- . %>%
  mutate(Isolate = case_when(
    grepl("^[0-9][A-Z]$", Isolate) ~ paste0("97", Isolate), 
    TRUE                           ~ Isolate
    ))
# Evaluation of isolates --------------------------------------------------
# 
# A       70 isolates vs Dassel - soybean       ## Partially resistant
# B       Straw test_32 isolates_dry bean_G122  ## Partially resistant
# C       29 isolates vs IAC_DLB                
# D       Straw test_28_isolates_IAC_Alv_Brazil 

aproj <- read_excel(data_path, sheet = "A", na = excel_nas, 
                    col_types = c("text", "text", "text", "text", "text", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  fix_isolate_name %>%
  readr::write_csv(path = here("clean_data", "A_DLB_SoyBean_Dassel.csv"))

# The G122 project is contained in two different sheets that need to be joined
# together. The first step is to read in the csv data. The first column will
# be renamed to X1 automatically. The first colum is the full isolate names.
# This is necessary to confirm that Block is in the correct order.
bproj_raw <- read_csv(here("Mensure and score in different days_straw test.csv"),
                      col_types = cols(
                        X1      = col_character(),
                        Block   = col_character(),
                        `3 dai` = col_double(),
                        `6 dai` = col_double(),
                        `8 dai` = col_double(),
                        AUDPC   = col_double(),
                        `After first node` = col_double()
                      ), 
                      na = excel_nas)
# The next step is to read in the excel sheet B and filter it. 
bproj <- read_excel(data_path, sheet = "B",na = excel_nas, range = "A1:F385",
                    col_types = c("text", "text", "numeric", "numeric", 
                                  "numeric", "numeric")) %>%
  fix_isolate_name %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  dplyr::group_by(Isolate) %>%
  dplyr::mutate(Block = as.character(seq(n()))) %>%
  dplyr::ungroup() %>%
  dplyr::inner_join(bproj_raw, 
                    by = c("Isolate"    = "X1", 
                           "8 dai (cm)" = "8 dai", 
                           "AUDPC", 
                           "After first node",
                           "Block")) %>%
  dplyr::select(Isolate_number, Isolate, Block, 
                `3 dai`, `6 dai`, `8 dai (cm)`, 
                everything()) %>%
  readr::write_csv(path = here("clean_data", "B_ST_DryBean_G122.csv"))

stopifnot(nrow(bproj) == nrow(bproj_raw))

cproj <- read_excel(data_path, sheet = "C", na = excel_nas,
                    col_types = c("text", "text", "text", "text", "text", 
                                  "numeric", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  fix_isolate_name %>%
  readr::write_csv(path = here("clean_data", "C_DLB_DryBean_IAC-Alvorada.csv"))

dproj <- read_excel(data_path, sheet = "D", na = excel_nas, 
                    col_types = c("text", "text", "numeric", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  fix_isolate_name %>%
  readr::write_csv(path = here("clean_data", "D_ST_DryBean_IAC-Alvorada.csv"))


# Evaluation of cultivars -------------------------------------------------
# E       Soybean cultivars                                   
# F       First exp_rep_ DLB_dry bean cultivars_2B and 2D     
# G       Second exp_re_DLB_dry bean cultivars_2B             
# H       First exp_rep_strawtest_dry bean cultivars_2B and 2D
# I       Second exp_rep_ strawtest_dry bean cultivars_2D 

eproj <- read_excel(data_path, sheet = "E", na = excel_nas, 
                    col_types = c("text", "text", "text","text", "numeric")) %>%
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


# isolate origin information ----------------------------------------------
# Downloading the file from the open science framework. 
the_download <- try(download.file("https://osf.io/2yfre/download", here("MasterIsolateList.xlsx")))
if (!inherits(the_download, "try-error")){
  # reading in the excel sheet has its own problems since the date column contains
  # part dates and part text and they get screwed up no matter what you do. The
  # way I've dealt with this: import as dates and then convert what didn't parse
  # into the number of days since 1899-12-30
  metadata  <- read_excel(here("MasterIsolateList.xlsx"), col_types = "text", na = c("NA", "")) %>%
    mutate(date = as.Date(parse_date_time(`JRS-Collection Date`, c("mdy", "y")))) %>%
    mutate(date = case_when(
      is.na(date) ~ as.Date("1899-12-30") + days(as.integer(`JRS-Collection Date`)),
      TRUE        ~ date
    )) %>%
    select(-`JRS-Collection Date`) %>%
    readr::write_csv(here("clean_data", "MasterIsolateList.csv"))
  # This table provides information on how to find specific isolates in the JR
  # Steadman collection. Here, we will challenge it against the A-D projects and
  # see which isolates do not match:
  anti_join(aproj, metadata, by = c("Isolate" = "AP-GenoID")) %>% count(Isolate) %>% print()
  anti_join(bproj, metadata, by = c("Isolate" = "AP-GenoID")) %>% count(Isolate) %>% print()
  anti_join(cproj, metadata, by = c("Isolate" = "AP-GenoID")) %>% count(Isolate) %>% print()
  anti_join(dproj, metadata, by = c("Isolate" = "AP-GenoID")) %>% count(Isolate) %>% print()
}

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
    mean = mean(Score, na.rm = TRUE),
    min = min(Score, na.rm = TRUE),
    max = max(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    se = plotrix::std.error(Score, na.rm = TRUE)
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
# summarizing data that was previously shown in the figure so that it can be added to a table 
# stopped here: dlb %>% group_by(proj) %>% print(n)



sydney_theme <- theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(axis.text = element_text(color = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, angle = 45, color = "black")) +
  theme(panel.border = element_rect(size = 1))

set.seed(2018-02-27)
p2 <- dlb %>%
  ggplot(mapping=aes(x = proj, y = mean)) +
  geom_jitter(width = .1, height = 0, shape = 21, color = "black", 
              fill = "white", size = 3.5, alpha = 2.5/4, stroke = 1) +
  stat_summary(fun.y = mean, geom = "point", shape = 95, size = 17, color = "black") + 
  labs(y = expression(paste("Detached leaf bioassay ", (cm^2) ))) +
  sydney_theme
  
p2
p3 <- st %>%
  ggplot(mapping = aes(x = proj, y = mean)) +
  geom_jitter(width = .1, height = 0, shape = 21, color = "black", 
              fill = "white", size = 3.5, alpha = 2.5/4, stroke = 1) +
  stat_summary(fun.y = mean, geom = "point", shape = 95, size = 17, color = "black") + 
  scale_y_continuous(position = "right", limits = c(1, 9), breaks = c(1, 3, 5, 7, 9)) +
  labs(y = "Straw test rating") +
  sydney_theme

aggressive_plot <- cowplot::plot_grid(p2, p3, labels = "AUTO", align = "h", 
                                      rel_widths = c(2.75, 1),
                                      label_size = 16, 
                                      label_fontfamily = "Helvetica", 
                                      label_x = c(A = 0.1650, B = 0.075),
                                      label_y = c(A = 0.975, B = 0.975))
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
# account for the effects of Collection and section, we will code these as
# random effects by specifying (1 | Collection) + (1 | Section), which accounts
# for both of these before assessing Isolate. 

# Dassel by Isolate -------------------------------------------------------
Dassel_model <- lmer(Area ~ Isolate + (1 | Collection) + (1 | Section), data = aproj)
anova(Dassel_model)
Dassel_LSD <- myLSD(aproj$Area, aproj$Isolate, Dassel_model, p.adj = "bonferroni")

# From this, we can see that Isolate is significantly different. However, we
# noticed earlier that there was a stark contrast between the collection times.
# Here we can add collection time as a fixed effect in our model and see if it
# is significant.

Dassel_model2 <- lmer(Area ~ Isolate + Collection + (1 | Section), data = aproj)
anova(Dassel_model2)
Dassel_LSD2 <- myLSD(aproj$Area, aproj$Isolate, Dassel_model2, p.adj = "bonferroni")

# Indeed it is significant, so we will now analyze each collection time
# separately.

aproj %>%
  group_by(Collection) %>%
  summarize(model = list(broom::tidy(anova(lmer(Area ~ Isolate + (1 | Section)))))) %>%
  unnest()

# Everything is significant while separating these out, so we can conclude that,
# while the experiments differed, they only differed in magnitude, but not 
# pattern. We can see the magnitude of how the experiments changed by looking 
# at the collection response, specifically.

myLSD(aproj$Area, aproj$Collection, Dassel_model2, p.adj = "bonferroni")

asum %>% 
  group_by(Collection) %>% 
  summarize(mean = mean(mean))


# IAC-Alvorada by Isolate -------------------------------------------------
# Here, we are performing the same analysis with the IAC-Alvorada data. We don't
# expect Collection to be significant in this model.
IAC_model <- lmer(`48 horas` ~ Isolate + (1 | Collection) + (1 | Block), data = cproj)
anova(IAC_model)
IAC_LSD <- myLSD(cproj$`48 horas`, cproj$Isolate, IAC_model, p.adj = "bonferroni")

# Again, because we saw the difference in Dassel if we considered leaf age, we
# will set that as a fixed effect and test it here. 
IAC_model2 <- lmer(`48 horas` ~ Isolate + Collection + (1 | Block), data = cproj)
anova(IAC_model2)
IAC_LSD2 <- myLSD(cproj$`48 horas`, cproj$Isolate, IAC_model2, p.adj = "bonferroni")

# Again, the collection time appears to be slightly significant, so we can check
# to see if this affected the outcome by separating the collections

cproj %>%
  group_by(Collection) %>%
  summarize(model = list(broom::tidy(anova(lmer(`48 horas` ~ Isolate + (1 | Block)))))) %>%
  unnest()

# Okay, we can see that everything still appears significant after considering
# collection separately. 
IAC_LSD2 <- myLSD(cproj$`48 horas`, cproj$Collection, IAC_model2, p.adj = "bonferroni")

# It appears that the third collection time is different in magnitude from the 
# first two, but only at p = 0.003
csum %>%
  group_by(Collection) %>%
  summarize(mean = mean(mean, na.rm = TRUE))

# Straw Test: Isolates
# 
# Straw tests are not performed on varying tissue age, so we need only compare
# by isolate here. We are treating each replicate as a random effect 
#
#
# G122 by Isolate ---------------------------------------------------------
G122_model <- lmer(Score ~ Isolate + (1 | Block), data = bproj)
anova(G122_model)
G122_LSD <- myLSD(bproj$Score, bproj$Isolate, G122_model, p.adj = "bonferroni")
# Isolate is significant

# IAC-Alvorada by Isolate: Straw Test -------------------------------------
IAC_ST_model <- lmer(Score ~ Isolate + (1 | Rep), data = dproj)
anova(IAC_ST_model)
ISC_ST_LSD <- myLSD(dproj$Score, dproj$Isolate, IAC_ST_model, p.adj = "bonferroni")
# Isolate is significant, however, this is largely driven by one 
# under-performing isolate (972D).

dproj2 <- filter(dproj, Isolate != "972D")
IAC_ST_model2 <- lmer(Score ~ Isolate + (1 | Rep), data = dproj2)
anova(IAC_ST_model2)
ISC_ST_LSD2 <- myLSD(dproj2$Score, dproj2$Isolate, IAC_ST_model2, p.adj = "bonferroni")
# Isolate is significant, however, this is largely driven by one 
# under-performing isolate (2D).

# Summary table across isolates -------------------------------------------
# 
# It would be nice to find out if there are any isolates that are consistently
# outperforming all other isolates. Here, I will create a table that aggregates
dir.create(here("tables"))
# the isolate means per experiment. 
isolate_data <- bind_rows(`Dassel DLB`              = asum, 
                          `IAC-Alvorada DLB`        = csum, 
                          `G122 Straw Test`         = bsum, 
                          `IAC-Alvorada Straw Test` = dsum,
                          .id = "Experiment")
isolate_data %>% 
  filter(grepl("Straw", Experiment)) %>% 
  group_by(Experiment) %>% 
  mutate(class = case_when(
    mean >= 7 ~ "Aggressive (7-9)",
    mean >= 4 ~ "Intermediate (4-6)",
    TRUE      ~ "Non-Aggressive (1-3)"
  )) %>%
  mutate(n = n()) %>%
  count(class, n) %>%
  mutate(n = 100 * (nn/n)) %>%
  rename(N = nn, Class = class, `%` = n) %>%
  select(Experiment, Class, N, `%`) %>% 
  readr::write_csv("tables/straw-test-classifications.csv")
isolate_summary <- isolate_data %>% 
  group_by(Experiment, Collection) %>%
  summarize(Min  = round(min(min), 3), 
            Mean = round(mean(mean, na.rm = TRUE), 3), 
            Max  = round(max(max), 3), 
            `Top 10` = list(
              data_frame(
                Isolate        = head(Isolate[order(mean, decreasing = TRUE)], 10),
                `Isolate Mean` = head(sort(mean, decreasing = TRUE), 10),
                rank           = 1:10
                )
              )) %>%
  arrange(grepl("Straw", Experiment))

isolate_summary_print <- isolate_summary %>%
  rowwise() %>%
  mutate(`Top 10` = paste(`Top 10`$Isolate, collapse = ", ")) %>%
  readr::write_csv(here("tables/isolate_summary.csv"))

# Because this isolate table may be difficult to parse, A better solution would
# be to arrange these isolates by the number of times an isolate is in the top 
# 10 of any experiment and is assessed over at least three of the four 
# experiments. 

experiment_order <-c(
  "Dassel DLB_first" = "Dassel DLB (21 dae)",
  "Dassel DLB_second" = "Dassel DLB (28 dae)",
  "Dassel DLB_third" = "Dassel DLB (35 dae)",
  "IAC-Alvorada DLB_first" = "IAC-Alvorada DLB (21 dae)",
  "IAC-Alvorada DLB_second" = "IAC-Alvorada DLB (28 dae)",
  "IAC-Alvorada DLB_third" = "IAC-Alvorada DLB (35 dae)",
  "G122 Straw Test_NA" = "G122 Straw Test",
  "IAC-Alvorada Straw Test_NA" = "IAC-Alvorada Straw Test" 
)
isolate_data_arranged <- isolate_data %>%
  ungroup() %>%
  filter(is.finite(mean)) %>%
  unite(col = EC, Experiment, Collection, remove = FALSE) %>%
  group_by(EC) %>%
  mutate(rank = rank(mean, ties.method = "last", na.last = TRUE)) %>%
  arrange(-rank) %>%
  mutate(rank = seq(n())) %>%
  ungroup() %>%
  arrange(grepl("Straw", EC)) %>%
  mutate(EC = fct_inorder(EC)) %>%
  group_by(Isolate) %>%
  mutate(top = case_when(rank < 11 ~ TRUE, TRUE ~ FALSE)) %>%
  mutate(sumtop = sum(top)) %>%
  mutate(perctop = sumtop/n()) %>%
  mutate(sum = sum(mean, na.rm = TRUE)) %>%
  filter(length(unique(Experiment)) >= 3) %>%
  ungroup() %>%
  # filter(sumtop > 0) %>%
  arrange(-sumtop) %>%
  mutate(Isolate = fct_inorder(Isolate)) %>%
  mutate(EC = fct_relevel(EC, names(experiment_order))) %>%
  mutate(EC = `levels<-`(EC, experiment_order))
isolate_data_arranged

# Here, I'm creating a summary table that summarizes what the data shows. This
# will arrange the isolates by the number of times they were found in the top 10
# of any experiment, give the percentage out of the number of total experiments
# (including collections), the number of experiments conducted, and those
# experiments that they were found to be in the top 10.
isolate_data_arranged %>%
  group_by(Isolate) %>%
  summarize(`In the Top 10` = unique(sumtop), 
            `%` = unique(perctop),
            `N Experiments` = length(unique(Experiment)),
            Experiments = paste(EC[top], collapse = ", ")) %>%
  mutate(Experiments = gsub("_", " ", Experiments)) %>%
  mutate(Experiments = gsub(" NA", "", Experiments)) %>%
  readr::write_csv("tables/isolates_in_top_ten.csv") %>%
  print()

# This barplot summarizes the above table by using transparency to denote the
# top 10. 
pal <- c(
  "Dassel DLB (21 dae)" = "#B2E0D2",
  "Dassel DLB (28 dae)" = "#8CD1BB",
  "Dassel DLB (35 dae)" = "#66C2A5",
  "IAC-Alvorada DLB (21 dae)" = "#FDC6B0",
  "IAC-Alvorada DLB (28 dae)" = "#FCA989",
  "IAC-Alvorada DLB (35 dae)" = "#FC8D62",
  "G122 Straw Test" = "#8DA0CB",
  "IAC-Alvorada Straw Test" = "#E78AC3"
)

explot <- ggplot(isolate_data_arranged, aes(x = Isolate, y = mean)) +
  geom_col(aes(fill = EC, color = top)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = c("FALSE" = "#FFFFFF69", "TRUE" = "black"), guide = "none") +
  labs(list(
    title = "Isolates ranked in at least three experiments",
    fill = "Experiment (Replicate)",
    caption = "Bars with borders = ranked in the top 10",
    y = "cumulative mean"
  )) +
  sydney_theme +
  theme(aspect.ratio = 0.62)
explot
ggsave(plot = explot, 
       filename = "figures/isolate-rank.pdf", 
       width = 9,
       height = 5)
ggsave(plot = explot, 
       filename = "figures/isolate-rank.png",
       dpi = 600, 
       width = 9,
       height = 5)
ggsave(plot = explot, 
       filename = "figures/isolate-rank.tiff",
       dpi = 900, 
       width = 9,
       height = 5)


# Comparing isolates between DLB assays -----------------------------------
# 
# The DLB assays were performed on a Brazilian and non-Brazilian cultivar.
# The question is: how do isolates shared between the tests compare?
# 
# Step 1: gather the isolates shared between projects
isos <- inner_join(select(aproj, Isolate), select(cproj, Isolate)) %>%
  count(Isolate) %>%
  pull(Isolate)
cat(isos, sep = ", ")

# Step 2: Tabulate the number of experiments each isolate was ranked in the
# top ten.
isolate_summary %>%
  unnest() %>%
  filter(Isolate %in% isos, grepl("DLB", Experiment)) %>%
  select(-matches("M")) %>%
  spread(Isolate, rank, fill = 0) %>%
  summarize_if(is.numeric, ~sum(. > 0)) %>%
  gather(Isolate, Count, -Experiment) %>%
  spread(Experiment, Count) %>%
  arrange(`Dassel DLB` + `IAC-Alvorada DLB`) %>%
  readr::write_csv(here("tables/DLB-comparison.csv")) %>%
  print()


# Cultivar tests ----------------------------------------------------------
# =========================================================================
# 
# Here we have three experiments that have to do with assessing if there is a
# difference in resistance between cultivars. 
# 
#  - eproj - Detached Leaf Bioassay on 11 soybean cultivars with two 
#      experimental replications at 34 dae and 60 dae
#  - fproj & gproj Detached Leaf Bioassay on 23 Dry Bean cultivars. The first
#      sheet represents testing of two isolates. 
#  - hproj - Straw test on 23 Dry Bean cultivars to determine isolates for the
#      experiment.
#  - irpoj - Straw test on 19 Dry Bean cultivars.
#

# Soybean Variety Detached Leaf Bioassay ---------------------------------
#
# We can do a similar thing that we did in the assessments above. We will test
# for differences between cultivars and use Experimental replicates and the
# replicate as the random effects
soy_model <- lmer(Area ~ Name + (1 | Exp_rep) + (1 | Rep), data = eproj) # Hola, model! Soy Zhian. 
anova(soy_model)
soy_LSD <- myLSD(eproj$Area, eproj$Name, soy_model, p.adj = "bonferroni")

# Notice, however that there appears to be an effect based on experimental 
# replicate
ggplot(eproj, aes(x = Name, y = Area, fill = Exp_rep)) +
  geom_boxplot() +
  sydney_theme
# The question then becomes, is it significant if we include it as a fixed
# effect in our model?
soy_model2 <- lmer(Area ~ Name + Exp_rep + (1 | Rep), data = eproj)
anova(soy_model2)
# Yes, it is significant
soy_LSD2 <- myLSD(eproj$Area, eproj$Exp_rep, soy_model2, p.adj = "bonferroni")

# What do the different experiments look like if we analyze them separately?
eproj %>% 
  group_by(Exp_rep) %>%
  summarize(model = list(lmer(Area ~ Name + (1 | Rep)) %>% anova() %>% broom::tidy())) %>%
  unnest()
# This is interesting. If we analyze these separately, then the results are not
# significant at p < 0.0001 or even p < 0.01. However, this could be due to 
# overdispersion of the data. 

# Dry Bean Cultivar Detached Leaf Bioassay --------------------------------
# 
# This one is a bit tricky since there are two experimental replicates with
# uneven blocks. TJM used a two-way AMOVA, but we are really only interested in
# the difference between cultivars.

# First, we must prepare the data by combining it with the same isolate.
cultivar_DLB <- fproj %>%
  filter(Isolate == "2B") %>% 
  select(Block, Cultivar = Cultivar_name, AUMPC=`AUMPC (48)`)
cultivar_DLB <- gproj %>%
  select(Block, Cultivar = Cultivar_name, AUMPC) %>%
  bind_rows(cultivar_DLB, .id = "Experiment")

# Now for the modelling. We will once again treat Experiment and BLock as random
# effects
cultivar_DLB_model <- lmer(AUMPC ~ Cultivar + (1 | Experiment) + (1 | Block), data = cultivar_DLB)
anova(cultivar_DLB_model)
cultivar_DLB_LSD <- myLSD(cultivar_DLB$AUMPC, cultivar_DLB$Cultivar, cultivar_DLB_model, p.adj = "bonferroni")

# And we can visualize the effect of experiment
ggplot(cultivar_DLB, aes(x = Cultivar, y = AUMPC, fill = Experiment)) + 
  geom_boxplot() +
  sydney_theme
# We can see that there's not as strong of an effect due to experiment, and we 
# can tickle our fancy by including this in our fixed effects
cultivar_DLB_model2 <- lmer(AUMPC ~ Cultivar + Experiment + (1 | Block), data = cultivar_DLB)
anova(cultivar_DLB_model2)
cultivar_DLB_LSD2 <- myLSD(cultivar_DLB$AUMPC, cultivar_DLB$Experiment, cultivar_DLB_model, p.adj = "bonferroni")
# The effect is significant, so we will proceed to split the Experiments and
# analyze them separately

cultivar_DLB %>%
  group_by(Experiment) %>%
  summarize(model = list(lmer(AUMPC ~ Cultivar + (1 | Block)) %>% anova() %>% broom::tidy())) %>%
  unnest()
# This is quite revealing, but it shows what we see in the data visualization:
# the results are inconsistent between experiments, especially for IAC Diplomata,
# and IAC Una.
# 
# Dry Bean Cultivar Straw Test --------------------------------------------
#
# Similar to the Detached Leaf Bioassay, the straw tests were done in two
# experiments. However, the first experiment included all of the cultivars, but
# the second one only included those that showed resistance. We should account
# for this when combining these data.

# Organizing Data
cultivar_ST <- hproj %>% 
  filter(Isolate == "2D") %>%
  select(-Isolate) %>%
  bind_rows(iproj, .id = "Experiment")

cultivars_to_keep <- cultivar_ST %>% 
  count(Cultivar) %>%
  arrange(n)
cultivars_to_keep # we should remove the top 4.
cultivars_to_keep <- filter(cultivars_to_keep, n > 7) %>% pull(Cultivar)
cultivar_ST <- filter(cultivar_ST, Cultivar %in% cultivars_to_keep)

# The Model
cultivar_ST_model <- lmer(Score ~ Cultivar + (1 | Experiment) + (1 | Rep), data = cultivar_ST) 
anova(cultivar_ST_model)
cultivar_ST_LSD <- myLSD(cultivar_ST$Score, cultivar_ST$Cultivar, cultivar_ST_model, p.adj = "bonferroni")

# The visualization
ggplot(cultivar_ST, aes(x = Cultivar, y = Score, fill = Experiment)) + 
  geom_boxplot() +
  sydney_theme +
  scale_y_continuous(limits = c(1, 9), breaks = c(1, 3, 5, 7, 9))

# There doesn't appear to be any significant effect of Experiment.
cultivar_ST_model2 <- lmer(Score ~ Cultivar + Experiment + (1 | Rep), data = cultivar_ST) 
anova(cultivar_ST_model2)
cultivar_ST_LSD2 <- myLSD(cultivar_ST$Score, cultivar_ST$Experiment, cultivar_ST_model2, p.adj = "bonferroni")
# The effect of experiment is not significant

# Session Information -----------------------------------------------------

  
.libPaths() # R library location
session_info()
library("pillar") # kludge to get this installed correctly by checkpoint

