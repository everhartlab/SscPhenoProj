#' Analyses of variance for DLB and Straw Test results ------------------------
#' 
#' This script was written by Sydney E. Everhart and Zhian N. Kamvar. 
#' 
#' Loading packages (and installing if needed) -----------------------------
#'
#' The checkpoint package is a fantastic package that will ensure reproducible
#' research by scanning your project for packages and then installing them to 
#' a temporary library from a specific date. This way you get non-invasive 
#' reproducibility (as long as MRAN continues to run).

if (!require("checkpoint")) {
  install.packages("checkpoint", repos = "https://cran.rstudio.com")
  library("checkpoint")
}
dir.create(".checkpoint")
checkpoint(snapshotDate = "2018-02-22", checkpointLocation = ".")

#' Some of the output you can expect to see:
#' library("checkpoint")
#>
#> # checkpoint: Part of the Reproducible R Toolkit from Microsoft
#> # https://mran.microsoft.com/documents/rro/reproducibility/
#
#' checkpoint("2018-02-22")
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
#' ...
#
#> checkpoint process complete
#> ---

library("tidyverse")
library("readxl")
library("plotrix")
library("cowplot")
library("agricolae")
library("here")
library("sessioninfo")
dir.create(here("clean_data"))

#' Reading raw data from Excel file ----------------------------------------
#' 
#' To read in the excel data, we have to ignore four possible missing values.
#' Additionally, we are enforcing column types in these data so isolate and
#' cultivar numbers are represented as character data instead of numbers. 
#' 
#' Moreover, because of floating point conversion issues, all number are rounded
#' to three decimal places as this is how they are represented in the 
#' spreadsheet. 
excel_nas   <- c("", "NA", ".", "#VALUE!")
data_path   <- here("Brazilian agressiveness_raw_data-final2.xlsx")
ssc_summary <- read_excel(data_path, sheet = "Summary", na = excel_nas, col_names = FALSE)
colnames(ssc_summary) <- c("sheetid", "projdesc")
ssc_summary

#' Evaluation of isolates --------------------------------------------------
#' 
#' A       70 isolates vs Dassel - soybean       ## Partially resistant
#' B       Straw test_32 isolates_dry bean_G122  ## Partially resistant
#' C       29 isolates vs IAC_DLB                
#' D       Straw test_28_isolates_IAC_Alv_Brazil 

aproj <- read_excel(data_path, sheet = "A", na = excel_nas, 
                    col_types = c("text", "text", "text", "text", "text", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  readr::write_csv(path = here("clean_data", "A_DLB_SoyBean_Dassel.csv"))

bproj <- read_excel(data_path, sheet = "B",na = excel_nas, range = "A1:F385",
                    col_types = c("text", "text", "numeric", "numeric", 
                                  "numeric", "numeric")) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
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


#' Evaluation of cultivars -------------------------------------------------
#' E       Soybean cultivars                                   
#' F       First exp_rep_ DLB_dry bean cultivars_2B and 2D     
#' G       Second exp_re_DLB_dry bean cultivars_2B             
#' H       First exp_rep_strawtest_dry bean cultivars_2B and 2D
#' I       Second exp_rep_ strawtest_dry bean cultivars_2D 


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
  readr::write_csv(path = here("clean_data", "I_ST_DryBean_Cultivars-2.csv"))

#' Analysis of aggressiveness (variation by isolate) -----------------------
#' 
#' In this part, we will summarize values for each replicate and then use these
#' to create a strip chart.
#' 
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

#' We want to create a single plot that contains both the results from the
#' detached leaf bioassay AND the straw test per isolate (sheets A-D). 
#' 

dir.create(here("figures"))

dlb <- bind_rows(a = asum, c = csum, .id = "proj") %>%
  mutate(proj = case_when(
    proj == "a" & Collection == "first"  ~ "Dassel (21 dae)",
    proj == "a" & Collection == "second" ~ "Dassel (28 dae)",
    proj == "a" & Collection == "third"  ~ "Dassel (35 dae)",
    # Sydney grouped the "c" isolates together probably because of the smaller
    # sample size. I'm showing them here to show how day doesn't matter.
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
                                      label_size = 16, 
                                      label_fontfamily = "Helvetica", 
                                      label_x = c(0.18, 0.045),
                                      label_y = c(0.975, 0.975))

cowplot::ggsave(filename = here("figures", "DAB-ST-stripplot.pdf"), 
                plot = aggressive_plot,
                width = 178, 
                height = 178*(0.621),
                units = "mm")
cowplot::ggsave(filename = here("figures", "DAB-ST-stripplot.tiff"), 
                dpi = 900,
                plot = aggressive_plot,
                width = 178, 
                height = 178*(0.621),
                units = "mm")

# grid.newpage()
# #grid.draw(cbind(ggplotGrob(p2), ggplotGrob(p3), size = "last"))
# #(arrangeGrob(p2,p3,ncol=2,widths=c(.8,.3)))
# 
# grid.arrange(p2, p3, nrow = 1, widths = c(2,1))


######## need to decide what kind of plot to use and obtain same data for the other isolates


#' LSD Test and ANOVA ------------------------------------------------------
#'

###  Performed here using 10 observations per isolate and compared:
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


#' Session Information -----------------------------------------------------

  
.libPaths() # R library location
session_info()