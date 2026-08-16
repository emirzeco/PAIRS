# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in the UK  #
# Author: Emir Zecovic                                                                    #
# Last Update: 05.08.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # #
# UKHLS Weights     #
# # # # # # # # # # #

# Setup ----
## Packages ----
# if ("convenience" %in% rownames(installed.packages()) ==F) {
#   devtools::install_github("ratsupaltuf/convenience", force=T)
# }

alabs<- function(x, kable=TRUE, extended=T){
  tb<- tibble(labels=names(attributes(x)$labels), values=attributes(x)$labels)
  
  if(extended==T) {
    lab<- attributes(x)$label
    cl<- class(x)
    u<- unique(x)
    
    if(kable==T) {
      
      cat(lab, "\n",
          "Class:",cl, "\n",
          "Unique values:", u, "\n",
          "\n"
      )
      print(knitr::kable(tb, caption="Labels"))
      
    }  else {
      cat(lab, "\n",
          "Class:",cl, "\n",
          "Unique values:", u, "\n",
          "\n"
      )
      print(tb)
    }
  } else if (kable==T) {
    
    kable(tb)
  }  else {
    tb
  }
}

packages <- c("tidyverse", "haven", "pastecs", "datawizard", #"convenience",
              "ggplot2", "ggalluvial", "ggthemes", "viridis", "ggrepel",
              "sjPlot", "lme4", "knitr", "kableExtra", "gt", "survey",
              "stringr", "flextable", "officer", "sf", "plm", "stargazer",
              "patchwork", "tidytext", "sjlabelled", "purrr")
install.packages(setdiff(packages, rownames(installed.packages())))
suppressMessages(lapply(packages, library, character.only = TRUE, quietly=T))
rm(packages)
options(max.print=10000)

## Load ----
uk <- readRDS("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/uk.rds")
#uk <- readRDS("~/PAIRS/data/uk.rds")



# Longindtunal weight ----

## Missings ----
uk_2 <- uk %>%
  filter(
    !is.na(psu),
    !is.na(strata)
  )

## Apply ----
options(survey.lonely.psu = "adjust")

uk_w <- svydesign(
  ids    = ~psu,
  strata = ~strata,
  #weights = ~o_indinus_lw,
  data = uk_2,
  nest = TRUE
)














# Cross-sectional weights ----
## Path
#ukhls_path <- "C:/Users/Emir  PC/Desktop/PhD/Paper1/Datasets/UKDA-6614-stata/stata/stata14_se/ukhls"
ukhls_path <- "~/PAIRS/data/UKHLS/"

# ## Individual waves ----
# weight_info <- tibble(
#   wave = 1:15,
#   prefix = letters[1:15],
#   weight_var = c(
#     "a_indinus_xw",
#     "b_indinub_xw",
#     "c_indinub_xw",
#     "d_indinub_xw",
#     "e_indinub_xw",
#     "f_indinui_xw",     #f_indinub_xw",
#     "g_indinui_xw",
#     "h_indinui_xw",
#     "i_indinui_xw",
#     "j_indinui_xw",
#     "k_indinui_xw",
#     "l_indinui_xw",
#     "m_indinui_xw",
#     "n_inding2_xw",
#     "o_inding2_xw"
#   )
# )
# 
# ## Weight ----
# weights_cs <- purrr::pmap_dfr(
#   weight_info,
#   function(wave, prefix, weight_var) {
#     
#     dat <- haven::read_dta(
#       paste0(ukhls_path, "/", prefix, "_indresp.dta"),
#       encoding = "latin1"
#     )
#     
#     dat %>%
#       dplyr::select(
#         pidp,
#         weight_cs = dplyr::all_of(weight_var)
#       ) %>%
#       dplyr::mutate(wave = wave)
#   }
# )

### Save ----
write_dta(weights_cs, "~/PAIRS/data/weights_cs.dta")




### Check ----
weights_cs %>%
  count(wave)

weights_cs %>%
  group_by(wave) %>%
  summarise(
    n = n(),
    missing = sum(is.na(weight_cs)),
    zero = sum(weight_cs == 0, na.rm = TRUE),
    positive = sum(weight_cs > 0, na.rm = TRUE),
    .groups = "drop"
  )


## Merge ----
uk <- uk %>%
  left_join(
    weights_cs,
    by = c("pidp", "wave")
  )

### Check ----
uk %>%
  group_by(wave) %>%
  summarise(
    n = n(),
    missing_weight = sum(is.na(weight_cs)),
    zero_weight = sum(weight_cs == 0, na.rm = TRUE),
    positive_weight = sum(weight_cs > 0, na.rm = TRUE),
    .groups = "drop"
  )

uk %>%
  filter(!is.na(benefit_MT)) %>%
  group_by(wave) %>%
  summarise(
    n_benefit_observed = n(),
    missing_weight = sum(is.na(weight_cs)),
    zero_weight = sum(weight_cs == 0, na.rm = TRUE),
    positive_weight = sum(weight_cs > 0, na.rm = TRUE),
    .groups = "drop"
  )




## Apply ----
### Missings ----
uk_cs <- uk %>%
  filter(
    !is.na(benefit_MT),
    weight_cs > 0,
    !is.na(psu),
    !is.na(strata)
  )


### Survey ----
options(survey.lonely.psu = "adjust")
uk_cs_w <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight_cs,
  data = uk_cs,
  nest = TRUE
)






# # # # # # # # # # # # # # # # # 
benefit_time_series <- uk_cs %>%
  group_by(wave) %>%
  summarise(
    n_unweighted = sum(benefit_MT == 1),
    n_weighted = sum(weight_cs * (benefit_MT == 1)),
    .groups = "drop"
  )

benefit_time_series



benefit_time_series <- uk_cs %>%
  group_by(wave) %>%
  summarise(
    n_unweighted = sum(benefit_MT == 1),
    n_weighted = sum(weight_cs * (benefit_MT == 1)),
    weighted_percent = 100 * weighted.mean(
      benefit_MT,
      w = weight_cs
    ),
    .groups = "drop"
  )
benefit_time_series



benefit_time_series %>%
  ggplot(aes(x = wave, y = n_weighted)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:15) +
  theme_bw() +
  labs(
    x = "Wave",
    y = "Weighted number of means-tested benefit receipt "
  )
