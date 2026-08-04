# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in the UK  #
# Author: Emir Zecovic                                                                    #
# Last Update: 05.08.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # #
# DESCRIBING UKHLS  #
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
              "sjPlot", "lme4", "knitr", "kableExtra", 
              "stringr", "flextable", "officer", "sf", "plm", "stargazer",
              "patchwork", "tidytext", "sjlabelled")
install.packages(setdiff(packages, rownames(installed.packages())))
suppressMessages(lapply(packages, library, character.only = TRUE, quietly=T))
rm(packages)
options(max.print=10000)

## Load ----
uk <- readRDS("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/uk.rds")







# Count ----
uk %>%
  count(benefit_MT) %>%
  mutate(Percent = 100 * n / sum(n))

uk %>%
  count(wave, benefit_MT) %>%
  group_by(wave) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  print(n = 45)

uk %>%
  count(wave, benefit_MT, lfstat) %>%
  na.omit() %>%
  group_by(wave, lfstat) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  select(-n) %>%
  print(n = 180)



# Grouped Summary Tables ----
## Income types ----
### hhnetinc ----
uk %>%
  group_by(wave) %>%
  summarise(N    = sum(!is.na(hhnetinc)),
            Mean = mean(hhnetinc, na.rm = T),
            Var  = var(hhnetinc, na.rm = T),
            SD   = sd(hhnetinc, na.rm = T),
            Min  = min(hhnetinc, na.rm = T),
            Max  = max(hhnetinc, na.rm = T),
            NAs  = sum(is.na(hhnetinc))
            )

### hhgrsinc ----
uk %>%
  group_by(wave) %>%
  summarise(N    = sum(!is.na(hhgrsinc)),
            Mean = mean(hhgrsinc, na.rm = T),
            Var  = var(hhgrsinc, na.rm = T),
            SD   = sd(hhgrsinc, na.rm = T),
            Min  = min(hhgrsinc, na.rm = T),
            Max  = max(hhgrsinc, na.rm = T),
            NAs  = sum(is.na(hhgrsinc))
            )

### hhnetinc_oecd ----
uk %>%
  group_by(wave) %>%
  summarise(N    = sum(!is.na(hhnetinc_oecd)),
            Mean = mean(hhnetinc_oecd, na.rm = T),
            Var  = var(hhnetinc_oecd, na.rm = T),
            SD   = sd(hhnetinc_oecd, na.rm = T),
            Min  = min(hhnetinc_oecd, na.rm = T),
            Max  = max(hhnetinc_oecd, na.rm = T),
            NAs  = sum(is.na(hhnetinc_oecd))
  )

### hhgrsinc_oecd ----
uk %>%
  group_by(wave) %>%
  summarise(N    = sum(!is.na(hhgrsinc_oecd)),
            Mean = mean(hhgrsinc_oecd, na.rm = T),
            Var  = var(hhgrsinc_oecd, na.rm = T),
            SD   = sd(hhgrsinc_oecd, na.rm = T),
            Min  = min(hhgrsinc_oecd, na.rm = T),
            Max  = max(hhgrsinc_oecd, na.rm = T),
            NAs  = sum(is.na(hhgrsinc_oecd))
  )

### log_hhnetinc_oecd ----
uk %>%
  group_by(wave) %>%
  summarise(N    = sum(!is.na(log_hhnetinc_oecd)),
            Mean = mean(log_hhnetinc_oecd, na.rm = T),
            Var  = var(log_hhnetinc_oecd, na.rm = T),
            SD   = sd(log_hhnetinc_oecd, na.rm = T),
            Min  = min(log_hhnetinc_oecd, na.rm = T),
            Max  = max(log_hhnetinc_oecd, na.rm = T),
            NAs  = sum(is.na(log_hhnetinc_oecd))
  )


### log_hhgrsinc_oecd ----
uk %>%
  group_by(wave) %>%
  summarise(N    = sum(!is.na(log_hhgrsinc_oecd)),
            Mean = mean(log_hhgrsinc_oecd, na.rm = T),
            Var  = var(log_hhgrsinc_oecd, na.rm = T),
            SD   = sd(log_hhgrsinc_oecd, na.rm = T),
            Min  = min(log_hhgrsinc_oecd, na.rm = T),
            Max  = max(log_hhgrsinc_oecd, na.rm = T),
            NAs  = sum(is.na(log_hhgrsinc_oecd))
  )


### Correct high incomes? ----
uk %>%
  filter(hhnetinc > 100000) %>%
  select(
    hidp,
    wave,
    hhnetinc,
    hhgrsinc
  ) %>%
  distinct() %>%
  arrange(desc(hhnetinc))




#### Summary ----
income_summary <- uk %>%
  summarise(
    across(
      c(
        hhnetinc,
        hhgrsinc,
        hhnetinc_oecd,
        hhgrsinc_oecd,
        log_hhnetinc_oecd,
        log_hhgrsinc_oecd
      ),
      list(
        N = ~sum(!is.na(.)),
        Mean = ~mean(., na.rm = TRUE),
        SD = ~sd(., na.rm = TRUE),
        Min = ~min(., na.rm = TRUE),
        P25 = ~quantile(., 0.25, na.rm = TRUE),
        Median = ~median(., na.rm = TRUE),
        P75 = ~quantile(., 0.75, na.rm = TRUE),
        P95 = ~quantile(., 0.95, na.rm = TRUE),
        P99 = ~quantile(., 0.99, na.rm = TRUE),
        Max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_(?=[^_]+$)"
  )
income_summary


##### By Wave ----
income_summary_wave <- uk %>%
  group_by(wave) %>%
  summarise(
    across(
      c(
        hhnetinc,
        hhgrsinc,
        hhnetinc_oecd,
        hhgrsinc_oecd,
        log_hhnetinc_oecd,
        log_hhgrsinc_oecd
      ),
      list(
        N = ~sum(!is.na(.x)),
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE),
        Min = ~min(.x, na.rm = TRUE),
        P25 = ~quantile(.x, 0.25, na.rm = TRUE),
        Median = ~median(.x, na.rm = TRUE),
        P75 = ~quantile(.x, 0.75, na.rm = TRUE),
        P95 = ~quantile(.x, 0.95, na.rm = TRUE),
        P99 = ~quantile(.x, 0.99, na.rm = TRUE),
        Max = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -wave,
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(N|Mean|SD|Min|P25|Median|P75|P95|P99|Max)$"
  ) %>%
  arrange(wave, variable)

income_summary_wave


## Main vars ----
uk %>%
  group_by(wave) %>%
  summarise(Mean_HH_Net_Log_Income     = mean(log_hhnetinc_oecd, na.rm = T),
            Mean_Benefit               = mean(benefit_MT, na.rm = T),
            Mean_Rel_Happy             = mean(rel_happy, na.rm = T),
            Mean_NChild                = mean(nchild, na.rm = T),
            Mean_MCS                   = mean(mcs, na.rm = T),
            Mean_PCS                   = mean(pcs, na.rm = T)
  )

### By LFS ----
uk %>%
  filter(!is.na(lfstat)) %>%
  group_by(wave, lfstat) %>%
  summarise(Mean_HH_Net_Log_Income = mean(log_hhnetinc_oecd, na.rm = T),
            Mean_Benefit    = mean(benefit_MT, na.rm = T)
  ) %>%
  print(n = 90)

### Benefit recipients only ----
uk %>%
  filter(
    !is.na(lfstat),
    benefit_MT == 1
  ) %>%
  group_by(wave, lfstat) %>%
  summarise(
    Mean_HH_Net_Log_Income = mean(log_hhnetinc_oecd, na.rm = TRUE),
    N_Benefit = n(),
    .groups = "drop"
  ) %>%
  print (n = 90)




# Correlation Matrices ----
## log_hhnetinc_oecd ----
income_cor <- uk %>%
  select(pidp, wave, log_hhnetinc_oecd) %>%
  arrange(pidp, wave) %>%
  tidyr::pivot_wider(
    names_from = wave,
    values_from = log_hhnetinc_oecd,
    names_prefix = "log_hhnetinc_oecd_"
  ) %>%
  select(
    all_of(
      paste0("log_hhnetinc_oecd_", 1:15)
    )
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

income_cor

## benefitMT ----
benefitMT_cor <- uk %>%
  select(pidp, wave, benefit_MT) %>%
  tidyr::pivot_wider(
    names_from = wave,
    values_from = benefit_MT,
    names_prefix = "benefit_MT_"
  ) %>%
  select(
    any_of(
      paste0("benefit_MT_", 1:15)
    )
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

benefitMT_cor

## rel_happy ----
relhappy_cor <- uk %>%
  select(pidp, wave, rel_happy) %>%
  tidyr::pivot_wider(
    names_from = wave,
    values_from = rel_happy,
    names_prefix = "rel_happy_"
  ) %>%
  select(
    any_of(
      paste0("rel_happy_", c(1, 3, 5, 7, 9, 11, 13, 15))
    )
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

relhappy_cor


# Transition Tables ----
uk %>%
  group_by(wave, benefit_MT) %>%
  summarise(mean_rel_happy = mean(rel_happy, na.rm = T) %>%
              round(2)) %>%
  na.omit() %>%
  spread(benefit_MT, mean_rel_happy)








# Graphs ----
uk %>%
  filter(!is.na(benefit_MT)) %>%
  ggplot(aes(wave, fill = benefit_MT)) +
  geom_bar(position = "fill")





## Alluvial Plots ----
trans_data <- uk %>%
  select(pidp, wave, benefit_MT) %>%
  tidyr::pivot_wider(
    names_from = wave,
    values_from = benefit_MT,
    names_prefix = "benefit_MT_"
  ) %>%
  select(
    pidp,
    any_of(paste0("benefit_MT_", 1:15))
  ) %>%
  count(
    across(starts_with("benefit_MT_"))
  ) %>%
  mutate(
    id = row_number()
  )

trans_data_long <- trans_data %>%
  gather(
    key = key,
    value = value,
    -n,
    -id
  )

trans_data_long <- trans_data_long %>%
  mutate(
    wave = str_remove(key, "benefit_MT_") %>%
      as.numeric(),
    
    value = if_else(
      is.na(value),
      "Missing",
      as.character(value)
    ),
    
    value = factor(value),
    
    value = fct_relevel(
      value,
      "0", "1", "Missing"
    )
  )

plot <- ggplot(
  trans_data_long,
  aes(
    x = wave,
    y = n,
    stratum = value,
    fill = value,
    alluvium = id
  )
) +
  geom_stratum(alpha = .5) +
  geom_flow()
plot +
  theme_tufte(base_size = 18) +
  labs(
    x = "Wave",
    y = "Frequency",
    fill = "Means-tested benefits"
  ) +
  scale_fill_viridis_d(direction = -1)
plot



## Trends by Groups ----
uk %>%
  filter(!is.na(benefit_MT)) %>%
  ggplot(aes(
    wave,
    rel_happy,
    color = factor(benefit_MT,
                   labels = c("No", "Yes")),
    group = benefit_MT
  )) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(
    y = "Relationship satisfaction",
    x = "Wave",
    color = "Means-tested benefit"
  )



## Moderation effects ----
uk %>%
  filter(!is.na(lfstat)) %>%
  filter(!is.na(benefit_MT)) %>%
  filter(!is.na(rel_happy)) %>%
  ggplot(aes(
    wave,
    rel_happy,
    color = factor(benefit_MT, labels = c("No", "Yes")),
    group = benefit_MT
  )) +
  geom_smooth(method = "lm") +
  facet_wrap(~ lfstat) +
  theme_bw() +
  labs(
    x = "Wave",
    y = "Relationship satisfaction",
    color = "Means-tested benefit"
  )
