# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in the UK  #
# Author: Emir Zecovic                                                                    #
# Last Update: 01.08.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # #
# DESCRIBING UKHLS  #
# # # # # # # # # # #

# Setup ----
## Load ----
uk <- readRDS("C:/Users/Emir  PC/Desktop/PhD/Paper1/PAIRS/data/uk.rds")

# Count ----
uk %>%
  count(benefit_MT) %>%
  mutate(Percent = 100 * n / sum(n))

## By Wave ----
uk %>%
  count(wave, benefit_MT) %>%
  group_by(wave) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  print(n = 45)




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




# # # # # #
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





















# Summary ----
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












# Correlation Matrices ----




# Transition Tables ----

