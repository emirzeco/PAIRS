# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #
# Author: Emir Zecovic                                                                    #
# Last Update: 11.04.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # #
# RE ANALYIS PAIRFAM  #
# # # # # # # # # # # # 

# M20 (HHInc) ----
## M20a ----
M20a <- plm(
  satrelship ~ log_hhincgcee +
    age + page + sex + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_reduc,
  index = c("id", "wave"),
  model = "random"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# M21 (Grundsicherung) ----
## M21a ----
M21a <- plm(
  satrelship ~ benefit_dummy,
  data = p_reduc,
  index = c("id", "wave"),
  model = "random"
)

## M21b ----
M21b <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee,
  data = p_reduc,
  index = c("id", "wave"),
  model = "random"
)


## M21c ----
M21c <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + 
    age + page + sex + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_reduc,
  index = c("id", "wave"),
  model = "random"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# M22 (Wohngeld) ----
## M22a ----
M22a <- plm(
  satrelship ~ wohngeld,
  data = p_reduc,
  index = c("id", "wave"),
  model = "random"
)

## M22b ----
M22b <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + 
    age + page + sex + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_reduc,
  index = c("id", "wave"),
  model = "random"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# In-Work Sample ----
p_employed <- p_reduc %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Marginal employment",
      "Self-employed"
    )
  )

## Grundsicherung (In-Work Sample) ----
M23 <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + 
    age + page + sex + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_employed,
  index = c("id", "wave"),
  model = "random"
)


## Wohngeld (In-Work Sample) ----
M24 <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + 
    age + page + sex + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_employed,
  index = c("id", "wave"),
  model = "random"
)

# Men ----
p_men <- p_reduc %>%
  filter(sex == "Male")

## M25 (HHInc) ----
M25 <- plm(
  satrelship ~ log_hhincgcee +
    age + page + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_men,
  index = c("id", "wave"),
  model = "random"
)

## M26 (Grundsicherung) ----
M26 <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + 
    age + page + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_men,
  index = c("id", "wave"),
  model = "random"
)

## M27 (Wohngeld) ----
M27 <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + 
    age + page + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_men,
  index = c("id", "wave"),
  model = "random"
)

# Women ----
p_women <- p_reduc %>%
  filter(sex == "Female")

## M28 (HHInc) ----
M28 <- plm(
  satrelship ~ log_hhincgcee +
    age + page + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_women,
  index = c("id", "wave"),
  model = "random"
)

## M29 (Grundsicherung) ----
M29 <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + 
    age + page + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_women,
  index = c("id", "wave"),
  model = "random"
)

## M30 (Wohngeld) ----
M30 <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + 
    age + page + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_women,
  index = c("id", "wave"),
  model = "random"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Duration ----
## Grundsicherung ----
p_reduc <- p_reduc %>%
  arrange(id, wave) %>%
  group_by(id) %>%
  mutate(
    welfare_start = benefit_dummy == 1 & lag(benefit_dummy, default = 0) == 0,
    welfare_spell = cumsum(welfare_start),
    welfare_duration = case_when(
      benefit_dummy == 1 ~ ave(benefit_dummy, welfare_spell, FUN = \(x) seq_along(x)) - 1,
      benefit_dummy == 0 ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()
p_reduc <- p_reduc %>%
  mutate(
    welfare_duration_sq = welfare_duration^2
  )

### M31 ----
M31 <- plm(
  satrelship ~ welfare_duration + welfare_duration_sq +
    age + page  + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_reduc,
  index = c("id", "wave"),
  model = "random"
)


## Wohngeld ----
p_reduc <- p_reduc %>%
  arrange(id, wave) %>%
  group_by(id) %>%
  mutate(
    wohngeld_start = wohngeld == 1 & lag(wohngeld, default = 0) == 0,
    wohngeld_spell = cumsum(wohngeld_start),
    wohngeld_duration = case_when(
      wohngeld == 1 ~ ave(wohngeld, wohngeld_spell, FUN = \(x) seq_along(x)) - 1,
      wohngeld == 0 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    wohngeld_duration_sq = wohngeld_duration^2
  ) %>%
  ungroup()


### M32 ----
M_duration_wohngeld <- plm(
  satrelship ~ wohngeld_duration + wohngeld_duration_sq +
    age + page + school + migstatus + 
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave + cohort + east,
  data = p_reduc,
  index = c("id", "wave"),
  model = "random"
)