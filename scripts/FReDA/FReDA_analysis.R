# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #
# Author: Emir Zecovic                                                                    #
# Last Update: 13.04.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # #
# FE ANALYIS FReDA    #
# # # # # # # # # # # # 


# M1 (HHInc)----
## M1a ----
M1a <- plm(
  satrelship ~ log_hhincgcee,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M1b ----
M1b <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + lfstat + nkids,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M1c ----
M1c <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + lfstat + wave,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# M2 (Grundsicherung) ----
## M2a ----
M2a <- plm(
  satrelship ~ benefit_dummy,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M2b ----
M2b <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)


## M2c ----
M2c <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lifesat,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)


## M2d ----
M2d <- plm(
  satrelship ~ benefit_dummy +
    lifesat +
    log_hhincgcee + 
    relstat2  +
    lfstat +
    wave,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# M3 (Wohngeld) ----
## M3a ----
M3a <- plm(
  satrelship ~ wohngeld,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M3b ----
M3b <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M3c ----
M3c <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + lifesat,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M3d ----
M3d <- plm(
  satrelship ~ wohngeld +
    lifesat +
    log_hhincgcee + 
    relstat2  +
    lfstat +
    wave,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# In-Work Benefits ----
f_employed <- f_reduc %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Marginal employment",
      "Self-employed"
    )
  )

M4 <- plm(
  satrelship ~ benefit_dummy +
    lifesat +
    log_hhincgcee + 
    relstat2  +
    lfstat +
    wave,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)

M5 <- plm(
  satrelship ~ wohngeld +
    lifesat +
    log_hhincgcee + 
    relstat2  +
    lfstat +
    wave,
  data = f_reduc,
  index = c("id", "wave"),
  model = "within"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Men ONLY ----
f_men <- f_reduc %>%
  filter(sex == "Male")

M6 <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + lfstat + wave,
  data = f_men,
  index = c("id", "wave"),
  model = "within"
)

M7 <- plm(
  satrelship ~ benefit_dummy +
    lifesat +
    log_hhincgcee + 
    relstat2  +
    lfstat +
    wave,
  data = f_men,
  index = c("id", "wave"),
  model = "within"
)

M8 <- plm(
  satrelship ~ wohngeld +
    lifesat +
    log_hhincgcee + 
    relstat2  +
    lfstat +
    wave,
  data = f_men,
  index = c("id", "wave"),
  model = "within"
)


# Women ONLY ----
f_women <- f_reduc %>%
  filter(sex == "Female")

M9 <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + lfstat + wave,
  data = f_women,
  index = c("id", "wave"),
  model = "within"
)

M10 <- plm(
  satrelship ~ benefit_dummy +
    lifesat +
    log_hhincgcee + 
    relstat2  +
    lfstat +
    wave,
  data = f_women,
  index = c("id", "wave"),
  model = "within"
)

M11 <- plm(
  satrelship ~ wohngeld +
    lifesat +
    log_hhincgcee + 
    relstat2  +
    lfstat +
    wave,
  data = f_women,
  index = c("id", "wave"),
  model = "within"
)