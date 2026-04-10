# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #
# Author: Emir Zecovic                                                                    #
# Last Update: 10.04.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # #
# ANALYIS PAIRFAM #
# # # # # # # # # # 

# FE ----
## M1 (HHInc)----
### M1a ----
M1a <- plm(
  satrelship ~ log_hhincgcee,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M1b ----
M1b <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + lfstat + p_lfstat + 
    nkidsliv + hlt1,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M1c ----
M1c <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + lfstat + p_lfstat + 
    nkidsliv + hlt1 +
    wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M1d ----
M1d <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + lfstat + p_lfstat + 
    nkidsliv + hlt1 +
    reldur,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M1e ----
M1e <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



## M2 (Grundsicherung) ----
### M2a ----
M2a <- plm(
  satrelship ~ benefit_dummy,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M2b ----
M2b <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)


### M2c ----
M2c <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lifesat,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)


### M2d ----
M2d <- plm(
  satrelship ~ benefit_dummy +
    lifesat +
    log_hhincgcee + 
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv + reldur +
    hlt1 + 
    wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #






## M3 (Wohngeld) ----
### M3a ----
M3a <- plm(
  satrelship ~ wohngeld,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M3b ----
M3b <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)


### M3c ----
M3c <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + lifesat,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)


### M3d ----
M3d <- plm(
  satrelship ~ wohngeld +
    lifesat +
    log_hhincgcee + 
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv + reldur +
    hlt1 + 
    wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




# RE ----
## M4 (HHInc) ----
### M4a ----
M4a <- plm(
  satrelship ~ log_hhincgcee,
  data = p_reduc,
  index = c("id", "wave"),
  model = "random"
)

### M4b ----
M4b <- plm(
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























# # # # # #  # # # # # # # # # # # # # # #
# # In-Work Benefits (FE) ----
# p_employed <- p_reduc %>%
#   filter(
#     lfstat %in% c(
#       "Full-time employed",
#       "Part-time employed",
#       "Marginal employment",
#       "Self-employed"
#     )
#   )