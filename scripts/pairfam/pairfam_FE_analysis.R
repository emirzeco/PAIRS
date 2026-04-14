# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              # 
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #
# Author: Emir Zecovic                                                                    #
# Last Update: 11.04.2026                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # #
# FE ANALYIS PAIRFAM  #
# # # # # # # # # # # # 


# M1 (HHInc)----
## M1a ----
M1a <- plm(
  satrelship ~ log_hhincgcee,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M1b ----
M1b <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + lfstat + p_lfstat + 
    nkidsliv + hlt1,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M1c ----
M1c <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + lfstat + p_lfstat + 
    nkidsliv + hlt1 +
    reldur,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M1d ----
M1d <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + lfstat + p_lfstat + 
    nkidsliv + hlt1 +
    wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M1e ----
M1e <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + 
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    reldur + wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# M2 (Grundsicherung) ----
## M2a ----
M2a <- plm(
  satrelship ~ benefit_dummy,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M2b ----
M2b <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)


## M2c ----
M2c <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lifesat,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)


## M2d ----
M2d <- plm(
  satrelship ~ benefit_dummy +
    lifesat +
    log_hhincgcee + 
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #






# M3 (Wohngeld) ----
## M3a ----
M3a <- plm(
  satrelship ~ wohngeld,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

## M3b ----
M3b <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)


## M3c ----
M3c <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + lifesat,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)


## M3d ----
M3d <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + lifesat +
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# In-Work Benefits ----
p_employed <- p_reduc %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Marginal employment",
      "Self-employed"
    )
  )

## HHInc (In-Work Sample) ----
### M4a ----
M4a <- plm(
  satrelship ~ log_hhincgcee,
  data = p_employed,
  index = c("id", "wave"),
  model = "within"
)

### M4b ----
M4b <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + lfstat + p_lfstat + 
    nkidsliv + hlt1,
  data = p_employed,
  index = c("id", "wave"),
  model = "within"
)

### M4c ----
M4c <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    reldur+ wave,
  data = p_employed,
  index = c("id", "wave"),
  model = "within"
)

## Grundsicherung (In-Work Sample) ----
### M5a ----
M5a <- plm(
  satrelship ~ benefit_dummy,
  data = p_employed,
  index = c("id", "wave"),
  model = "within"
)

### M5b ----
M5b <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee,
  data = p_employed,
  index = c("id", "wave"),
  model = "within"
)

### M5c ----
M5c <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lifesat,
  data = p_employed,
  index = c("id", "wave"),
  model = "within"
)

### M5d ----
M5d <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lifesat +
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv +
    reldur + wave,
  data = p_employed,
  index = c("id", "wave"),
  model = "within"
)


## Wohngeld (In-Work Sample) ----
### M6a ----
M6a <- plm(
  satrelship ~ wohngeld,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M6b ----
M6b <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M6c ----
M6c <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + lifesat,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M6d ----
M6d <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + lifesat +
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# BOTH In-Work Benefits ----
p_both_employed <- p_reduc %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Marginal employment",
      "Self-employed"
    ),
    p_lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Marginal employment",
      "Self-employed"
    )
  )
## M7 (HHInc) ----
M7 <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave,
  data = p_both_employed,
  index = c("id", "wave"),
  model = "within"
)

## M8 (Grundsicherung) ----
M8 <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lifesat +
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_both_employed,
  index = c("id", "wave"),
  model = "within"
)

## M9 (Wohngeld) ----
M9 <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + lifesat +
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_both_employed,
  index = c("id", "wave"),
  model = "within"
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Men ONLY ----
p_men <- p_reduc %>%
  filter(sex == "Male")

## M10 ----
M10 <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave,
  data = p_men,
  index = c("id", "wave"),
  model = "within"
)

## M11 ----
M11 <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lifesat +
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv + hlt1 +
    reldur + wave,
  data = p_men,
  index = c("id", "wave"),
  model = "within"
)

## M12 ----
M12 <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + lifesat +
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv + hlt1 +
    reldur + wave,
  data = p_men,
  index = c("id", "wave"),
  model = "within"
)


# Women ONLY ----
p_women <- p_reduc %>%
  filter(sex == "Female")

## M13 ----
M13 <- plm(
  satrelship ~ log_hhincgcee +
    relstat2 + reldur +
    lfstat + p_lfstat + 
    nkidsliv + 
    hlt1 + 
    wave,
  data = p_women,
  index = c("id", "wave"),
  model = "within"
)

## M14 ----
M14 <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lifesat +
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_women,
  index = c("id", "wave"),
  model = "within"
)

## M15 ----
M15 <- plm(
  satrelship ~ wohngeld +
    log_hhincgcee + lifesat +
    relstat2  +
    lfstat + p_lfstat + 
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_women,
  index = c("id", "wave"),
  model = "within"
)