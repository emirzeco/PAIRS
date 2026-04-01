# Missings ----
## Missings (fe model only) ----

vars_main <- c(
  "satrelship",
  "grundsich", "aII", "wohngeld",
  "sub_fin_hh"
  )

f_reduc <- f
f_reduc[vars_main][f_reduc[vars_main] < 0] <- NA
summary(f_reduc[vars_main])

## Remove NAs ----
prop.table(table(complete.cases(f_reduc[vars_main])))
f_reduc <- f_reduc[complete.cases(f_reduc[vars_main]), ]
#rm(vars_main)


# Log-Income ----

#f_reduc$log_hhincnet <- log1p(f_reduc$hhincnet)    ## HH-Income 
f_reduc$log_hhincgcee <- log1p(f_reduc$hhincgcee)  ## HH-Income (Nettoäquivalenzeinkommen, GCEE)
# f_reduc$log_hhincoecd <- log1p(f_reduc$hhincoecd)  ## HH-Income (Nettoäquivalenzeinkommen, OECD)





# HH-Income ----
## hhincnet ----
### FE ----
fe_income_hhincnet <- plm(
  satrelship ~ log_hhincnet,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "within"
)

### RE ----
re_income_hhincnet <- plm(
  satrelship ~ log_hhincnet,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "random"
)



## hhincgcee (GCEE) ----
### FE ----
fe_income_hhincgcee <- plm(
  satrelship ~ log_hhincgcee,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "within"
)

### RE ----
re_income_hhincgcee <- plm(
  satrelship ~ log_hhincgcee,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "random"
)




## hhincoecd (OECD) ----
### FE ----
fe_income_hhincoecd <- plm(
  satrelship ~ log_hhincoecd,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "within"
)

### RE ----
re_income_hhincoecd <- plm(
  satrelship ~ log_hhincoecd,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "random"
)

## MSummary ----
modelsummary::msummary(
  list(
    "FE (hhincnet)"  = fe_income_hhincnet,
    "RE (hhincnet)"  = re_income_hhincnet,
    "FE (hhincgcee)" = fe_income_hhincgcee,
    "RE (hhincgcee)" = re_income_hhincgcee,
    "FE (hhincoecd)" = fe_income_hhincoecd,
    "RE (hhincoecd)" = re_income_hhincoecd
  ),
  title = "FE/RE Effect: Relationship satisfaction x HH-Income (hhincnet/hhincgcee/hhincoecd)",
  output = "hhinc_models.html",
  coef_map = c(
    "log_hhincnet"  = "Log household net income",
    "log_hhincgcee" = "Log equivalized household income (GCEE)",
    "log_hhincoecd" = "Log equivalized household income (OECD)"
  ),
  estimate  = "{estimate}{stars}",
  statistic = "p = {p.value}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3
)






# Finanzielle Situation Haushalt (subjektiv) ----
## FE ----
fe_subfin <- plm(
  satrelship ~ sub_fin_hh,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "within"
  )


## RE ----
re_subfin <- plm(
  satrelship ~ sub_fin_hh,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "random"
)



## MSummary ----
modelsummary::msummary(
  list(
    "FE (sub_fin_hh)" = fe_subfin,
    "RE (sub_fin_hh)" = re_subfin
  ),
  title = "FE/RE Effect: Relationship satisfaction x Subjective household financial situation",
  output = "subfin_models.html",
  coef_map = c(
    "sub_fin_hh" = "Subjective household financial situation"
  ),
  estimate  = "{estimate}{stars}",
  statistic = "p = {p.value}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3
)




# Welfare Benefits ----
## Either (Grunds. + AGII) ----

### Keep only valid values (0/1); set labelled missings to NA
f_reduc$grundsich_bin <- ifelse(f_reduc$grundsich %in% c(0, 1), f_reduc$grundsich, NA)
f_reduc$aII_bin       <- ifelse(f_reduc$aII %in% c(0, 1), f_reduc$aII, NA)

### Welfare receipt dummy:
### 1 = received either Grundsicherung or ALG II, 0 = neither
f_reduc$welfare_any <- ifelse(
  is.na(f_reduc$grundsich_bin) & is.na(f_reduc$aII_bin),
  NA,
  ifelse(f_reduc$grundsich_bin == 1 | f_reduc$aII_bin == 1, 1, 0)
)

### FE ----
fe_welfare <- plm(
  satrelship ~ welfare_any,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "within"
)

### RE ----
re_welfare <- plm(
  satrelship ~ welfare_any,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "random"
)


fe_welfare2 <- plm(
  satrelship ~ welfare_any + sub_fin_hh,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "within"
)


fe_welfare3 <- plm(
  satrelship ~ welfare_any + sub_fin_hh + satlife,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "within"
)


fe_welfare4 <- plm(
  satrelship ~ welfare_any + sub_fin_hh + log_hhincgcee + satlife,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "within"
)


## MSummary ----
modelsummary::msummary(
  list(
    "FE (1)" = fe_welfare,
    "RE (2)" = re_welfare,
    "FE (3)" = fe_welfare2,
    "FE (4)" = fe_welfare3,
    "FE (5)" = fe_welfare4
  ),
  title = "Welfare receipt and relationship satisfaction (stepwise controls)",
  output = "welfare_stepwise_models.html",
  coef_map = c(
    "welfare_any"    = "Welfare recept (Grundsicherung or ALG II) (0-1)",
    "sub_fin_hh"     = "Subjective household financial situation (1–6)",
    "satlife"        = "Life satisfaction",
    "log_hhincgcee"  = "Log equivalized household income (GCEE)"
  ),
  estimate  = "{estimate}{stars}",
  statistic = "p = {p.value}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared", "aic", "bic")
)






# Grundsicherung ----
m1 <- plm(satrelship ~ grundsich,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

m2 <- plm(satrelship ~ grundsich +
            log_hhincgcee,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

m3 <- plm(satrelship ~ grundsich +
            log_hhincgcee + sub_fin_hh,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

m4 <- plm(satrelship ~ grundsich +
            log_hhincgcee + sub_fin_hh + 
            satlife,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

modelsummary::msummary(
  list(
    "FE (1)" = m1,
    "FE (2)" = m2,
    "FE (3)" = m3,
    "FE (4)" = m4
  ),
  title = "Grundsicherung and relationship satisfaction (stepwise FE models)",
  output = "grundsicherung_stepwise_models.html",
  coef_map = c(
    "grundsich"      = "Grundsicherung receipt",
    "log_hhincgcee"  = "Log equivalized household income (GCEE)",
    "sub_fin_hh"     = "Subjective household financial situation (1–6)",
    "satlife"        = "Life satisfaction"
  ),
  estimate  = "{estimate}{stars}",
  statistic = "p = {p.value}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared")
)





# AG II ----
m5 <- plm(satrelship ~ aII,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

m6 <- plm(satrelship ~ aII +
            log_hhincgcee,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

m7 <- plm(satrelship ~ aII +
            log_hhincgcee + sub_fin_hh,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

m8 <- plm(satrelship ~ aII +
            log_hhincgcee + sub_fin_hh + 
            satlife,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

modelsummary::msummary(
  list(
    "FE (1)" = m5,
    "FE (2)" = m6,
    "FE (3)" = m7,
    "FE (4)" = m8
  ),
  title = "Arbeitslosengeld II and relationship satisfaction (stepwise FE models)",
  output = "AGII_stepwise_models.html",
  coef_map = c(
    "aII"      = "Arbeitslosengeld II receipt",
    "log_hhincgcee"  = "Log equivalized household income (GCEE)",
    "sub_fin_hh"     = "Subjective household financial situation (1–6)",
    "satlife"        = "Life satisfaction"
  ),
  estimate  = "{estimate}{stars}",
  statistic = "p = {p.value}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared")
)





# Wohngeld ----
m9 <- plm(satrelship ~ wohngeld,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

m10 <- plm(satrelship ~ wohngeld +
            log_hhincgcee,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

m11 <- plm(satrelship ~ wohngeld +
            log_hhincgcee + sub_fin_hh,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

m12 <- plm(satrelship ~ wohngeld +
            log_hhincgcee + sub_fin_hh + 
            satlife,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

modelsummary::msummary(
  list(
    "FE (1)" = m9,
    "FE (2)" = m10,
    "FE (3)" = m11,
    "FE (4)" = m12
  ),
  title = "Wohngeld x relationship satisfaction (stepwise FE models)",
  output = "Wohngeld_stepwise_models.html",
  coef_map = c(
    "wohngeld"            = "Wohngeld receipt",
    "log_hhincgcee"  = "Log equivalized household income (GCEE)",
    "sub_fin_hh"     = "Subjective household financial situation (1–6)",
    "satlife"        = "Life satisfaction"
  ),
  estimate  = "{estimate}{stars}",
  statistic = "p = {p.value}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared")
  )