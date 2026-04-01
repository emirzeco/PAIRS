# Sample reduc ----
f_couples <- subset(f, relstat3 %in% c("LAT", "Cohabiting", "Married"))

## Missings ----
vars_main <- c(
  "age",
  #"sex", "", "school", "educy", "voctrain",
  "hhincgcee", 
  #"sub_fin_hh",
  #"hhincnet", "hhincoecd",
  #"lfstat_filled", #"lfstat",
  #"sd55", "job40",
  "grundsich", "wohngeld", "aII",
  "satrelship",
  "satlife"
  #"pstat", "separation", "sd3",
  #"relstat3_filled" #"relstat"
)

f_reduc <- f_couples
f_reduc[vars_main][f_reduc[vars_main] < 0] <- NA
summary(f_reduc[vars_main])

## Remove NAs ----
prop.table(table(complete.cases(f_reduc[vars_main])))
f_reduc <- f_reduc[complete.cases(f_reduc[vars_main]), ]
#rm(vars_main

## Log-Income ----
#f_reduc$log_hhincnet <- log1p(f_reduc$hhincnet)    ## HH-Income 
f_reduc$log_hhincgcee <- log1p(f_reduc$hhincgcee)  ## HH-Income (Nettoäquivalenzeinkommen, GCEE)
# f_reduc$log_hhincoecd <- log1p(f_reduc$hhincoecd)  ## HH-Income (Nettoäquivalenzeinkommen, OECD)







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
fe_welfare4_couples <- plm(
  satrelship ~ welfare_any + sub_fin_hh + log_hhincgcee + satlife,
  data  = f_reduc,
  index = c("id", "welle"),
  model = "within")


## Grundsicherung ----
m4_couples <- plm(satrelship ~ grundsich +
            log_hhincgcee + sub_fin_hh + 
            satlife,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)

## AGII
m8_couples <- plm(satrelship ~ aII +
            log_hhincgcee + sub_fin_hh + 
            satlife,
          data = f_reduc,
          index = c("id", "welle"),
          model = "within"
)


## Wohngeld ----
m12_couples <- plm(satrelship ~ wohngeld +
             log_hhincgcee + sub_fin_hh + 
             satlife,
           data = f_reduc,
           index = c("id", "welle"),
           model = "within"
           )


modelsummary::msummary(
  list(
    "FE (Welfare receipt)"      = fe_welfare4_couples,
    "FE (Grundsicherung )"      = m4_couples,
    "FE (Arbeitslosengeld II )" = m8_couples,
    "FE (Wohngeld )"            = m12_couples
  ),
  title = "Welfare/Grund./AII/Wohng. x relationship satisfaction (LAT/COHAB/MARRIED only sample)",
  output = "couples_only_models.html",
  coef_map = c(
    "welfare_any"    = "Welfare receipt (Grundsicherung or ALG II) (0-1)",
    "grundsich"      = "Grundsicherung receipt",
    "aII"            = "Arbeitslosengeld II receipt",
    "wohngeld"       = "Wohngeld receipt",
    "log_hhincgcee"  = "Log equivalized household income (GCEE)",
    "sub_fin_hh"     = "Subjective household financial situation (1–6)",
    "satlife"        = "Life satisfaction"
  ),
  estimate  = "{estimate}{stars}",
  statistic = "p = {p.value}",
  stars = c('+' = .10, '*' = .05, '**' = .01, '***' = .001),
  notes = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared")
)