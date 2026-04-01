# HH-Income ----
## FE ----
fe_income_hhinc <- plm(
  satrelship ~ log_hhinc,
  data  = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

fe_income_hhinc2 <- plm(
  satrelship ~ log_hhinc + sub_fin_hh,
  data  = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

fe_income_hhinc3 <- plm(
  satrelship ~ log_hhinc + sub_fin_hh + lifestat + inc27i2,
  data  = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

modelsummary::msummary(
  list(
    "FE (1)" = fe_income_hhinc,
    "FE (2)" = fe_income_hhinc2,
    "FE (3)" = fe_income_hhinc3
  ),
  title = "HH-Income and relationship satisfaction FE (stepwise controls) [PAIRFAM]",
  output = "hhinc_stepwise_FE_models_pairfam.html",
  coef_map = c(
    "log_hhinc"       = "Logged monthly household net income",
    "sub_fin_hh"      = "Subjective household financial situation (0-10)",
    "lifestat"        = "Life satisfaction (0-10)",
    "inc27i2"         = "HH financial deprivation (1-5)"
  ),
  estimate  = "{estimate}{stars}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared", "aic", "bic")
)




## RE ----
re_income_hhinc <- plm(
  satrelship ~ log_hhinc,
  data  = p_reduc,
  index = c("id", "wave"),
  model = "random"
)

re_income_hhinc2 <- plm(
  satrelship ~ log_hhinc + sub_fin_hh,
  data  = p_reduc,
  index = c("id", "wave"),
  model = "random"
)

re_income_hhinc3 <- plm(
  satrelship ~ log_hhinc + sub_fin_hh + lifestat + inc27i2,
  data  = p_reduc,
  index = c("id", "wave"),
  model = "random"
)

modelsummary::msummary(
  list(
    "RE (1)" = re_income_hhinc,
    "RE (2)" = re_income_hhinc2,
    "RE (3)" = re_income_hhinc3
  ),
  title = "HH-Income and relationship satisfaction RE (stepwise controls) [PAIRFAM]",
  output = "hhinc_stepwise_RE_models_pairfam.html",
  coef_map = c(
    "log_hhinc"       = "Logged monthly household net income",
    "sub_fin_hh"      = "Subjective household financial situation (0-10)",
    "lifestat"        = "Life satisfaction (0-10)",
    "inc27i2"         = "HH financial deprivation (1-5)"
  ),
  estimate  = "{estimate}{stars}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared", "aic", "bic")
)












# Welfare ----
## Any ----
fe_welfare <- plm(
  satrelship ~ welfare_any,
  data  = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

fe_welfare2 <- plm(
  satrelship ~ welfare_any + sub_fin_hh,
  data  = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

fe_welfare3 <- plm(
  satrelship ~ welfare_any + sub_fin_hh + lifestat,
  data  = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

fe_welfare4 <- plm(
  satrelship ~ welfare_any + sub_fin_hh + lifestat + log_hhinc,
  data  = p_reduc,
  index = c("id", "wave"),
  model = "within"
)



modelsummary::msummary(
  list(
    "FE (1)" = fe_welfare,
    "FE (3)" = fe_welfare2,
    "FE (4)" = fe_welfare3,
    "FE (5)" = fe_welfare4
  ),
  title = "Welfare receipt and relationship satisfaction (stepwise controls) [PAIRFAM]",
  output = "welfare_stepwise_models_pairfam.html",
  coef_map = c(
    "welfare_any"    = "Welfare recept (Grundsicherung or ALG II) (0-1)",
    "sub_fin_hh"     = "Subjective household financial situation (0-10)",
    "lifestat"        = "Life satisfaction (0-10)",
    "log_hhinc"  = "Logged monthly household net income"
  ),
  estimate  = "{estimate}{stars}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared", "aic", "bic")
)








# Grundsicherung ----
m1 <- plm(satrelship ~ grundsich,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)

m2 <- plm(satrelship ~ grundsich +
            log_hhinc,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)

m3 <- plm(satrelship ~ grundsich +
            log_hhinc + sub_fin_hh,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)

m4 <- plm(satrelship ~ grundsich +
            log_hhinc + sub_fin_hh + 
            lifestat,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)


modelsummary::msummary(
  list(
    "FE (1)" = m1,
    "FE (2)" = m2,
    "FE (3)" = m3,
    "FE (4)" = m4
  ),
  title = "Grundsicherung and relationship satisfaction (stepwise FE models) [PAIRFAM]",
  output = "grundsicherung_stepwise_models_pairfam.html",
  coef_map = c(
    "grundsich"      = "Grundsicherung receipt",
    "log_hhinc"      = "Logged monthly household net income",
    "sub_fin_hh"     = "Subjective household financial situation (0-10)",
    "lifestat"       = "Life satisfaction (0-10)"
  ),
  estimate  = "{estimate}{stars}",
  stars = c('+' = .10, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared")
)





# Sozialgeld ----
m20 <- plm(satrelship ~ sozhilfe,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)

m21 <- plm(satrelship ~ sozhilfe +
            log_hhinc,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)

m22 <- plm(satrelship ~ sozhilfe +
            log_hhinc + sub_fin_hh,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)

m23 <- plm(satrelship ~ sozhilfe +
            log_hhinc + sub_fin_hh + 
            lifestat,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)

modelsummary::msummary(
  list(
    "FE (1)" = m20,
    "FE (2)" = m21,
    "FE (3)" = m22,
    "FE (4)" = m23
  ),
  title = "Sozialhilfe and relationship satisfaction (stepwise FE models) [PAIRFAM]",
  output = "sozialhilfe_stepwise_models_pairfam.html",
  coef_map = c(
    "sozhilfe"       = "Sozialhilfe receipt",
    "log_hhinc"      = "Logged monthly household net income",
    "sub_fin_hh"     = "Subjective household financial situation (0-10)",
    "lifestat"       = "Life satisfaction (0-10)"
  ),
  estimate  = "{estimate}{stars}",
  stars = c('+' = .10, '*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared")
)






# AG II ----
m5 <- plm(satrelship ~ aII,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)

m6 <- plm(satrelship ~ aII +
            log_hhinc,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)

m7 <- plm(satrelship ~ aII +
            log_hhinc + sub_fin_hh,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)

m8 <- plm(satrelship ~ aII +
            log_hhinc + sub_fin_hh + 
            lifestat,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)


modelsummary::msummary(
  list(
    "FE (1)" = m5,
    "FE (2)" = m6,
    "FE (3)" = m7,
    "FE (4)" = m8
  ),
  title = "Arbeitslosengeld II and relationship satisfaction (stepwise FE models) [PAIRFAM]",
  output = "AGII_stepwise_models_pairfam.html",
  coef_map = c(
    "aII"            = "Arbeitslosengeld II receipt",
    "log_hhinc"      = "Logged monthly household net income",
    "sub_fin_hh"     = "Subjective household financial situation (0-10)",
    "lifestat"       = "Life satisfaction (0-10)"
  ),
  estimate  = "{estimate}{stars}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared")
)







# Wohngeld ----
m9 <- plm(satrelship ~ wohngeld,
          data = p_reduc,
          index = c("id", "wave"),
          model = "within"
)

m10 <- plm(satrelship ~ wohngeld +
             log_hhinc,
           data = p_reduc,
           index = c("id", "wave"),
           model = "within"
)

m11 <- plm(satrelship ~ wohngeld +
             log_hhinc + sub_fin_hh,
           data = p_reduc,
           index = c("id", "wave"),
           model = "within"
)

m12 <- plm(satrelship ~ wohngeld +
             log_hhinc + sub_fin_hh + 
             lifestat,
           data = p_reduc,
           index = c("id", "wave"),
           model = "within"
)


modelsummary::msummary(
  list(
    "FE (1)" = m9,
    "FE (2)" = m10,
    "FE (3)" = m11,
    "FE (4)" = m12
  ),
  title = "Wohngeld and relationship satisfaction (stepwise FE models) [PAIRFAM]",
  output = "Wohngeld_stepwise_models_pairfam.html",
  coef_map = c(
    "wohngeld"            = "Wohngeld receipt",
    "log_hhinc"           = "Logged monthly household net income",
    "sub_fin_hh"          = "Subjective household financial situation (0-10)",
    "lifestat"            = "Life satisfaction (0-10)"
  ),
  estimate  = "{estimate}{stars}",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared")
)




