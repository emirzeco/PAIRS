# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# R code for                                                                              #
# FReDA Dissertation Paper Emir Zecovic (11.2025)                                         #
# Means-Tested Benefits and Relationship Satisfaction among Low-Income Couples in Germany #                                                                
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # #
# DATA PREPERATION  #
# # # # # # # # # # #

# Setup ----
## Packages ----
if ("convenience" %in% rownames(installed.packages()) ==F) {
  devtools::install_github("ratsupaltuf/convenience", force=T)
}

packages <- c("tidyverse", "haven", "pastecs", "datawizard", "convenience",
              "ggplot2", "ggrepel", "sjPlot", "lme4", "knitr", "kableExtra", 
              "stringr", "flextable", "officer", "sf", "plm", "stargazer",
              "patchwork", "tidytext", "sjlabelled")
install.packages(setdiff(packages, rownames(installed.packages())))
suppressMessages(lapply(packages, library, character.only = TRUE, quietly=T))
rm(packages)
options(max.print=10000)


## Load ----
#f_long<-haven::read_dta("FREDAanchor_long.dta")
p <-haven::read_dta("pairfam_long.dta")


p <- select(p,
            "id", "wave", "source_file", sex = "original_sex",
            
            "srs1i4",                                           # Present: Importance living in a partnership (w1 - w11)
            "sd3",                                              # Current relationship status: Partnership existent (w1)
            "sd27",
            "pa16i1", "pa16i2", "pa16i3",                       # Quality of relationship indicators (CASI) (w1 - w13; two-year)
            "pa16i4", "pa16i5", "pa16i6",                       # Quality of relationship indicators (CASI) (w1 - w13; two-year)
            
            satrelship   = "sat3",
            p_satrelship = "sat4",
            lifestat     = "sat6", # Life satisfaction (w1 - w14)
            
            "pa21i1", # Disagree with partner: Spending leisure time 	                (w3 - w13; two-year)
            "pa21i2", # Disagree with partner: Division of Chores                     (w3 - w13; two-year)
            "pa21i3", # Disagree with partner: Financial matters                      (w3 - w13; two-year) 
            "pa21i4", # Disagree with partner: Involvement in career/education/School (w3 - w13; two-year)
            "pa21i5", # Disagree with partner: Dealing with each other                (w3 - w13; two-year)
            
            netinc     = "inc2",  # # Net income last month (w1 - w14)
            hhinc      = "inc13", # Monthly net household income (w1 - w14)
            hhincshare = "inc24", # Own share in household income (per cent) (w2 - w3)
            sub_fin_hh = "inc28", # Satisfaction with household's financial situation (w1 - w14)
            
            "inc27i2", # HH: Wir müssen häufig verzichten, wegen finanzieller Einschränkungen
            "inc27i3", # HH: Bei uns ist das Geld meistens knapp
            
            #"inc10i1",             # Kindergeld
            #"inc10i2",             # Lohnfortzahlung im Mutterschutz
            #"inc10i3",             # Elterngeld
            wohngeld  = "inc10i4",  # Wohngeld oder Lastenzuschuss
            #"inc10i5",             # Leistungen der Pflegeversicherung
            sozhilfe  = "inc10i7",  # Sozialhilfe
            aI        =  "inc10i8", # Arbeitslosengeld I (ALG I)
            aII       = "inc10i9",  # Arbeitslosengeld II einschließlich Sozialgeld
            grundsich = "inc10i10", # Grundsicherung im Alter und bei Erwerbsminderung
            krankgeld = "inc10i11", # Krankengeld
            )







# Missings ----

## satrelship ----
p$satrelship <- ifelse(p$satrelship %in% 0:10, as.numeric(p$satrelship), NA)

## Benefits ----
p <- p %>%
  dplyr::mutate(
    wohngeld = dplyr::case_when(
      as.numeric(wohngeld) %in% c(0, 7) ~ 0,
      as.numeric(wohngeld) == 1         ~ 1,
      TRUE                              ~ NA_real_
    ),
    sozhilfe = dplyr::case_when(
      as.numeric(sozhilfe) %in% c(0, 7) ~ 0,
      as.numeric(sozhilfe) == 1         ~ 1,
      TRUE                              ~ NA_real_
    ),
    aII = dplyr::case_when(
      as.numeric(aII) %in% c(0, 7) ~ 0,
      as.numeric(aII) == 1         ~ 1,
      TRUE                         ~ NA_real_
    ),
    grundsich = dplyr::case_when(
      as.numeric(grundsich) %in% c(0, 7) ~ 0,
      as.numeric(grundsich) == 1         ~ 1,
      TRUE                               ~ NA_real_
    )
  )

## Lifestat and finance_sat ----
p <- p %>%
  dplyr::mutate(
    lifestat   = ifelse(as.numeric(lifestat)   %in% 0:10, as.numeric(lifestat), NA),
    sub_fin_hh = ifelse(as.numeric(sub_fin_hh) %in% 0:10, as.numeric(sub_fin_hh), NA)
  )



## Income ----
p$hhinc <- ifelse(p$hhinc >= 0, as.numeric(p$hhinc), NA)
p$log_hhinc <- log1p(p$hhinc)

### Quartiles ----
qcuts <- quantile(p$hhinc, probs = c(.25, .50, .75), na.rm = TRUE)

p$inc_quartile <- with(p, dplyr::case_when(
  is.na(hhinc) ~ NA_character_,
  hhinc <= qcuts[1] ~ "Q1 (lowest)",
  hhinc <= qcuts[2] ~ "Q2",
  hhinc <= qcuts[3] ~ "Q3",
  hhinc >  qcuts[3] ~ "Q4 (highest)"
))

p$inc_quartile <- factor(
  p$inc_quartile,
  levels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")
)

table(p$inc_quartile, useNA = "ifany")

### Low-income ----
p <- p %>%
  dplyr::mutate(
    low_income_q1 = dplyr::case_when(
      is.na(inc_quartile)                 ~ NA_real_,
      inc_quartile == "Q1 (lowest)"       ~ 1,
      TRUE                                ~ 0
    )
  )

table(p$low_income_q1, useNA = "ifany")


table(p$wohngeld, useNA = "ifany")
table(p$aII, useNA = "ifany")
table(p$grundsich, useNA = "ifany")
table(p$satrelship, useNA = "ifany")
table(p$log_hhinc, useNA = "ifany")




## Welfare (any) ----
p <- p %>%
  dplyr::mutate(
    welfare_any = ifelse(
      is.na(sozhilfe) & is.na(grundsich),
      NA,
      ifelse(sozhilfe == 1 | grundsich == 1, 1, 0)
    )
  )
table(p$welfare_any, useNA = "ifany")


# Clean ----
vars_main <- c(
  "satrelship",
  "grundsich", "sozhilfe", "aII", "wohngeld",
  "sub_fin_hh", "lifestat", "log_hhinc", "inc_quartile", "low_income_q1"
)

p_reduc <- p


## Remove NAs ----
prop.table(table(complete.cases(p_reduc[vars_main])))
p_reduc <- p_reduc[complete.cases(p_reduc[vars_main]), ]
#rm(vars_main)