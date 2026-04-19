# FE ---- 
## Grundischerung ----
### M100a ----
M100a <- plm(
  satrelship ~ benefit_dummy +
    relstat2  +
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

## Income ----
### M101a ----
M101a <- plm(
  satrelship ~ log_hhincgcee +
    relstat2  +
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

## Wohngeld ----
### M102a ----
M102a <- plm(
  satrelship ~ wohngeld +
    relstat2  +
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

## LFS ----
### M103a ----
M103a <- plm(
  satrelship ~ lfstat,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M103b ----
M103b <- plm(
  satrelship ~ lfstat +
    relstat2  +
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)


## P_LFS ----
### M104a ----
M104a <- plm(
  satrelship ~ p_lfstat,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M104b ----
M104b <- plm(
  satrelship ~ p_lfstat +
    relstat2  +
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)


## Combo ----
### M105a ----
M105a <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lfstat,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M105b ----
M105b <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lfstat + p_lfstat  +
    relstat2  +
    nkidsliv + hlt1 + 
    reldur + wave,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

## In-Work only ----
p_employed <- p_reduc %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Marginal employment",
      "Self-employed"
    )
  )

### M106a ----
M106a <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lfstat,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

### M106b ----
M106b <- plm(
  satrelship ~ benefit_dummy +
    log_hhincgcee + lfstat + p_lfstat  +
    relstat2  +
    nkidsliv + hlt1 + 
    reldur + wave,,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)


###  M107c ----
M106c <- plm(
  satrelship ~ benefit_dummy*lfstat  +
    log_hhincgcee + lfstat +
    relstat2  +
    nkidsliv + hlt1 + 
    reldur + wave,,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)














# Graph ----
scale_x_continuous(breaks = 1:max(benefit_wave$wave))
benefit_wave <- p_reduc %>%
  group_by(wave) %>%
  summarise(
    n_benefit = sum(benefit_dummy == 1, na.rm = TRUE)
  )

ggplot(benefit_wave, aes(x = wave, y = n_benefit)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  scale_x_continuous(breaks = 1:max(benefit_wave$wave)) +
  labs(
    x = "Wave",
    y = "Number receiving benefits",
    title = "Number of welfare recipients over time"
  ) +
  theme_minimal()