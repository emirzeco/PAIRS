# Summary ----
p %>%
  summarise(
    min_comperz = min(comperz, na.rm = TRUE),
    mean_comperz = mean(comperz, na.rm = TRUE),
    median_comperz = median(comperz, na.rm = TRUE),
    max_comperz = max(comperz, na.rm = TRUE),
    sd_comperz = sd(comperz, na.rm = TRUE)
  )


# Models ----
M1a <- plm(
  comperz ~ log_hhincgcee,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

M2a <- plm(
  comperz ~ benefit_dummy,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

M2b <- plm(
  comperz ~ benefit_dummy +
    log_hhincgcee,
  data = p_reduc,
  
  index = c("id", "wave"),
  model = "within"
)

M2c <- plm(
  comperz ~ benefit_dummy +
    log_hhincgcee + lfstat,
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)













  


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Different samples ----
## In-Work Benefits ----
p_employed <- p_reduc %>%
  filter(
    lfstat %in% c(
      "Full-time employed",
      "Part-time employed",
      "Marginal employment",
      "Self-employed"
    )
  )

p_men <- p_reduc %>%
  filter(sex == "Male")
p_women <- p_reduc %>%
  filter(sex == "Female")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 