# Summary ----
p %>%
  summarise(
    min_comperz = min(comperz, na.rm = TRUE),
    mean_comperz = mean(comperz, na.rm = TRUE),
    median_comperz = median(comperz, na.rm = TRUE),
    max_comperz = max(comperz, na.rm = TRUE),
    sd_comperz = sd(comperz, na.rm = TRUE)
  )

# # # # # # #
# p %>%
#   select(crn20i1, crn20i2, crn20i3, crn20i4_r) %>%
#   cor(use = "pairwise.complete.obs")
# 
# p %>%
#   count(crn20i1, crn20i2, crn20i3, crn20i4_r) %>%
#   arrange(desc(n))
# 
# 
# p %>%
#   summarise(
#     across(
#       c(crn20i1, crn20i2, crn20i3, crn20i4_r, comperz),
#       ~ mean(.x, na.rm = TRUE)
#     )
#   )
# 
# p %>%
#   select(crn20i1, crn20i2, crn20i3, crn20i4_r, comperz) %>%
#   cor(use = "pairwise.complete.obs")
# 


poverty_trans_table <- p_reduc %>%
  arrange(id, wave) %>%
  group_by(id) %>%
  mutate(
    povertySD_cat_next = dplyr::lead(povertySD_cat)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(povertySD_cat),
    !is.na(povertySD_cat_next)
  ) %>%
  count(povertySD_cat, povertySD_cat_next) %>%
  group_by(povertySD_cat) %>%
  mutate(
    row_total = sum(n),
    pct = n / row_total * 100,
    cell = sprintf("%.2f [%s]", pct, format(n, big.mark = ","))
  ) %>%
  ungroup() %>%
  select(povertySD_cat, povertySD_cat_next, cell, row_total) %>%
  pivot_wider(
    names_from = povertySD_cat_next,
    values_from = cell,
    values_fill = "0.00 [0]"
  ) %>%
  mutate(
    Total = paste0("100 [", format(row_total, big.mark = ","), "]")
  ) %>%
  distinct()

poverty_trans_table
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  













# Models ----
M1a <- plm(
  comperz ~ benefit_dummy ,
  data = p_reduc,
  index = c("id", "wave"),
  model = "random"
)

M2a <- plm(
  comperz ~ benefit_dummy + log_hhincgcee + factor(povertySD_cat),
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

M2b <- plm(
  comperz ~ benefit_dummy +
    factor(povertySD_cat),
  data = p_reduc,
  index = c("id", "wave"),
  model = "within"
)

M2c <- plm(
  comperz ~ benefit_dummy +
    ecodep + lfstat,
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