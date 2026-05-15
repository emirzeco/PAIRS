# Out-Of-Work Benfits ----
M1 <- plm(
  rel_happy ~ benefit_OOW + log_hhincoecd + relstat2 + lfstat + wavename + age + age + nchild_dv + cohabn + nchild_dv + health,
  data = uk,
  index = c("pidp", "wavename"),
  model = "within"
)

# In-Work-Benefits ----
M2 <- plm(
  rel_happy ~ benefit_IWB + log_hhincoecd + relstat2 + lfstat + wavename + age + nchild_dv + cohabn + health,
  data = uk,
  index = c("pidp", "wavename"),
  model = "within"
)

summary(M1)
summary(M2)



# Pairfam ----
# M2d <- plm(
#   satrelship ~ benefit_dummy +
#     log_hhincgcee + 
#     relstat2  +
#     lfstat + p_lfstat + 
#     nkidsliv + hlt1 + 
#     reldur + wave,
#   data = p_reduc,
#   index = c("id", "wave"),
#   model = "within"
# )


# Number of children
alabs(uk$nchild_dv)
