# ------------------------------------------------------------
# Jia Ying Chua, ActivInsights 2026
# 13Mar2026
# Running examples for Mx, Lx, Cx and Fx functions in
# GENEAcore math.R with minimum test data
# ------------------------------------------------------------

source("R/math.R")

test_data <- data.frame(
  TimeUTC = c(
    1773214226, 1773214246, 1773214271, 1773214301, 1773214336,
    1773214376, 1773214421, 1773214441, 1773214466, 1773214496
  ),
  Duration = c(20, 25, 30, 35, 40, 45, 20, 25, 30, 35),
  AGSAMean = c(0.01, 0.2, 0.03, 0.4, 0.05, 0.01, 0.02, 0.3, 0.4, 0.5)
)

# Most and least active 1 minute
mx_lx <- mx_lx_window(
  bouts = test_data,
  window = 1 * 60,
  low = TRUE
)
M1Intensity <- mx_lx$M_intensity
M1Time <- mx_lx$M_time
L1Intensity <- mx_lx$L_intensity
L1Time <- mx_lx$L_time

# 75th percentile of AGSA
c75 <- cx_percentile(
  bouts = test_data,
  percentile = 75,
  value_col = "AGSAMean",
  duration_col = "Duration"
)

# Most and least number of bouts to reach 30 seconds of active time
f30 <- fx_count(
  bouts = test_data,
  AGSA_threshold = 0.0625,
  duration = 30
)
Mf30Count <- f30$M_count
Mf30Time <- f30$M_time
Lf30Count <- f30$L_count
Lf30Time <- f30$L_time
