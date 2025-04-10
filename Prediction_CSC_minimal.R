# Load libraries and data -------------------------------------------------

# General packages (riskRegression version should be >= 2021.10.10)
pkgs <- c("survival", "pec", "splines", "geepack", "riskRegression")
vapply(
  pkgs,
  function(pkg) {
    if (!require(pkg, character.only = TRUE)) install.packages(pkg)
    require(pkg, character.only = TRUE, quietly = TRUE)
  },
  FUN.VALUE = logical(length = 1L)
)

# Load datasets
rdata <- readRDS("Data/rdata.rds")
vdata <- readRDS("Data/vdata.rds")

# Set seed (for bootstrapping)
set.seed(2021)


# Fit cause-specific hazards models ---------------------------------------

fit_csh <- CSC(
  formula = Hist(time, status_num) ~ age + size + ncat + hr_status,
  data = rdata
)

# External validation at 5 years for primary event
horizon <- 5
primary_event <- 1 # Set to 2 if cause 2 was of interest

score_vdata <- Score(
  list("csh_validation" = fit_csh),
  formula = Hist(time, status_num) ~ 1,
  cens.model = "km",
  data = vdata,
  conf.int = TRUE,
  times = horizon,
  metrics = c("auc", "brier"),
  summary = c("ipa"),
  cause = primary_event,
  plots = "calibration"
)

# Calculate estimated risk for each patient (in validation data) by time horizon
pred <- predictRisk(
  object = fit_csh,
  cause = primary_event,
  newdata = vdata,
  times = horizon
)


# Calibration plot (pseudo-obs approach) ----------------------------------

calplot_pseudo <- plotCalibration(
  x = score_vdata,
  brier.in.legend = FALSE,
  auc.in.legend = FALSE,
  cens.method = "pseudo",
  bandwidth = 0.05, # leave as NULL for default choice of smoothing
  cex = 1,
  round = FALSE, # Important, keeps all unique risk estimates rather than rounding
  xlim = c(0, 0.6),
  ylim = c(0, 0.6),
  rug = TRUE,
  xlab = "Estimated risks",
  ylab = "Observed outcome proportions"
)

# We can extract predicted and observed, observed will depend on degree of smoothing (bandwidth)
dat_pseudo <- calplot_pseudo$plotFrames$csh_validation


# Calibration plot (pseudo-obs approach, loess smoothing) -----------------

# This approach will also yield confidence intervals:

# Use pseudo-observations calculated by Score() (can alternatively use pseudo::pseudoci)
pseudos <- data.frame(score_vdata$Calibration$plotframe)
pseudos <- pseudos[order(pseudos$risk), ]

# Note:
# - 'pseudos' is the data.frame with ACTUAL pseudo-observations, not the smoothed ones
# - Column ID is not the id in vdata; it is just a number assigned to each row of
# the original validation data sorted by time and event indicator
head(pseudos$pseudovalue) # the pseudo-observations

# Use linear loess (weighted local regression with polynomial degree = 1) smoothing
smooth_pseudos <- predict(
  stats::loess(pseudovalue ~ risk, data = pseudos, degree = 1, span = 0.33),
  se = TRUE
)

# Calibration plot (reported in manuscript):

# First, prepare histogram of estimated risks for x-axis
spike_bounds <- c(-0.075, 0)
bin_breaks <- seq(0, 0.6, length.out = 100 + 1)
freqs <- table(cut(pred, breaks = bin_breaks))
bins <- bin_breaks[-1]
freqs_valid <- freqs[freqs > 0]
freqs_rescaled <- spike_bounds[1] +
  (spike_bounds[2] - spike_bounds[1]) *
    (freqs_valid - min(freqs_valid)) /
    (max(freqs_valid) - min(freqs_valid))

# Produce plot
plot(
  x = pseudos$risk,
  y = pseudos$pseudovalue,
  xlim = c(0, 0.6),
  ylim = c(spike_bounds[1], 0.6),
  yaxt = "n",
  frame.plot = FALSE,
  xlab = "Estimated risks",
  ylab = "Observed outcome proportions",
  type = "n"
)
axis(2, seq(0, 0.6, by = 0.1), labels = seq(0, 0.6, by = 0.1))
polygon(
  x = c(pseudos$risk, rev(pseudos$risk)),
  y = c(
    pmax(
      smooth_pseudos$fit - qt(0.975, smooth_pseudos$df) * smooth_pseudos$se,
      0
    ),
    rev(smooth_pseudos$fit + qt(0.975, smooth_pseudos$df) * smooth_pseudos$se)
  ),
  border = FALSE,
  col = "lightgray"
)
abline(a = 0, b = 1, col = "gray")
lines(x = pseudos$risk, y = smooth_pseudos$fit, lwd = 2)
segments(
  x0 = bins[freqs > 0],
  y0 = spike_bounds[1],
  x1 = bins[freqs > 0],
  y1 = freqs_rescaled
)


# Calibration plot (flexible regression approach) -------------------------

# Add estimated risk and complementary log-log of it to dataset
vdata$pred <- pred
vdata$cll_pred <- log(-log(1 - pred))

# 5 knots seems to give somewhat equivalent graph to pseudo method with bw = 0.05
n_internal_knots <- 5 # Austin et al. advise to use between 3 (more smoothing, less flexible) and 5 (less smoothing, more flexible)
rcs_vdata <- ns(vdata$cll_pred, df = n_internal_knots + 1)
colnames(rcs_vdata) <- paste0("basisf_", colnames(rcs_vdata))
vdata_bis <- cbind.data.frame(vdata, rcs_vdata)

# Use subdistribution hazards (Fine-Gray) model
form_fgr <- reformulate(
  termlabels = colnames(rcs_vdata),
  response = "Hist(time, status_num)"
)

# Regress subdistribution of event of interest on cloglog of estimated risks
calib_fgr <- FGR(formula = form_fgr, cause = primary_event, data = vdata_bis)

# Add observed and predicted together in a data frame
dat_fgr <- cbind.data.frame(
  "obs" = predict(calib_fgr, times = horizon, newdata = vdata_bis),
  "pred" = vdata$pred
)

# Calibration plot
dat_fgr <- dat_fgr[order(dat_fgr$pred), ]
plot(
  x = dat_fgr$pred,
  y = dat_fgr$obs,
  type = "l",
  frame.plot = FALSE,
  xlim = c(0, 0.6),
  ylim = c(0, 0.6),
  xlab = "Estimated risks",
  ylab = "Observed outcome proportions"
)
abline(a = 0, b = 1, col = "gray")


# Plot calibration plots all methods together
plot(
  x = pseudos$risk,
  y = pseudos$pseudovalue,
  xlim = c(0, 0.6),
  ylim = c(spike_bounds[1] + 0.025, 0.6),
  yaxt = "n",
  type = "n",
  frame.plot = FALSE,
  xlab = "Estimated risks",
  ylab = "Observed outcome proportions"
)
axis(2, seq(0, 0.6, by = 0.1), labels = seq(0, 0.6, by = 0.1))
abline(a = 0, b = 1, col = "gray")
lines(x = pseudos$risk, y = smooth_pseudos$fit, lwd = 2, lty = 3, col = "blue")
lines(
  x = dat_pseudo$Pred,
  y = dat_pseudo$Obs,
  col = "lightblue",
  lwd = 2,
  lty = 1
)
lines(x = dat_fgr$pred, y = dat_fgr$obs, col = "black", lwd = 2, lty = 4)
legend(
  x = -0.025,
  y = 0.65,
  legend = c(
    "Subdistribution",
    "Pseudo-obs (NN smoothing)",
    "Pseudo-obs (LOESS smoothing)"
  ),
  col = c("black", "lightblue", "blue"),
  lty = c(4, 1, 3),
  lwd = rep(2, 3),
  bty = "n"
)
segments(
  x0 = bins[freqs > 0],
  y0 = spike_bounds[1] + 0.025,
  x1 = bins[freqs > 0],
  y1 = freqs_rescaled + 0.025
)


# Calibration (O/E) -------------------------------------------------------

# First calculate Aalen-Johansen estimate (as 'observed')
obj <- summary(survfit(Surv(time, status) ~ 1, data = vdata), times = horizon)
aj <- list(
  "obs" = obj$pstate[, primary_event + 1],
  "se" = obj$std.err[, primary_event + 1]
)

# Calculate O/E
OE <- aj$obs / mean(pred)

# For the confidence interval we use method proposed in Debray et al. (2017) doi:10.1136/bmj.i6460
OE_summary <- c(
  "OE" = OE,
  "lower" = exp(log(OE) - qnorm(0.975) * aj$se / aj$obs),
  "upper" = exp(log(OE) + qnorm(0.975) * aj$se / aj$obs)
)
OE_summary


# Calibration intercept/slope ---------------------------------------------

# Add the cloglog risk estimates to dataset with pseudo-observations
pseudos$cll_pred <- log(-log(1 - pseudos$risk))

# Fit model for calibration intercept
fit_cal_int <- geese(
  pseudovalue ~ offset(cll_pred),
  data = pseudos,
  id = ID,
  scale.fix = TRUE,
  family = gaussian,
  mean.link = "cloglog",
  corstr = "independence",
  jack = TRUE
)

# Fit model for calibration slope
fit_cal_slope <- geese(
  pseudovalue ~ offset(cll_pred) + cll_pred,
  data = pseudos,
  id = ID,
  scale.fix = TRUE,
  family = gaussian,
  mean.link = "cloglog",
  corstr = "independence",
  jack = TRUE
)

# Perform joint test on intercept and slope
betas <- fit_cal_slope$beta
vcov_mat <- fit_cal_slope$vbeta
wald <- drop(betas %*% solve(vcov_mat) %*% betas)
pchisq(wald, df = 2, lower.tail = FALSE)

# Value, confidence interval and test for calibration slope
summary(fit_cal_slope)
with(
  summary(fit_cal_slope)$mean["cll_pred", ],
  c(
    "slope" = 1 + estimate,
    `2.5 %` = 1 + (estimate - qnorm(0.975) * san.se),
    `97.5 %` = 1 + (estimate + qnorm(0.975) * san.se)
  )
)

# Value, confidence interval and test for calibration intercept
summary(fit_cal_int)
with(
  summary(fit_cal_int)$mean,
  c(
    "intercept" = estimate,
    `2.5 %` = estimate - qnorm(0.975) * san.se,
    `97.5 %` = estimate + qnorm(0.975) * san.se
  )
)


# Discrimination ----------------------------------------------------------

# AUC as described in paper - same as AUC_2 from timeROC::timeROC()
score_vdata$AUC$score

# C-index
cindex_csh <- pec::cindex(
  object = fit_csh,
  formula = Hist(time, status_num) ~ 1,
  cause = primary_event,
  eval.times = horizon,
  data = vdata
)$AppCindex$CauseSpecificCox

cindex_csh
# Optional bootstrap for C-index confidence interval at the end of this code

# Prediction error --------------------------------------------------------

# Brier score + scaled Brier score (here named index of prediction accuracy-IPA)
score_vdata$Brier$score
# Optional bootstrap for IPA at the end of this code

# Decision curve analysis -------------------------------------------------

# 1. Set grid of thresholds
thresholds <- seq(0, 0.6, by = 0.01)

# 2. Calculate Aalen-Johansen estimator for all patients exceeding the threshold (i.e. treat-all)
survfit_all <- summary(
  survfit(Surv(time, status) ~ 1, data = vdata),
  times = horizon
)
f_all <- survfit_all$pstate[primary_event + 1]

# 3. Calculate Net Benefit across all thresholds
list_nb <- lapply(thresholds, function(ps) {
  # Treat all
  NB_all <- f_all - (1 - f_all) * (ps / (1 - ps))

  # Based on threshold
  p_exceed <- mean(vdata$pred > ps)
  survfit_among_exceed <- try(
    summary(
      survfit(Surv(time, status) ~ 1, data = vdata[vdata$pred > ps, ]),
      times = horizon
    ),
    silent = TRUE
  )

  # If a) no more observations above threshold, or b) among subset exceeding..
  # ..no individual has event time >= horizon, then NB = 0
  if (class(survfit_among_exceed) == "try-error") {
    NB <- 0
  } else {
    f_given_exceed <- survfit_among_exceed$pstate[primary_event + 1]
    TP <- f_given_exceed * p_exceed
    FP <- (1 - f_given_exceed) * p_exceed
    NB <- TP - FP * (ps / (1 - ps))
  }

  # Return together
  df_res <- data.frame("threshold" = ps, "NB" = NB, "treat_all" = NB_all)
  return(df_res)
})

# Combine into data frame
df_nb <- do.call(rbind.data.frame, list_nb)
head(df_nb)

# Make basic decision curve plot
par(
  xaxs = "i",
  yaxs = "i",
  las = 1,
  mar = c(6.1, 5.8, 4.1, 2.1),
  mgp = c(4.25, 1, 0)
)
plot(
  df_nb$threshold,
  df_nb$NB,
  type = "l",
  lwd = 2,
  ylim = c(-0.1, 0.1),
  xlim = c(0, 0.5),
  xlab = "",
  ylab = "Net Benefit",
  bty = "n",
  xaxt = "n"
)
lines(df_nb$threshold, df_nb$treat_all, type = "l", col = "darkgray", lwd = 2)
abline(h = 0, lty = 2, lwd = 2)
legend(
  "topright",
  c("Treat all", "Treat none", "Prediction model"),
  lwd = c(2, 2, 2),
  lty = c(1, 2, 1),
  col = c("darkgray", "black", "black"),
  bty = "n"
)
axis(side = 1, at = c(0, 0.1, 0.2, 0.3, 0.4, 0.5))
axis(
  side = 1,
  pos = -0.145,
  at = c(0.1, 0.2, 0.3, 0.4, 0.5),
  labels = c("1:9", "1:4", "3:7", "2:3", "1:1")
)
mtext("Threshold probability", 1, line = 2)
mtext("Harm to benefit ratio", 1, line = 5)
#title("Validation data")

# Restore old graphical parameters
dev.off()


# Optional: bootstrap confidence intervals --------------------------------

# Validate final model in resampled test datasets

# Number of bootstrap samples
B <- 100

boots_ls <- lapply(seq_len(B), function(b) {
  # Resample validation data
  vdata_boot <- vdata[sample(nrow(vdata), replace = TRUE), ]

  # Get cindex on boot validation data
  cindex_boot <- pec::cindex(
    object = fit_csh,
    formula = Hist(time, status_num) ~ 1,
    cause = 1,
    eval.times = horizon,
    data = vdata_boot,
    verbose = FALSE
  )$AppCindex$CauseSpecificCox

  # Get IPA on boot validation data
  score_boot <- Score(
    list("csh_validation" = fit_csh),
    formula = Hist(time, status_num) ~ 1,
    cens.model = "km",
    data = vdata_boot,
    conf.int = FALSE,
    times = horizon,
    metrics = c("brier"),
    summary = c("ipa"),
    cause = primary_event
  )

  # .. can add other measures here, eg. E50/E90/net-benefit

  ipa_boot <- score_boot$Brier$score[model == "csh_validation"][["IPA"]]
  cbind.data.frame("cindex" = cindex_boot, "ipa" = ipa_boot)
})

df_boots <- do.call(rbind.data.frame, boots_ls)
hist(df_boots$cindex, main = "Bootstrapped C-index", xlab = "C-index")
hist(df_boots$ipa, main = "Bootstrapped IPA", xlab = "IPA")

# Summarize C-index
c("cindex_5y" = cindex_csh, quantile(df_boots$cindex, probs = c(0.025, 0.975)))

# Summarize IPA
c(
  "ipa_5y" = score_vdata$Brier$score[model == "csh_validation"][["IPA"]],
  quantile(df_boots$ipa, probs = c(0.025, 0.975))
)
