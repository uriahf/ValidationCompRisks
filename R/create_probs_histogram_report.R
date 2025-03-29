rdata <- readRDS("Data/rdata.rds")
library(riskRegression)


# Models ------------------------------
fit_csh <- CSC(
  Hist(time, status_num) ~ age + size + ncat + hr_status,
  data = rdata,
  fitter = "cph"
)

fit_csh_2 <- CSC(Hist(time, status_num) ~ age, data = rdata, fitter = "cph")

# useful objects
primary_event <- 1 # Set to 2 if cause 2 was of interest
horizon <- 5 # Set time horizon for prediction (here 5 years)

# Development data
# calculation estimated risk
rdata$pred5 <- predictRisk(
  fit_csh,
  cause = primary_event,
  newdata = rdata,
  times = horizon
)

rdata$pred_age <- predictRisk(
  fit_csh_2,
  cause = primary_event,
  newdata = rdata,
  times = horizon
)


create_probs_histogram_report <- function(
  probs,
  reals,
  by = 0.01,
  fixed_probability_threshold = NULL,
  interactive = TRUE,
  output_file = "probs_histogram_report.html",
  output_dir = getwd(),
  stratified_by = c("ppcr", "probability_threshold")
) {
  rmarkdown::render(
    "inst/probs_histogram_report_template.Rmd",
    params = list(
      probs = probs,
      reals = reals,
      by = by,
      stratified_by = stratified_by,
      fixed_probability_threshold = fixed_probability_threshold,
      interactive = interactive
    ),
    output_file = output_file,
    output_dir = output_dir
  )

  print(glue::glue("{output_file} was rendered in {output_dir}"))
}


create_probs_histogram_report(
  probs = list(
    "Full Model" = as.vector(rdata$pred5),
    "Age Model" = as.vector(rdata$pred_age),
    "Train" = as.vector(rdata$pred5)
  ),
  reals = list("Train" = as.vector(rdata$status_num)),
  stratified_by = "ppcr"
)


create_probs_histogram_report_times <- function(
  probs,
  reals,
  times,
  fixed_horizon_times,
  by,
  stratified_by = c("ppcr", "probability_threshold"),
  fixed_probability_threshold = NULL,
  output_file = "probs_histogram_report_times.html",
  output_dir = getwd()
) {
  rmarkdown::render(
    "inst/probs_histogram_report_template_times.Rmd",
    params = list(
      probs = probs,
      reals = reals,
      times = times,
      fixed_horizon_times = fixed_horizon_times,
      by = by,
      stratified_by = stratified_by,
      fixed_probability_threshold = NULL
    ),
    output_file = output_file,
    output_dir = output_dir
  )

  print(glue::glue("{output_file} was rendered in {output_dir}"))
}


# create_probs_histogram_report(
#   probs = list("Train" = as.vector(rdata$pred5)),
#   reals = list("Train" = as.vector(rdata$status_num))
# )
#
#
#
#
# create_probs_histogram_report_times(
#   probs = list(
#     "Nice Model" = as.vector(rdata$pred5),
#     "Other Model" = as.vector(rdata$pred_age)),
#   reals = list("Train" = as.vector(rdata$status_num)),
#   times = rdata$time,
#   fixed_horizon_times = c(1, 3, 5, 10),
#   by = 0.01
# )
#
# nested_data_frame_with_probs_distributions <- create_nested_data_frame_with_probs_distributions(
#   probs = list("Train" = as.vector(rdata$pred5)),
#   reals = list("Train" = as.vector(rdata$status_num)),
#   times = list("Train" = rdata$time),
#   fixed_horizon_times = c(1, 3, 5),
#   by = 0.01
# )

create_probs_histogram_report_times(
  probs = list("train" = as.vector(rdata$pred5)),
  reals = list("train" = rdata$status_num),
  times = rdata$time,
  fixed_horizon_times = c(1, 2),
  by = 0.1,
  stratified_by = "probability_threshold"
)


create_probs_histogram_report_times(
  probs = list(
    "train" = as.vector(rdata$pred5),
    "test" = as.vector(rdata$pred5)
  ),
  reals = list(
    "train" = rdata$status_num,
    "test" = rdata$status_num
  ),
  times = rdata$time,
  fixed_horizon_times = c(1, 3, 5),
  by = 0.1
)

# put more censored obs

# TODO: why fixed_horizon_times doesn't render 1, 2, 3

create_probs_histogram_report_times(
  probs = list(
    "train" = as.vector(rdata$pred5),
    "test" = as.vector(rdata$pred5)
  ),
  reals = list(
    "train" = rdata$status_num,
    "test" = rdata$status_num
  ),
  times = rdata$time / 2,
  fixed_horizon_times = c(0, 1, 2, 3),
  by = 0.01
)
