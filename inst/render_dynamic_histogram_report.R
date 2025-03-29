render_dynamic_histogram_report <- function(
  probs,
  reals,
  times,
  fixed_horizon_times,
  by,
  stratified_by = c("ppcr", "probability_threshold"),
  fixed_probability_threshold = NULL,
  output_file = "probs_dynamic_histogram_report.html",
  output_dir = getwd()
) {
  # View(probs)

  rmarkdown::render(
    "inst/probs_dynamic_histogram_report_template.Rmd",
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


render_dynamic_histogram_report(
  probs = list(
    "random_guess" = as.vector(sample(
      as.vector(rdata$pred5)
    ))
  ),
  reals = list("random_guess" = rdata$status_num),
  times = rdata$time,
  fixed_horizon_times = c(1, 2, 3, 4, 5),
  by = 0.01,
  stratified_by = c("probability_threshold", "ppcr")
)


render_dynamic_histogram_report(
  probs = list(
    "full_model" = as.vector(rdata$pred5),
    "age" = as.vector(rdata$pred5_age)
  ),
  reals = list("train" = rdata$status_num),
  times = rdata$time,
  fixed_horizon_times = c(1, 2, 3, 4, 5),
  by = 0.01,
  stratified_by = c("probability_threshold", "ppcr")
)
