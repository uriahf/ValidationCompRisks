library(rtichoke)
library(dplyr)

nested_data_frame_with_probs_distributions <- create_nested_data_frame_with_probs_distributions(
  probs = list("train" = example_dat$estimated_probabilities),
  reals = list("train" = as.numeric(example_dat$outcome)),
  times = rep(1.5, 150),
  fixed_horizon_times = c(0, 1, 3, 5),
  by = 0.01
)


nested_data_frame_with_probs_distributions$train$probability_threshold$probs_reals_times |>
  View()


nested_data_frame_with_probs_distributions$train$probability_threshold$probs_distribution[[
  2
]]$real_negatives |>
  View()

nested_data_frame_with_probs_distributions$train$probability_threshold |>
  names()

# Time Horizon 1

performance_data_time_horizon_1 <- nested_data_frame_with_probs_distributions$train$probability_threshold |>
  filter(time_horizon == 1) |>
  select(performance_data) |>
  pull(performance_data)


decision_curve_rtichoke_list_th_1_baseline_treat_none <- performance_data_time_horizon_1 |>
  rtichoke:::create_rtichoke_curve_list(
    "decision",
    min_p_threshold = 0,
    max_p_threshold = 1
  )
