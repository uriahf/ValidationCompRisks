# One Model One Population

library(magrittr)
library(dplyr)

one_model_one_pop <- create_nested_data_frame_with_probs_distributions(
  probs = list("train" = as.vector(rdata$pred5)),
  reals = list("train" = rdata$status_num),
  times = rdata$time,
  fixed_horizon_times = c(1, 3, 4, 5),
  by = 0.1
)

one_model_one_pop$train$probability_threshold$probs_distribution |> View()

View(one_model_one_pop)
# |>
#   extract_all_time_horizons_confusion_matrix_for_display_data()

reference_group <- "train"

probs_histogram_list <- create_probs_histogram_list(
  reference_groups = "train",
  one_model_one_pop
)

waldo::compare(
  names(one_model_one_pop$train$probability_threshold),
  names(one_model_one_pop$train$ppcr)
)

render_confusion_matrix_for_probs_histogram(
  one_model_one_pop$train$probability_threshold,
  "bla"
)

one_model_one_pop$train$probability_threshold |>
  View()

one_model_one_pop <- create_nested_data_frame_with_probs_distributions(
  probs = list("train" = as.vector(rdata$pred5)),
  reals = list("train" = rdata$status_num),
  times = rdata$time,
  fixed_horizon_times = c(1, 3, 5),
  by = 0.01
)


one_model_one_pop$train$probability_threshold$probs_distribution[[1]] |>
  create_probs_histogram_out_of_probs_distribution(
    stratified_by = "probability_threshold",
    population_name = "train",
    by = 0.01
  )

library(magrittr)


one_model_one_pop %$%
  train %$%
  probability_threshold %$%
  probs_reals_times %$%
  .[[1]] |>
  select(probs)

one_model_one_pop |>
  purrr::map(
    function(x)
      purrr::map(
        x,
        function(y)
          y$probs_reals_times |>
            purrr::map(
              function(z) z$probs
            )
      )
  )


# ppcr

rtichoke:::extend_axis_ranges(c(0, 1))

# probability_threshold

rtichoke:::extend_axis_ranges(
  one_model_one_pop |>
    purrr::map(
      \(x)
        x$probability_threshold$probs_reals_times |>
          purrr::map(
            \(y) y$probs
          )
    ) |>
    purrr::list_flatten() |>
    unlist() |>
    range()
)


extract_ranges_for_probs_histogram <- function(
  nested_data_frame_with_probs_distributions,
  stratified_by,
  ...
) {
  if (stratified_by == "ppcr") {
    original_range <- c(0, 1)
  } else if (stratified_by == "probability_threshold") {
    original_range <- nested_data_frame_with_probs_distributions |>
      purrr::map(
        \(x)
          x$probability_threshold$probs_reals_times |>
            purrr::map(
              \(y) y$probs
            )
      ) |>
      purrr::list_flatten() |>
      unlist() |>
      range()
  }

  rtichoke:::extend_axis_ranges(
    original_range
  )
}

extract_ranges_for_probs_histogram(
  one_model_one_pop,
  "probability_threshold"
)

extract_ranges_for_probs_histogram(
  one_model_one_pop,
  "ppcr"
)

bla <- run_function_with_stratified_by_values(
  one_model_one_pop,
  func = extract_ranges_for_probs_histogram,
  by = 0.01
)


probs_distribution |>
  View()

one_model_one_pop$train$probability_threshold %$%
  performance_data %>%
  .[[3]] |>
  View()


# HERE 030724
one_model_one_pop$train$ppcr %$%
  confusion_matrix_for_display_data %>%
  .[[3]] |>
  select(type) |>
  distinct() |>
  View()

render_confusion_matrix_for_probs_histogram(
  "bla"
)

one_model_one_pop$train$probability_threshold %$%
  confusion_matrix_for_display_data %>%
  .[[3]] |>
  select(type) |>
  distinct() |>
  View()
render_confusion_matrix_for_probs_histogram(
  "bla"
)


one_model_one_pop$train$probability_threshold %$%
  confusion_matrix_for_display_data %>%
  .[[3]] |>
  filter(
    type %in%
      c(
        "real_positives",
        "real_negatives",
        "total_predicted",
        "real_competing",
        "total_included",
        "real_censored"
      )
  ) |>
  mutate(
    predicted_positives = case_when(
      type == "real_censored" ~ 200,
      TRUE ~ predicted_positives
    ),
    predicted_negatives = case_when(
      type == "real_censored" ~ 200,
      TRUE ~ predicted_negatives
    ),
    total_reals = case_when(
      type == "real_censored" ~ 200,
      TRUE ~ total_reals
    )
  ) |>
  # View()
  render_confusion_matrix_for_probs_histogram(
    "bla"
  )


render_confusion_matrix_for_probs_histogram(
  "bla"
)

?create_confusion_matrix_for_display_data

probs_histogram_list$train$ppcr
probs_histogram_list$train$probability_threshold

View(one_model_one_pop$train$probability_threshold)

all_time_horizons_confusion_matrix_for_display_data <- extract_all_time_horizons_confusion_matrix_for_display_data(
  one_model_one_pop
)


all_time_horizons_confusion_matrix_for_display_data

create_stratified_by_sublist(
  "probability_threshold",
  "train",
  all_time_horizons_confusion_matrix_for_display_data$train$probability_threshold
)


one_model_one_pop$train |>
  purrr::map(
    \(stratified_by_data) {
      stratified_by_data |>
        dplyr::select(time_horizon, confusion_matrix_for_display_data) |>
        tidyr::unnest(confusion_matrix_for_display_data)
    }
  )


# Several Models One Population

several_models_one_pop <- create_nested_data_frame_with_probs_distributions(
  probs = list(
    "Full Model" = as.vector(rdata$pred5),
    "Age Model" = as.vector(rdata$pred_age)
  ),
  reals = list("train" = rdata$status_num),
  times = rdata$time,
  fixed_horizon_times = c(1, 3, 5),
  by = 0.1
)

# one_pop_one_model
one_model_one_pop$train$probability_threshold$probs_distribution[[1]]


one_model_one_pop$train$probability_threshold$probs_distribution

several_models_one_pop$`Full Model`$ppcr$probs_distribution


bla <- run_function_with_stratified_by_values(
  several_models_one_pop,
  func = extract_ranges_for_probs_histogram,
  by = 0.01
)


# TODO: fix run_function_over_probs_reals_lists

nested_data <- run_function_over_probs_reals_lists(
  probs = list(
    "Full Model" = as.vector(rdata$pred5),
    "Age Model" = as.vector(rdata$pred_age)
  ),
  reals = list("train" = rdata$status_num),
  func = turn_reals_codes_to_reals_labels,
  times = rdata$time,
  fixed_horizon_times = c(1, 3, 5)
)

nested_data$`Full Model` |>
  tidyr::unnest() |>
  dplyr::pull(reals_codes) |>
  unique()
View()


turn_reals_codes_to_reals_labels(
  probs_vec = as.vector(rdata$pred5),
  reals_vec = rdata$status_num,
  times = rdata$time,
  fixed_horizon_times = c(1, 3, 5)
) |>
  tidyr::unnest() |>
  View()


several_models_one_pop$`Full Model`$probability_threshold$probs_reals_times[[
  2
]]$reals_codes |>
  unique()

several_models_one_pop$`Full Model`$probability_threshold$probs_distribution[[
  3
]] |>
  View()

several_models_one_pop$`Full Model`$probability_threshold$performance_data[[
  3
]] |>
  View()

several_models_one_pop$`Full Model`$probability_threshold$confusion_matrix_for_display_data[[
  1
]] |>
  filter(type == "real_competing") |>
  View()

all_time_horizons_confusion_matrix_for_display_data <- extract_all_time_horizons_confusion_matrix_for_display_data(
  several_models_one_pop
)

View(
  all_time_horizons_confusion_matrix_for_display_data$`Full Model`$probability_threshold |>
    filter(type == "real_competing")
)


# Several Models Several Populations

nested_data <- create_nested_data_frame_with_probs_distributions(
  probs = list(
    "Train" = as.vector(rdata$pred5),
    "Test" = as.vector(rdata$pred_age)
  ),
  reals = list(
    "Train" = rdata$status_num,
    "Test" = rdata$status_num
  ),
  times = rdata$time,
  fixed_horizon_times = c(1, 3, 5),
  by = 0.1
)

# TODO: create a function that adds a new column: "initial zoom"

bla <- run_function_with_stratified_by_values(
  nested_data,
  func = extract_ranges_for_probs_histogram,
  by = 0.01
)


run_function_with_stratified_by_values(
  nested_data,
  func = extract_ranges_for_probs_histogram,
  by = 0.01
)


nested_data |>
  purrr::map(
    add_initial_zoom_to_nested_data,
    run_function_with_stratified_by_values(
      nested_data,
      func = extract_ranges_for_probs_histogram,
      by = 0.01
    )
  )


add_initial_zoom_to_nested_data <- function(
  nested_data,
  ranges_for_probs_histogram
) {
  purrr::map2(
    nested_data,
    ranges_for_probs_histogram,
    \(x, y)
      x |>
        dplyr::mutate(
          initial_zoom = list(y)
        )
  )
}


# TODO: turn this to map

add_initial_zoom_to_nested_data(
  nested_data[[1]],
  bla
)

add_initial_zoom_to_nested_data(
  nested_data[[2]],
  bla
)

nested_data |>
  purrr::map(
    add_initial_zoom_to_nested_data,
    bla
  )


#

purrr::map2(
  nested_data[[1]],
  bla,
  \(x, y)
    x |>
      dplyr::mutate(
        initial_zoom = list(y)
      )
)


nested_data[[1]]$ppcr |>
  dplyr::mutate(
    initial_zoom = list(bla$ppcr)
  ) |>
  View()

nested_data[[1]]$probability_threshold |>
  dplyr::mutate(
    initial_zoom = list(
      bla$probability_threshold
    )
  )


nested_data[[1]]$ppcr |>
  View()


nested_data$`Train`$probability_threshold$probs_reals_times[[3]] |>
  # tidyr::unnest() |>
  View()

####

create_probs_histogram_report_times(
  probs = list(
    "Full Model" = as.vector(rdata$pred5),
    "Age Model" = as.vector(rdata$pred_age)
  ),
  reals = list("Train" = as.vector(rdata$status_num)),
  times = list("Train" = rdata$time)
)

create_probs_histogram_report_times(
  probs = list(
    "Train" = as.vector(rdata$pred5),
    "Test" = as.vector(rdata$pred_age)
  ),
  reals = list(
    "Train" = rdata$status_num,
    "Test" = rdata$status_num
  ),
  times = rdata$time,
  fixed_horizon_times = c(1, 3, 5),
  by = 0.01
)


reals_labels_by_time_horizon <- turn_reals_codes_to_reals_labels(
  probs = as.vector(rdata$pred5),
  reals = rdata$status_num,
  times = rdata$time,
  fixed_horizon_times = c(0, 1, 3, 5)
) |>
  tidyr::nest(.by = time_horizon)
