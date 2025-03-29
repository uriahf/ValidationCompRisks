# create_decision_report_times <- function(
#     probs,
#     reals,
#     # times,
#     # fixed_horizon_times,
#     # by,
#     output_file = "decision_report_times.html",
#     output_dir = getwd()
# ){
#
#   rmarkdown::render(
#     "inst/decision_report_times_template.Rmd",
#     params = list(
#       probs = probs,
#       reals = reals,
#       times = NA,#times,
#       fixed_horizon_times = NA,#fixed_horizon_times,
#       by = 0.01#by
#     ),
#     output_file = output_file,
#     output_dir = output_dir
#   )
#
#   print(glue::glue("{output_file} was rendered in {output_dir}"))
#
# }
#
# create_decision_report_times(
#   probs = list(example_dat$estimated_probabilities),
#   reals = list(example_dat$outcome)
# )

library(rtichoke)

performance_data_for_decision_curve <- prepare_performance_data(
  probs = list("train" = example_dat$estimated_probabilities),
  reals = list("train" = example_dat$outcome)
)

library(dplyr)

nested_data_frame_with_probs_distributions <- create_nested_data_frame_with_probs_distributions(
  probs = list("train" = example_dat$estimated_probabilities),
  reals = list("train" = as.numeric(example_dat$outcome)),
  times = rep(1.5, 150),
  fixed_horizon_times = c(0, 1, 3, 5),
  by = 0.01
)


all_time_horizons_confusion_matrix_for_display_data <- extract_all_time_horizons_confusion_matrix_for_display_data(
  nested_data_frame_with_probs_distributions
)


cmdisplay <- gsub(
  "[,\\s]+",
  "_",
  paste(
    "confusion_matrix_for_display",
    "probability_threshold",
    "train",
    sep = "_"
  ),
  perl = TRUE
)

library(dplyr)
confusion_matrix_for_display <- htmltools::tagList(
  all_time_horizons_confusion_matrix_for_display_data$train$probability_threshold |>
    render_confusion_matrix_for_probs_histogram(
      elementID = cmdisplay
    )
)


decision_curve_rtichoke_list <- performance_data_for_decision_curve |>
  rtichoke:::create_rtichoke_curve_list(
    "decision",
    # size = size,
    # color_values = color_values,
    min_p_threshold = 0,
    max_p_threshold = 1
  )

decision_curve_rtichoke_list$reference_data <- decision_curve_rtichoke_list$reference_data |>
  dplyr::mutate(
    reference_group = dplyr::case_when(
      reference_group == "reference_line_treat_all" ~
        "reference_line_treat_all",
      reference_group == "reference_line" ~ "reference_line_treat_none"
    )
  )

decision_curve_rtichoke_list_avoided <- performance_data_for_decision_curve |>
  rtichoke:::create_rtichoke_curve_list(
    "interventions avoided",
    # size = size,
    # color_values = color_values,
    min_p_threshold = 0,
    max_p_threshold = 1
  )


decision_curve_rtichoke_list_avoided$reference_data <- decision_curve_rtichoke_list_avoided$reference_data |>
  dplyr::mutate(
    reference_group = dplyr::case_when(
      reference_group == "reference_line_treat_none" ~
        "reference_line_treat_none",
      reference_group == "reference_line" ~ "reference_line_treat_all"
    )
  )

unique(decision_curve_rtichoke_list_avoided$reference_data$reference_group)
unique(decision_curve_rtichoke_list$reference_data$reference_group)


confusion_matrix_input_id <- gsub(
  "[,\\s]+",
  "_",
  paste("confusionMatrixInput", "probability_threshold", "train", sep = "_"),
  perl = TRUE
)

confusion_matrix_input <- create_confusion_matrix_as_input_element(
  confusion_matrix_input_id,
  cmdisplay,
  real_competing = TRUE,
  real_censored = TRUE
)

filteredperformancetableId <- gsub(
  "[,\\s]+",
  "_",
  paste(
    "filtered_performance_table",
    "probability_threshold",
    "train",
    sep = "_"
  ),
  perl = TRUE
)


censored_assumption_radiobutton_input_id <- gsub(
  "[,\\s]+",
  "_",
  paste(
    "censored_assumption_radiobutton",
    "probability_threshold",
    "train",
    sep = "_"
  ),
  perl = TRUE
)

competing_assumption_radiobutton_input_id <- gsub(
  "[,\\s]+",
  "_",
  paste(
    "competing_assumption_radiobutton",
    "probability_threshold",
    "train",
    sep = "_"
  ),
  perl = TRUE
)

baseline_treatment_strategy_radiobutton_input_id <- gsub(
  "[,\\s]+",
  "_",
  paste(
    "baseline_treatment_strategy_radiobutton",
    "probability_threshold",
    "train",
    sep = "_"
  ),
  perl = TRUE
)


create_radio_buttons_group(
  "Competing Events",
  competing_assumption_radiobutton_input_id,
  list(
    values = c("excluded", "non_event"),
    labels = c("Excluded", "As Non Event")
  ),
)


create_radio_buttons_group <- function(
  title,
  radio_buttons_group_id,
  radio_buttons_names_and_labels,
  onclickfunction
) {
  script <- "function ManipulateConfusionMatrixInputTables(value) {
  
    //console.log('value')
    //console.log(value)
    
    //const displayValue = parseFloat(value.value).toFixed(2);
    //document.getElementById('filter_confusion_matrix_for_display_probability_threshold_train_probability_threshold__value').textContent = displayValue;    
    
  }"

  htmltools::div(
    htmltools::HTML(paste0("<b>", title, ":</b>")),
    htmltools::tags$label(
      `for` = radio_buttons_group_id,
      htmltools::tags$input(
        id = paste0(
          radio_buttons_group_id,
          radio_buttons_names_and_labels$labels[1],
          sep = "_"
        ),
        name = radio_buttons_group_id,
        value = radio_buttons_names_and_labels$values[1],
        checked = TRUE,
        type = "radio" #,
        # onclick = "ManipulateConfusionMatrixInputTables(this)" #onClickFunction
      ),
      radio_buttons_names_and_labels$labels[1]
    ),
    htmltools::tags$label(
      `for` = radio_buttons_group_id,
      htmltools::tags$input(
        id = paste0(
          radio_buttons_group_id,
          radio_buttons_names_and_labels$labels[2],
          sep = "_"
        ),
        name = radio_buttons_group_id,
        value = radio_buttons_names_and_labels$values[2],
        type = "radio" #,
        # onclick = "ManipulateConfusionMatrixInputTables(this)"
      ),
      radio_buttons_names_and_labels$labels[2]
    ),
    htmltools::tags$script(
      htmltools::HTML(script)
    )
  )
}


rangeFilterDecision <- function(
  tableId,
  tableId2,
  columnId,
  label,
  min,
  max,
  value = NULL,
  step = NULL,
  width = "200px"
) {
  value <- if (!is.null(value)) value else min
  inputId <- sprintf("filter_%s_%s", tableId, columnId)
  valueId <- sprintf("filter_%s_%s__value", tableId, columnId)
  oninput <- paste(
    sprintf("document.getElementById('%s').textContent = this.value;", valueId)
  )

  script <- "function updateSliderValue(value) {
  
    //console.log('value')
    //console.log(value)
    
    const displayValue = parseFloat(value.value).toFixed(2);
    document.getElementById('filter_confusion_matrix_for_display_probability_threshold_train_probability_threshold__value').textContent = displayValue;    
    
  }"

  # TODO: fix id in span

  htmltools::div(
    htmltools::tags$label(`for` = inputId, label),
    htmltools::div(
      style = sprintf(
        "display: flex; align-items: center; width: %s",
        htmltools::validateCssUnit(width)
      ),
      htmltools::tags$input(
        id = inputId,
        type = "range",
        min = min,
        max = max,
        step = step,
        value = value,
        # oninput = oninput,
        # onchange = oninput, # For IE11 support
        oninput = "updateSliderValue(this);",
        onchange = "updateSliderValue(this);",
        style = "width: 100%;"
      ),
      htmltools::span(id = valueId, style = "margin-left: 8px;", value)
    ),
    htmltools::tags$script(
      htmltools::HTML(script)
    )
  )
}

filteredperformancetableId <- gsub(
  "[,\\s]+",
  "_",
  paste(
    "filtered_performance_table",
    "probability_threshold",
    "train",
    sep = "_"
  ),
  perl = TRUE
)

# Run Here

decision_curve_d3 <- r2d3::r2d3(
  # data = list(a = performance_data_for_decision_curve),
  data = data_to_json(
    list(
      "treat_none" = decision_curve_rtichoke_list,
      "treat_all" = decision_curve_rtichoke_list_avoided
    )
    # decision_curve_rtichoke_list
  ),
  script = "inst/decision_curve.js",
  width = 350,
  height = 350,
  container = 'div',
  # elementId = histogram_predicted_id,
  options = list(
    listenTO = "filter_confusion_matrix_for_display_probability_threshold_train_probability_threshold",
    #     outerDiv = div_histogram_predicted_id,
    #     confusionMatrixInputID = confusion_matrix_input_id,
    #     by = by,
    # initial_zoom = decision_curve_rtichoke_list$axes_ranges,
    initial_zoom = list(
      "treat_none" = decision_curve_rtichoke_list$axes_ranges,
      "treat_all" = decision_curve_rtichoke_list_avoided$axes_ranges
    ),
    axes_labels = decision_curve_rtichoke_list_avoided$axes_labels
    # axes_labels = decision_curve_rtichoke_list$axes_labels
  )
)


crosstalk::bscols(
  widths = c(12, 12, 12, 12, 12, 6, 6),
  create_radio_buttons_group(
    "Competing Events",
    competing_assumption_radiobutton_input_id,
    list(
      values = c("excluded", "non_event"),
      labels = c("Excluded", "As Non Event")
    )
  ),
  create_radio_buttons_group(
    "Censored Events",
    censored_assumption_radiobutton_input_id,
    list(
      values = c("excluded", "adjusted"),
      labels = c("Excluded", "Adjusted")
    )
  ),
  create_radio_buttons_group(
    "Baseline Strategy",
    baseline_treatment_strategy_radiobutton_input_id,
    list(
      values = c("treat_none", "treat_all"),
      labels = c("Treat None", "Treat All")
    )
  ),
  time_horizon_slider(
    cmdisplay,
    filteredperformancetableId,
    "time_horizon",
    "Time Horizon",
    0,
    5,
    step = 1
  ),
  rangeFilterDecision(
    cmdisplay,
    filteredperformancetableId,
    "probability_threshold",
    "Probability Threshold: ",
    0,
    1,
    step = 0.01,
    value = 0.15
  ),
  htmltools::div(
    decision_curve_d3,
    id = "decision_curve_div"
  ) #,
  # htmltools::div(
  # confusion_matrix_input,
  # confusion_matrix_for_display)
)

# TODO: sort xaxis + yaxis labels
