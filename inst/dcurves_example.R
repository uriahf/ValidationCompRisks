library(dca)
library(survival)
library(dcurves)
library(gtsummary); library(dplyr); library(tidyr)

cox_model <- coxph(
  Surv(ttcancer, cancer) ~ 
    age + famhistory + marker, data = df_surv)


pred_1 <- broom::augment(
  cox_model, 
  newdata = df_surv %>% mutate(ttcancer =  1),
  type.predict = "expected"
) |> 
  mutate(
    pr_failure18 = 1 - exp(-.fitted)
  ) |>
  pull(pr_failure18)

pred_1_5 <- broom::augment(
  cox_model, 
  newdata = df_surv %>% mutate(ttcancer =  1.5),
  type.predict = "expected"
) |> 
  mutate(
    pr_failure18 = 1 - exp(-.fitted)
  ) |>
  pull(pr_failure18)

pred_3 <- broom::augment(
  cox_model, 
  newdata = df_surv %>% mutate(ttcancer =  3),
  type.predict = "expected"
) |> 
  mutate(
    pr_failure18 = 1 - exp(-.fitted)
  ) |>
  pull(pr_failure18)

View(pred_3)

cmdisplay <- gsub(
  "[,\\s]+", "_", paste(
    "confusion_matrix_for_display", 
    "probability_threshold", "train", 
    sep = "_"), perl = TRUE)


extract_decision_curve_rtichoke_lists <- function(
    nested_data_frame_with_probs_distributions) {
  
  nested_data_frame_with_probs_distributions |>
    purrr::map("probability_threshold") |>
    purrr::map_df(
      \(x) x |> 
        dplyr::select(time_horizon, performance_data)
    ) |> 
    dplyr::mutate(
      "treat_none_baseline_strategy" = purrr::map(
        performance_data,
        \(x) {
          rtichoke:::create_rtichoke_curve_list(
            x, "decision")
        }
      ),
      "treat_all_baseline_strategy" = purrr::map(
        performance_data,
        \(x) {
          rtichoke:::create_rtichoke_curve_list(
            x, "interventions avoided")
        }
      )
    )
  
}

# Something is wrong with reference lines

prop.table(table(as.numeric(df_surv$cancer)))

nested_data_frame_with_probs <- create_nested_data_frame_with_probs_distributions(
  probs = list("train" = pred_1_5),
  reals = list("train" = df_surv$cancer),
  times = df_surv$ttcancer,
  fixed_horizon_times = c(0, 1.5, 3),
  by = 0.01
) 

nested_data_frame_with_probs$train$probability_threshold |> 
  View()

nested_data_frame_with_probs$train$probability_threshold$performance_data[[3]] %>% 
  dplyr::filter(ppcr == 1) |> 
  View()
  
  
  # dplyr::select(dplyr::any_of(c("model", "population", 
                                 
nested_data_frame_with_probs$train$probability_threshold |> 
  View()
                                                                                                       "PPV"))) %>% distinct() %>% dplyr::pull(PPV, name = 1)
# prevalence |> 
#   View()

decision_curve_rtichoke_lists <- nested_data_frame_with_probs |> 
  extract_decision_curve_rtichoke_lists()

# DONE: create decision curve lists out of nested data frames

decision_curve_rtichoke_lists$treat_none_baseline_strategy[[3]]$reference_data |> 
  View()

decision_curve_rtichoke_lists$treat_none_baseline_strategy[[3]]$reference_data |> 
  View()


confusion_matrix_input_id <-  gsub(
  "[,\\s]+", "_", paste("confusionMatrixInput",
                        "probability_threshold",
                        "train",
                        sep = "_"
  ), perl = TRUE)

confusion_matrix_input <- create_confusion_matrix_as_input_element(
  confusion_matrix_input_id, cmdisplay, 
  real_competing = TRUE,
  real_censored = TRUE
)

filteredperformancetableId <- gsub(
  "[,\\s]+", "_", paste(
    "filtered_performance_table", 
    "probability_threshold", "train", 
    sep = "_"), perl = TRUE)


censored_assumption_radiobutton_input_id <-  gsub(
  "[,\\s]+", "_", paste("censored_assumption_radiobutton",
                        "probability_threshold",
                        "train",
                        sep = "_"
  ), perl = TRUE)

competing_assumption_radiobutton_input_id <-  gsub(
  "[,\\s]+", "_", paste("competing_assumption_radiobutton",
                        "probability_threshold",
                        "train",
                        sep = "_"
  ), perl = TRUE)

baseline_treatment_strategy_radiobutton_input_id <-  gsub(
  "[,\\s]+", "_", paste("baseline_treatment_strategy_radiobutton",
                        "probability_threshold",
                        "train",
                        sep = "_"
  ), perl = TRUE)



create_radio_buttons_group <- function(
    title, radio_buttons_group_id, 
    radio_buttons_names_and_labels,
    onclickfunction) {
  
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
          sep = "_"),
        name = radio_buttons_group_id,
        value = radio_buttons_names_and_labels$values[1],
        checked = TRUE,
        type = "radio"#,
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
          sep = "_"),
        name = radio_buttons_group_id,
        value = radio_buttons_names_and_labels$values[2],
        type = "radio"#,
        # onclick = "ManipulateConfusionMatrixInputTables(this)"
      ),
      radio_buttons_names_and_labels$labels[2]
    ),
    htmltools::tags$script(
      htmltools::HTML(script)))
  
}



rangeFilterDecision <- function(
    tableId, tableId2, columnId, label, min, max, value = NULL, step = NULL, width = "200px") {
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
      style = sprintf("display: flex; align-items: center; width: %s", htmltools::validateCssUnit(width)),
      htmltools::tags$input(
        id = inputId,
        type = "range",
        min = min,
        max = max,
        step = step,
        value = value,
        # oninput = oninput,
        # onchange = oninput, # For IE11 support
        oninput="updateSliderValue(this);",
        onchange="updateSliderValue(this);",
        style = "width: 100%;"
      ),
      htmltools::span(id = valueId, style = "margin-left: 8px;", value)
    ),
    htmltools::tags$script(
      htmltools::HTML(script))
  )
}

filteredperformancetableId <- gsub(
  "[,\\s]+", "_", paste(
    "filtered_performance_table", 
    "probability_threshold", "train", 
    sep = "_"), perl = TRUE)

data_to_json <- function(data){
  jsonlite::toJSON(
    data, dataframe = "rows"
  )
}

decision_curve_rtichoke_lists$treat_none_baseline_strategy[[3]] |> 
  rtichoke:::create_plotly_curve()

decision_curve_rtichoke_lists$treat_all_baseline_strategy[[3]] |> 
  rtichoke:::create_plotly_curve()

# Run Here



decision_curve_d3 <-  r2d3::r2d3(
  # data = list(a = performance_data_for_decision_curve),
  # data = data_to_json(
  #   list(
  #     "treat_none" = decision_curve_rtichoke_list, 
  #     "treat_all" = decision_curve_rtichoke_list_avoided
  #   )
  #   # decision_curve_rtichoke_list
  # ),
  data = data_to_json(
    list(
      "treat_none" = decision_curve_rtichoke_lists$treat_none_baseline_strategy[[3]],
      "treat_all" = decision_curve_rtichoke_lists$treat_all_baseline_strategy[[3]]
    )
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
      "treat_none" = decision_curve_rtichoke_lists$treat_none_baseline_strategy[[3]]$axes_ranges,
      "treat_all" = decision_curve_rtichoke_lists$treat_all_baseline_strategy[[1]]$axes_ranges),
    axes_labels = decision_curve_rtichoke_lists$treat_none_baseline_strategy[[1]]$axes_labels    
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
    id = "decision_curve_div")#,
  # htmltools::div(
  # confusion_matrix_input,
  # confusion_matrix_for_display)
)


# TODO: sort xaxis + yaxis labels
