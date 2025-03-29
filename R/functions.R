# DONE: Tab for each combination of ppcr/p.threshold, model/population
# DONE: confusion matrix-input for each combination of ppcr/p.threshold, model/population
# DONE: manipulate confusion matrix-input by assumptions

# TODO1: nest reference_group and stratified_by
# TODO2: Add customized range slider with specific values allowed

data_to_json <- function(data) {
  jsonlite::toJSON(
    data,
    dataframe = "rows",
    auto_unbox = FALSE,
    rownames = TRUE
  )
}

create_stratified_by_radio_buttons <- function() {
  htmltools::div(
    class = "radio-group",
    htmltools::HTML("<b>Stratified Predictions By:</b>"),
    htmltools::tags$label(
      `for` = "probability_threshold",
      htmltools::tags$input(
        id = "probability_threshold",
        name = "stratified_by",
        value = "probability_threshold",
        type = "radio",
        checked = TRUE,
        onclick = "ManipulateStratifiedBy(this)"
      ),
      "Probability Threshold"
    ),
    htmltools::tags$label(
      `for` = "ppcr",
      htmltools::tags$input(
        id = "ppcr",
        name = "stratified_by",
        value = "ppcr",
        type = "radio",
        onclick = "ManipulateStratifiedBy(this)"
      ),
      "PPCR"
    )
  )
}

# TODO: Find better way handling radio buttons
# TODO: Manipulate confusion matrix for display and histogram according to assumptions

create_radio_button <- function(
  value,
  radio_name,
  onclick_handler,
  is_checked = FALSE
) {
  htmltools::tags$input(
    id = paste0(radio_name, value),
    name = radio_name,
    value = value,
    type = "radio",
    checked = is_checked,
    onclick = onclick_handler
  )
}

create_radio_label <- function(config, radio_name, onclick_handler) {
  htmltools::tags$label(
    `for` = radio_name,
    create_radio_button(
      config$value,
      radio_name,
      onclick_handler,
      config$checked
    ),
    config$label
  )
}


create_radio_buttons <- function(
  radio_configs,
  title,
  radio_name,
  onclick_handler
) {
  title_element <- htmltools::HTML(paste0("<b>", title, "</b>"))
  radio_elements <- purrr::map(radio_configs, \(radio_config) {
    create_radio_label(
      radio_config,
      radio_name,
      onclick_handler
    )
  })

  htmltools::div(
    title_element,
    !!!radio_elements
  )
}

create_reference_groups_radio_buttons <- function(
  reference_groups
) {
  create_radio_buttons(
    reference_groups |>
      purrr::map(\(x) {
        list(value = x, label = x, checked = FALSE)
      }),
    "Reference Group:",
    "reference_group_radiobutton_",
    "ManipulateReferenceGroup(this)" # write this function
  )
}


create_competing_assumptions_radio_buttons <- function() {
  create_radio_buttons(
    list(
      list(value = "excluded", label = "Excluded", checked = TRUE),
      list(value = "non_event", label = "As Non Event", checked = FALSE)
    ),
    "Competing Events:",
    "competing_assumption_radiobutton_",
    "ManipulateConfusionMatrixInputTables(this)"
  )
}

create_censored_assumptions_radio_buttons <- function() {
  create_radio_buttons(
    list(
      list(value = "excluded", label = "Excluded", checked = TRUE),
      list(value = "adjusted", label = "Adjusted", checked = FALSE)
    ),
    "Censored Events:",
    "censored_assumption_radiobutton_",
    "ManipulateConfusionMatrixInputTables(this)"
  )
}

create_predictions_cutoff_filter <- function(by) {
  rangeFilter(
    "confusion_matrix_for_display",
    "filtered_performance_table",
    "chosen_cutoff",
    "Probability Threshold", #TODO: should be manipulated
    inputId = "cutoff_slider",
    value = 0.2,
    min = 0,
    max = 1,
    step = by
  )
}


create_confusion_matrices_data <- function(
  performance_data,
  stratified_by = c("probability_threshold", "ppcr")
) {
  # View(performance_data)

  purrr::map(
    stratified_by,
    ~ create_confusion_matrix_for_display_data(
      performance_data,
      stratified_by = .x
    )
  ) |>
    purrr::map(
      ~ tidyr::pivot_longer(
        .x,
        cols = tidyselect::any_of(
          c("probability_threshold", "ppcr")
        ),
        values_to = c("chosen_cutoff"),
        names_to = c("stratified_by")
      )
    )
}

run_function_over_probs_reals_lists <- function(
  probs,
  reals,
  func,
  times,
  fixed_horizon_times
) {
  if (length(reals) > 1) {
    purrr::map2(
      probs,
      reals,
      \(probs_vec, reals_vec)
        func(
          probs_vec = probs_vec,
          reals_vec = reals_vec,
          times,
          fixed_horizon_times
        )
    ) |>
      purrr::set_names(names(probs))
  } else {
    purrr::map(
      probs,
      \(probs_vec)
        func(
          probs_vec = probs_vec,
          reals_vec = reals[[1]],
          times,
          fixed_horizon_times
        )
    ) |>
      purrr::set_names(names(probs))
  }
}

# DONE: fix by to ...
run_function_with_stratified_by_values <- function(
  data_input,
  stratified_by_values,
  func,
  ...
) {
  dynamic_dots <- rlang::list2(...)

  # print("func being used")
  # print(func)
  #
  # print("dynamic_dots in run_function_with_stratified_by_values")
  # print(dynamic_dots)
  #
  # extras=list(...)
  # print("extras")
  # print(extras)
  #
  # cl <- match.call()
  # print("match.call")
  # print(cl)

  # print("stratified_by_values")
  # print(stratified_by_values)
  #
  stratified_by_values_new <- list("probability_threshold", "ppcr")
  # stratified_by_values_new <- list("probability_threshold")
  # stratified_by_values_new <- list("ppcr")

  # print("waldo compare")
  # View(waldo::compare(as.list(stratified_by_values), stratified_by_values_new))
  #
  # View(stratified_by_values)

  stratified_by_values_new |>
    purrr::map(~ func(data_input, stratified_by = .x, dynamic_dots)) |>
    purrr::set_names(stratified_by_values_new)
}

create_nested_data_frame_with_probs_distributions <- function(
  probs,
  reals,
  times = NULL,
  fixed_horizon_times = NULL,
  stratified_by,
  by
) {
  # print("probs in create_nested_data_frame_with_probs_distributions")
  # View(probs)
  #
  # print("by in create_nested_data_frame_with_probs_distributions")
  # print(by)

  nested_data <- run_function_over_probs_reals_lists(
    probs = probs,
    reals = reals,
    func = turn_reals_codes_to_reals_labels,
    times = times,
    fixed_horizon_times = fixed_horizon_times
  )

  nested_data <- nested_data |>
    purrr::map(
      \(nested_data)
        run_function_with_stratified_by_values(
          nested_data,
          add_probs_distributions_to_reals_labels,
          stratified_by = stratified_by,
          by = by
        )
    )

  saveRDS(nested_data, "nested_data_to_debug.rds")

  nested_data |>
    purrr::map(
      add_initial_zoom_to_nested_data,
      run_function_with_stratified_by_values(
        nested_data,
        stratified_by_values = stratified_by,
        func = extract_ranges_for_probs_histogram,
        by = by
      )
    )
}

create_mids_and_counts_data_from_probs_times <- function(
  probs,
  breaks,
  stratified_by,
  fixed_horizon_times,
  weights,
  by
) {
  # View(breaks)
  # View(probs)

  if (length(probs) == 0) {
    return(
      tibble::tibble(
        mids = round(
          seq(by, 1 + by, by = by),
          digits = nchar(format(by, scientific = FALSE))
        ),
        counts = 0
      )
    )
  }

  hist_data <- hist(
    probs,
    plot = FALSE,
    breaks = breaks
  )

  # print("hist_data")
  # print(hist_data)

  if (stratified_by == "probability_threshold") {
    hist_data %>%
      .[c("mids", "counts")] |>
      tibble::as_tibble() |>
      tibble::add_row(
        mids = 0,
        counts = as.integer(0),
        .before = 0
      )
  } else if (stratified_by == "ppcr") {
    tibble::tibble(
      mids = round(
        seq(by, 1 + by, by = by),
        digits = nchar(format(by, scientific = FALSE))
      ),
      counts = as.integer(c(hist_data$counts, 0)) # TODO fix this
    )
  }
}

prepare_cumulative_probs_distribution_data <- function(
  probs_distribution_data,
  stratified_by,
  by
) {
  if (stratified_by == "probability_threshold") {
    # print("This is the data")
    # print(probs_distribution_data)

    probs_distribution_data |>
      purrr::map(
        function(x) {
          mutate(
            x,
            lower_bound = 0
          ) |>
            add_include_bound_variables_to_probs_distribution_data() |>
            group_by(reference_group) |>
            mutate(
              n_obs = sum(counts),
              counts = cumsum(counts)
            ) |>
            ungroup() |>
            add_text_variables_to_probs_distribution_data(
              stratified_by = stratified_by,
              by = by
            ) |>
            dplyr::mutate(reference_group = "Train")
        }
      )
  } else {
    # print("by")
    # print(by)

    probs_distribution_data |>
      purrr::map(
        function(x) {
          # mutate(
          #   x,
          #   lower_bound = 0) |>
          group_by(x, reference_group) |>
            dplyr::arrange(desc(mids)) |>
            mutate(
              n_obs = sum(counts),
              counts = cumsum(counts)
            ) |>
            ungroup() |>
            add_text_variables_to_probs_distribution_data(
              stratified_by,
              cumulative = TRUE,
              by = by
            )
        }
      )
  }
}


turn_cumulative_probs_distribution_data_to_performance_data <- function(
  cumulative_probs_distribution_data,
  reference_group_column_name,
  by = 0.01,
  stratified_by = "probability_threshold"
) {
  if (stratified_by == "probability_threshold") {
    main_slider_tibble <- tibble(
      probability_threshold = round(
        seq(0, 1, by = by),
        digits = nchar(format(by, scientific = FALSE))
      )
    )
  } else {
    main_slider_tibble <- tibble(
      ppcr = round(
        seq(0, 1, by = by),
        digits = nchar(format(by, scientific = FALSE))
      )
    )
  }

  main_slider_tibble |>
    join_cumulative_probs_distribution_data_to_main_slider(
      cumulative_probs_distribution_data = cumulative_probs_distribution_data,
      reference_group_column_name = {{ reference_group_column_name }},
      stratified_by = stratified_by
    ) |>
    add_performance_metrics_to_performance_data(stratified_by)
}

# TODO: design better function that takes into account all combinations
# of assumptions

add_performance_metrics_to_performance_data <- function(
  performance_data,
  stratified_by
) {
  performance_data <- performance_data |>
    dplyr::mutate(
      N = real_positives + real_negatives,
      sensitivity = TP / (TP + FN),
      FPR = FP / (FP + TN),
      specificity = TN / (TN + FP),
      PPV = TP / (TP + FP),
      NPV = TN / (TN + FN),
      lift = (TP / (TP + FN)) / ((TP + FP) / N),
      predicted_positives = TP + FP,
      predicted_negatives = TN + FN,
      real_positives = TP + FN,
      real_negatives = FP + TN
      # ppcr = (TP + FP) / N,
      # NB = TP / N - (FP / N) * (
      # probability_threshold / (1 - probability_threshold))
    )

  if (
    "real_competing" %in%
      names(performance_data)
  ) {
    performance_data <- performance_data |>
      dplyr::mutate(
        real_competing = competing_predicted_positives +
          competing_predicted_negatives,
        predicted_positives = TP + FP + competing_predicted_positives,
        predicted_negatives = TN + FN + competing_predicted_negatives,
        N = real_positives + real_negatives + real_competing
      )
  }

  if (
    "real_censored" %in%
      names(performance_data)
  ) {
    performance_data <- performance_data |>
      dplyr::mutate(
        real_censored = censored_predicted_positives +
          censored_predicted_negatives,
        predicted_positives = TP +
          FP +
          competing_predicted_positives +
          censored_predicted_positives,
        predicted_negatives = TN +
          FN +
          competing_predicted_negatives +
          censored_predicted_negatives,
        N = real_positives + real_negatives + real_competing + real_censored
      )
  }

  if (stratified_by == "probability_threshold") {
    performance_data <- performance_data |>
      dplyr::mutate(
        NB = TP /
          N -
          (FP / N) * (probability_threshold / (1 - probability_threshold)),
        ppcr = predicted_positives / N
      )
  }

  performance_data
}

# TODO: ensure consistency of join_cumulative function and refactor

join_cumulative_probs_distribution_data_to_main_slider <- function(
  main_slider_tibble,
  cumulative_probs_distribution_data,
  reference_group_column_name,
  stratified_by
) {
  if (stratified_by == "probability_threshold") {
    performance_data <- main_slider_tibble |>
      dplyr::left_join(
        cumulative_probs_distribution_data$real_negatives |>
          select(reference_group, upper_bound, counts, n_obs) |>
          dplyr::rename(
            {{ reference_group_column_name }} := "reference_group",
            "probability_threshold" = "upper_bound",
            "TN" = "counts",
            "real_negatives" = "n_obs",
          ),
        by = "probability_threshold"
      ) |>
      mutate(
        "FP" = real_negatives - TN
      ) |>
      left_join(
        cumulative_probs_distribution_data$real_positives |>
          select(reference_group, upper_bound, counts, n_obs) |>
          dplyr::rename(
            {{ reference_group_column_name }} := "reference_group",
            "probability_threshold" = "upper_bound",
            "FN" = "counts",
            "real_positives" = "n_obs",
          ),
        by = c("probability_threshold", reference_group_column_name)
      ) |>
      mutate(
        "TP" = real_positives - FN
      )

    if (
      "real_competing" %in%
        names(cumulative_probs_distribution_data)
    ) {
      performance_data <- performance_data |>
        dplyr::left_join(
          cumulative_probs_distribution_data$real_competing |>
            select(reference_group, upper_bound, counts, n_obs) |>
            dplyr::rename(
              {{ reference_group_column_name }} := "reference_group",
              "probability_threshold" = "upper_bound",
              "competing_predicted_negatives" = "counts",
              "real_competing" = "n_obs",
            ),
          by = c("probability_threshold", reference_group_column_name)
        ) |>
        mutate(
          "competing_predicted_positives" = real_competing -
            competing_predicted_negatives
        )
    }

    if (
      "real_censored" %in%
        names(cumulative_probs_distribution_data)
    ) {
      performance_data <- performance_data |>
        dplyr::left_join(
          cumulative_probs_distribution_data$real_censored |>
            select(reference_group, upper_bound, counts, n_obs) |>
            dplyr::rename(
              {{ reference_group_column_name }} := "reference_group",
              "probability_threshold" = "upper_bound",
              "censored_predicted_negatives" = "counts",
              "real_censored" = "n_obs",
            ),
          by = c("probability_threshold", reference_group_column_name)
        ) |>
        mutate(
          "censored_predicted_positives" = real_censored -
            censored_predicted_negatives
        )
    }
  } else {
    performance_data <- main_slider_tibble |>
      dplyr::left_join(
        cumulative_probs_distribution_data$real_negatives |>
          dplyr::mutate(ppcr = round(ppcr, digits = 2)) |> # TODO: fix this
          select(reference_group, ppcr, counts, n_obs) |>
          dplyr::rename(
            {{ reference_group_column_name }} := "reference_group",
            "FP" = "counts",
            "real_negatives" = "n_obs",
          ),
        by = "ppcr"
      ) |>
      mutate(
        "TN" = real_negatives - FP
      ) |>
      left_join(
        cumulative_probs_distribution_data$real_positives |>
          select(reference_group, ppcr, counts, n_obs) |>
          dplyr::rename(
            {{ reference_group_column_name }} := "reference_group",
            "TP" = "counts",
            "real_positives" = "n_obs",
          ),
        by = c("ppcr", reference_group_column_name)
      ) |>
      mutate(
        "FN" = real_positives - TP
      )

    if (
      "real_competing" %in%
        names(cumulative_probs_distribution_data)
    ) {
      performance_data <- performance_data |>
        dplyr::left_join(
          cumulative_probs_distribution_data$real_competing |>
            select(reference_group, upper_bound, counts, n_obs) |>
            dplyr::rename(
              {{ reference_group_column_name }} := "reference_group",
              "ppcr" = "upper_bound",
              "competing_predicted_negatives" = "counts",
              "real_competing" = "n_obs",
            ),
          by = c("ppcr", reference_group_column_name)
        ) |>
        mutate(
          "competing_predicted_positives" = real_competing -
            competing_predicted_negatives
        )
    }

    if (
      "real_censored" %in%
        names(cumulative_probs_distribution_data)
    ) {
      performance_data <- performance_data |>
        dplyr::left_join(
          cumulative_probs_distribution_data$real_censored |>
            select(reference_group, upper_bound, counts, n_obs) |>
            dplyr::rename(
              {{ reference_group_column_name }} := "reference_group",
              "ppcr" = "upper_bound",
              "censored_predicted_negatives" = "counts",
              "real_censored" = "n_obs",
            ),
          by = c("ppcr", reference_group_column_name)
        ) |>
        mutate(
          "censored_predicted_positives" = real_censored -
            censored_predicted_negatives
        )
    }
  }

  performance_data
}

create_confusion_matrix_for_display_data <- function(
  performance_data,
  stratified_by = "probability_threshold"
) {
  # print("stratified_by")
  # print(stratified_by)

  if (stratified_by == "probability_threshold") {
    confusion_matrix_for_display_data <- performance_data |>
      arrange(reference_group, {{ stratified_by }}) %>%
      split(f = .[stratified_by]) |>
      purrr::map_df(function(x) {
        tibble::tibble(
          probability_threshold = rep(x$probability_threshold, 5),
          type = c(
            "real_competing",
            "real_negatives",
            "real_positives",
            "total_included",
            "total_predicted"
          ),
          predicted_negatives = c(
            x$competing_predicted_negatives,
            x$TN,
            x$FN,
            x$TN + x$FN,
            x$predicted_negatives
          ),
          predicted_positives = c(
            x$competing_predicted_positives,
            x$FP,
            x$TP,
            x$FP + x$TP,
            x$predicted_positives
          ),
          total_reals = c(
            x$real_competing,
            x$real_negatives,
            x$real_positives,
            x$real_negatives + x$real_positives,
            x$N
          ),
        )
      })
  } else {
    confusion_matrix_for_display_data <- performance_data |>
      arrange(reference_group, {{ stratified_by }}) %>%
      split(f = .[stratified_by]) |>
      purrr::map_df(function(x) {
        tibble::tibble(
          ppcr = rep(x$ppcr, 3),
          type = c("real_negatives", "real_positives", "total_predicted"),
          predicted_negatives = c(x$TN, x$FN, x$predicted_negatives),
          predicted_positives = c(x$FP, x$TP, x$predicted_positives),
          total_reals = c(x$real_negatives, x$real_positives, x$N),
        )
      })
  }

  confusion_matrix_for_display_data
}


render_confusion_matrix_for_probs_histogram <- function(
  confusion_matrix,
  elementID,
  elementIDinputconfusionmatrix
) {
  # print("elementID")
  # print(elementID)

  N <- confusion_matrix |>
    dplyr::filter(type == "total_predicted") |>
    dplyr::slice(1) |>
    dplyr::pull(total_reals) |>
    unique()

  # Filter method that filters numeric columns by minimum value
  filterMinValue <- reactable::JS(
    "function(rows, columnId, filterValue) {
  
  //console.log('triggered!')
  
  return rows.filter(function(row) {
    return row.values[columnId] == filterValue
  })
}"
  )

  reactable::reactable(
    confusion_matrix,
    sortable = FALSE,
    fullWidth = FALSE,
    borderless = FALSE,
    defaultColDef = reactable::colDef(
      style = reactable::JS(
        confusion_matrix_style_maker()
      ),
      html = TRUE,
      align = "center",
      headerStyle = reactable::JS(
        paste0(
          'function(column, state) {
          
          if (column.id === "type") {
          
            
          
          }
          
          return { 
      fontWeight: `normal`
      }
          
          ',

          '}'
        )
      ),
      show = FALSE
    ),
    columns = list(
      chosen_cutoff = reactable::colDef(
        filterMethod = filterMinValue
      ),
      predicted_positives = reactable::colDef(
        name = "Predicted Positives",
        cell = reactable::JS(paste0(
          'function(cellInfo, state) {

            const N = state.meta.n_obs;
            const value = `${cellInfo.value}<br>(${((cellInfo.value / N) * 100).toFixed(2)}%)`
            const disabled = ""
            


             if (cellInfo.row.type === "real_negatives") {
            
            const checked = state.meta.highlightedMetrics.FP ? "checked" : ""
            return `',
          create_confusion_matrix_renered_text("FP"),
          '`
            
            } else if (cellInfo.row.type === "real_positives") {
            

            const checked = state.meta.highlightedMetrics.TP ? "checked" : ""
            return `',
          create_confusion_matrix_renered_text("TP"),
          "`
            
            } else {
            
            return `<br>${value}`
            
            }

          }"
        )),
        show = TRUE
      ),
      predicted_negatives = reactable::colDef(
        name = "Predicted Negatives",
        cell = reactable::JS(
          'function(cellInfo, state) {
          

            const N = state.meta.n_obs;
            const value = `${cellInfo.value}<br>(${((cellInfo.value / N) * 100).toFixed(2)}%)`
            const disabled = ""


            if (cellInfo.row.type === "real_negatives") {
            
            const checked = state.meta.highlightedMetrics.TN ? "checked" : ""
            return `',
          create_confusion_matrix_renered_text("TN"),
          '`
            
            } else if (cellInfo.row.type === "real_positives") {
            
            const checked = state.meta.highlightedMetrics.FN ? "checked" : ""
            return `',
          create_confusion_matrix_renered_text("FN"),
          "`
            
            } else {
            
            return `<br>${value}`
            
            }
            
            
          }"
        ),
        show = TRUE
      ),
      type = reactable::colDef(
        name = "All Observations",
        align = "left",
        minWidth = 200,
        show = TRUE
      ),
      total_reals = reactable::colDef(
        name = "Total<br>Reals",
        # header = "",
        align = "left",
        cell = reactable::JS(
          "function(cellInfo, state) {
          
            const N = state.meta.n_obs;
            const value = `${cellInfo.value}<br>(${((cellInfo.value / N) * 100).toFixed(2)}%)`

          if (cellInfo.row.type === 'total_predicted') {

            return `<br>${value}`
            
          } else {
          
            return `<br>${value}`
          
          }

          }"
        ),
        show = TRUE
      )
    ),
    meta = list(
      highlightedMetrics = list(
        TP = TRUE,
        FP = TRUE,
        TN = TRUE,
        FN = TRUE,
        N = TRUE,
        predicted_positives = TRUE,
        predicted_negatives = TRUE,
        real_positives = TRUE,
        real_negatives = TRUE,
        real_competing = TRUE,
        real_censored = TRUE,
        total_reals = TRUE,
        total_predicted = TRUE,
        competing_predicted_negatives = TRUE,
        competing_predicted_positives = TRUE,
        censored_predicted_negatives = TRUE,
        censored_predicted_positives = TRUE
      ),
      n_obs = N
    ),
    elementId = elementID,
    pagination = FALSE
  )
}

confusion_matrix_style_maker <- function() {
  "function(rowInfo, column, state) {
  const metrics = state.meta.highlightedMetrics;
  let value;

  const highlightStyles = {
    censored_predicted_positives: { checked: '#E3F09B', unchecked: '#f3f9d7' },
    competing_predicted_positives: { checked: '#DAB1E3', unchecked: '#f5ebf8' },
    TP: { checked: '#90EE90', unchecked: '#F4FFF0' },
    FP: { checked: '#FAC8CD', unchecked: '#FFF7F8' },
    censored_predicted_negatives: { checked: '#E3F09B', unchecked: '#f3f9d7' },
    competing_predicted_negatives: { checked: '#DAB1E3', unchecked: '#f5ebf8' },
    TN: { checked: '#90EE90', unchecked: '#F4FFF0' },
    FN: { checked: '#FAC8CD', unchecked: '#FFF7F8' },
    total_real_positives: { checked: '#D3D3D3', unchecked: '#eef2f8' },
    total_real_negatives: { checked: '#D3D3D3', unchecked: '#eef2f8' },
    total_real_competing: { checked: '#DAB1E3', unchecked: '#f5ebf8' },
    total_real_censored: { checked: '#E3F09B', unchecked: '#f3f9d7' },
    total_obs: { checked: '#D3D3D3', unchecked: '#eef2f8' },
    total_predicted_positives: { checked: '#D3D3D3', unchecked: '#eef2f8' },
    total_predicted_negatives: { checked: '#D3D3D3', unchecked: '#eef2f8' },
    total_included_predicted_negatives: { checked: '#D3D3D3', unchecked: '#eef2f8' },
    total_included_predicted_positives: { checked: '#D3D3D3', unchecked: '#eef2f8' },
    total_included: { checked: '#D3D3D3', unchecked: '#eef2f8' }
  };
  
  const rowMarginHighlight = (metric) => {
    
    if (metrics[metric] && value === metric) {
      return { fontWeight: '600' };
    } else if (!metrics[metric] && value === metric) {
      return { fontWeight: '400' };
    }
    
  };

  const checkHighlight = (metric) => {
    const styles = highlightStyles[metric];
    if (metrics[metric] && value === metric) {
      return { backgroundColor: styles.checked, fontWeight: '600' };
    } else if (!metrics[metric] && value === metric) {
      return { backgroundColor: styles.unchecked, fontWeight: '400' };
    }
    return {};
  };
  
  const checkHighlightPartialFilled = (metric) => {
  
  let N = state.meta.n_obs
  let width = rowInfo.values[column.id] / state.meta.n_obs
  let background = 100*width + '%'
  
  //console.log('metric')
  //console.log(metric)
  
    const styles = highlightStyles[metric];
    if (metrics[metric] && value === metric) {
      return { 
      background: `linear-gradient(90deg, ${styles.checked} ${background}, transparent ${background})`,
      backgroundPosition: `center`,
      backgroundSize: `98% 88%`,
      backgroundRepeat: `no-repeat`,
      fontWeight: '600' };
    } else if (!metrics[metric] && value === metric) {
      return { 
      background: `linear-gradient(90deg, ${styles.unchecked} ${background}, transparent ${background})`,
      backgroundPosition: `center`,
      backgroundSize: `98% 88%`,
      backgroundRepeat: `no-repeat`,
      fontWeight: '400' };
    }
    return {};
  };

  if (column.id === 'predicted_positives') {
    if (rowInfo.values.type.toLowerCase() === 'real_positives') {
      value = 'TP';
    } else if (rowInfo.values.type.toLowerCase() === 'real_negatives') {
      value = 'FP';
    } else if (rowInfo.values.type.toLowerCase() === 'real_competing') {
      value = 'competing_predicted_positives';
    } else if (rowInfo.values.type.toLowerCase() === 'real_censored') {
      value = 'censored_predicted_positives';
    } else if (rowInfo.values.type.toLowerCase() === 'total_predicted') {
      value = 'total_predicted_positives';
    } else if (rowInfo.values.type.toLowerCase() === 'total_included') {
      value = 'total_included_predicted_positives';
    }
    
    if (state.meta.n_obs === undefined) {
      
      return checkHighlight(value);
    
    } else {
    
      return checkHighlightPartialFilled(value);
    
    }
    
    
    
    
  } else if (column.id === 'predicted_negatives') {
    if (rowInfo.values.type.toLowerCase() === 'real_positives') {
      value = 'FN';
    } else if (rowInfo.values.type.toLowerCase() === 'real_negatives') {
      value = 'TN';
    } else if (rowInfo.values.type.toLowerCase() === 'real_competing') {
      value = 'competing_predicted_negatives';
    } else if (rowInfo.values.type.toLowerCase() === 'real_censored') {
      value = 'censored_predicted_negatives';
    } else if (rowInfo.values.type.toLowerCase() === 'total_predicted') {
      value = 'total_predicted_negatives';
    } else if (rowInfo.values.type.toLowerCase() === 'total_included') {
      value = 'total_included_predicted_negatives';
    }
    
    if (state.meta.n_obs === undefined) {
      
      return checkHighlight(value);
    
    } else {
    
      return checkHighlightPartialFilled(value);
    
    }
  } else if (column.id === 'total_reals') {
    if (rowInfo.values.type.toLowerCase() === 'real_positives') {
      value = 'total_real_positives';
    } else if (rowInfo.values.type.toLowerCase() === 'real_negatives') {
      value = 'total_real_negatives';
    } else if (rowInfo.values.type.toLowerCase() === 'real_competing') {
      value = 'total_real_competing';
    } else if (rowInfo.values.type.toLowerCase() === 'real_censored') {
      value = 'total_real_censored';
    } else if (rowInfo.values.type.toLowerCase() === 'total_predicted') {
      value = 'total_obs';
    } else if (rowInfo.values.type.toLowerCase() === 'total_included') {
      value = 'total_included';
    }
    
    return checkHighlightPartialFilled(value);
      
  } else if (column.id === 'type') {
    
    value = rowInfo.values.type
    
    return rowMarginHighlight(value);
    
    
  }
}

"
}

create_confusion_matrix_renered_text <- function(
  confusion_metric
) {
  paste0(
    confusion_metric,
    "<br>${value}"
  )
}

create_confusion_matrix_as_input_element <- function(
  elementID,
  cmdisplayID,
  real_competing = FALSE,
  real_censored = FALSE
) {
  tibble_for_confusion_matrix_as_input <- tibble::tribble(
    ~"type",
    ~"predicted_negatives",
    ~"predicted_positives",
    # "Real Negatives",
    "real_negatives",
    "TN",
    "FP",
    # "Real Positives",
    "real_positives",
    "FN",
    "TP"
  )

  if (real_competing == TRUE) {
    tibble_for_confusion_matrix_as_input <- tibble_for_confusion_matrix_as_input |>
      dplyr::add_row(
        type = "real_competing",
        `predicted_negatives` = "competing_predicted_negatives",
        `predicted_positives` = "competing_predicted_positives",
        .before = 1
      )
  }

  if (real_censored == TRUE) {
    tibble_for_confusion_matrix_as_input <- tibble_for_confusion_matrix_as_input |>
      dplyr::add_row(
        type = "real_censored",
        `predicted_negatives` = "censored_predicted_negatives",
        `predicted_positives` = "censored_predicted_positives",
        .before = 1
      )
  }

  filterCompeting <- reactable::JS(
    "function(rows, columnId, filterValue) {
    
    
    if (filterValue === 'all_excluded') {
    
    //console.log('filterValue all excluded')
    //console.log(filterValue)
    
      return rows
    
    } else if (filterValue === 'competing_nonevent_censored_excluded' ) {
    
      return rows.filter(function(row) {
        return row.values[columnId] != 'real_competing'
        
        })
    
    } else if (filterValue === 'competing_nonevent_censored_adjusted' ) {
    
      return rows.filter(function(row) {
        return row.values[columnId] != 'real_censored' && row.values[columnId] != 'real_competing'
        
        })
    
    } else if (filterValue === 'competing_excluded_censored_adjusted' ) {
    
      return rows.filter(function(row) {
        return row.values[columnId] != 'real_censored'
      })
    
    }
    
  
}"
  )

  tibble_for_confusion_matrix_as_input |>
    reactable::reactable(
      sortable = FALSE,
      fullWidth = FALSE,
      borderless = FALSE,
      defaultColDef = reactable::colDef(
        html = TRUE,
        align = "center",
        header = reactable::JS(confusion_matrix_header_maker()),
        style = reactable::JS(confusion_matrix_style_maker()),
        cell = reactable::JS(confusion_matrix_cell_maker()),
        minWidth = 300
      ),
      columns = list(
        predicted_positives = reactable::colDef(
          name = "Predicted Positives"
        ),
        type = reactable::colDef(
          # TODO: remove linespace from radiobutton
          name = "All Observations",
          align = "left",
          minWidth = 200,
          filterMethod = filterCompeting,
          # filterMethod = reactable::JS("function(rows, columnId, filterValue) {
          #
          # if ( filterValue === 'excluded' ) {
          #
          #   return rows.filter(function(row) {
          #
          #   console.log('row.values')
          #   console.log(row.values.type)
          #
          #   console.log(columnId)
          #   console.log('columnId')
          #
          #   const hasMissing = row.values
          #       return hasMissing
          #     })
          #
          #   return rows
          # }
          #
          # console.log('This is my filter value')
          # console.log(filterValue)
          #
          # console.log('These are my rows')
          # console.log(rows)
          #
          #   if (filterValue === true) {
          #     return rows.filter(function(row) {
          #   const hasMissing = row.values[columnId]
          #       return hasMissing
          #     })
          #   }
          #   return rows
          # }")
        ),
        predicted_negatives = reactable::colDef(
          name = "Predicted Negatives"
        )
      ),
      meta = list(
        highlightedMetrics = list(
          TP = TRUE,
          FP = TRUE,
          TN = TRUE,
          FN = TRUE,
          N = TRUE,
          PP = TRUE,
          PN = TRUE,
          RP = TRUE,
          RN = TRUE,
          competing_predicted_negatives = TRUE,
          competing_predicted_positives = TRUE,
          censored_predicted_negatives = TRUE,
          censored_predicted_positives = TRUE,
          real_competing = TRUE,
          real_censored = TRUE
        ),
        exclusion_assumptions = list(
          excluded_competing_risks = TRUE,
          excluded_censored_risks = TRUE
        ),
        elementID = elementID
      ),
      elementId = elementID
    )
}

confusion_matrix_cell_maker <- function(confusionMatrixInputID, cmdisplayID) {
  'function(cellInfo, state) {
    const { currency, exchangeRates, elementID } = state.meta;
    const cellInfoValueMetric = cellInfo.value.toLowerCase();

    const specialCases = {
        "competing_predicted_positives": "Competing & Predicted Positives",
        "competing_predicted_negatives": "Competing & Predicted Negatives",
        "censored_predicted_positives": "Censored & Predicted Positives",
        "censored_predicted_negatives": "Censored & Predicted Negatives",
        "real_competing": "Real Competing",
        "real_censored": "Real Censored"
    };

    const simpleCases = ["tp", "fp", "tn", "fn"];

    if (simpleCases.includes(cellInfoValueMetric)) {
        return `${cellInfo.value}`;
    }

    if (specialCases[cellInfoValueMetric]) {
        return specialCases[cellInfoValueMetric];
    }

    if (cellInfoValueMetric === "real_positives" || cellInfoValueMetric === "real_negatives") {
        const checkbox_id = `${cellInfoValueMetric}_checkbox-${elementID}`;
        const checkbox_name = `condition_checkbox-${elementID}`;
        const value = cellInfoValueMetric === "real_positives" ? "rp" : "rn";
        const label = cellInfoValueMetric === "real_positives" ? "Real Positives" : "Real Negatives";

        return `<input type="radio" id="${checkbox_id}" value="${value}" name="${checkbox_name}" checked="false" onclick="ManipulateReactablesByRadioButtons(this)"> ${label}`;
    }

    return `${cellInfo.value}`;
}'
}


confusion_matrix_header_maker <- function(confusionMatrixInputID, cmdisplayID) {
  paste(
    'function(column, state) {
    
    const checkbox_id = column.column.id + "_checkbox" + "-" + state.meta.elementID
  
  const checkbox_name = "condition_checkbox-" + state.meta.elementID
    
    if ( column.column.id === "predicted_positives" ) {

        return `<input type = "radio" id = ${checkbox_id} value = "pp" name = ${checkbox_name} checked = false onclick=ManipulateReactablesByRadioButtons(this)> Predicted Positives`
        
      } else if ( column.column.id === "predicted_negatives" ) {
      
    return `<input type = "radio" id = ${checkbox_id} value = "pn" name = ${checkbox_name} checked = false onclick=ManipulateReactablesByRadioButtons(this)> Predicted Negatives` } else if ( column.column.id === "type" ) {
    
    return `<input type = "radio" id = ${checkbox_id} value = "ao" name = ${checkbox_name} checked = false onclick=ManipulateReactablesByRadioButtons(this)> All Observations` }
      }'
  )
}

add_include_bound_variables_to_probs_distribution_data <- function(
  probs_distribution_data
) {
  probs_distribution_data |>
    dplyr::mutate(
      include_lower_bound = (lower_bound == 0 & upper_bound != 0),
      include_upper_bound = upper_bound != 0
    )
}

add_text_variables_to_probs_distribution_data <- function(
  probs_distribution_data,
  stratified_by,
  cumulative = FALSE,
  by
) {
  if (stratified_by == "probability_threshold") {
    probs_distribution_data |>
      dplyr::mutate(
        text_obs = glue::glue("{counts} observations in "),
        text_range = glue::glue(
          "{ifelse(include_lower_bound==TRUE,'[','(')}{lower_bound}, \\
        {upper_bound}{ifelse(include_upper_bound==TRUE,']',')')}"
        ),
        text = glue::glue("{text_obs}{text_range}")
      )
  } else {
    if (cumulative == FALSE) {
      probs_distribution_data |>
        dplyr::mutate(
          text_obs = glue::glue("{counts} observations"),
          text_range = glue::glue(
            "Probability Percentile of {mids}"
          ),
          text = glue::glue("{text_obs}")
        )
    } else {
      # print(probs_distribution_data)

      probs_distribution_data |>
        dplyr::mutate(
          ppcr = round(
            1 - mids + by,
            digits = nchar(format(by, scientific = FALSE))
          ),
          text_obs = glue::glue("{counts} observations with ppcr of "),
          text_range = glue::glue(
            "{ppcr}"
          ),
          text = glue::glue("{text_obs}{text_range}")
        )
    }
  }
}

add_bounds_variables <- function(probs_distribution_data, by, stratified_by) {
  # print("by in add_bounds_variable")
  # print(by)
  #
  half_by <- by / 2
  #
  # print("half_by in add_bounds_variable")
  # print(half_by)

  if (stratified_by == "probability_threshold") {
    probs_distribution_data |>
      dplyr::mutate(
        lower_bound = dplyr::case_when(
          mids > 0 ~
            round(
              mids - half_by,
              digits = nchar(format(by, scientific = FALSE))
            ),
          mids == 0 ~ 0
        ),
        upper_bound = dplyr::case_when(
          mids > 0 ~
            round(
              mids + half_by,
              digits = nchar(format(by, scientific = FALSE))
            ),
          mids == 0 ~ 0
        )
      ) |>
      add_include_bound_variables_to_probs_distribution_data()
  } else {
    probs_distribution_data |>
      dplyr::mutate(lower_bound = mids - half_by, upper_bound = mids + half_by)
  }
}


turn_reals_codes_to_reals_labels <- function(
  probs_vec,
  reals_vec,
  times,
  fixed_horizon_times
) {
  # View(probs_vec)

  real_labels <- fixed_horizon_times |>
    purrr::map(
      function(w) {
        tibble::tibble(
          reals_codes = reals_vec,
          times = times,
          probs = probs_vec,
          time_horizon_vec = w
        ) |>
          dplyr::mutate(
            reals_labels = dplyr::case_when(
              times > time_horizon_vec ~ "real_negatives",
              (times < time_horizon_vec) & (reals_codes == 0) ~ "real_censored",
              (times < time_horizon_vec) & (reals_codes == 1) ~
                "real_positives",
              (times < time_horizon_vec) & (reals_codes == 2) ~ "real_competing"
            )
          )
      }
    ) |>
    purrr::set_names(fixed_horizon_times) |>
    purrr::list_rbind(names_to = "time_horizon") |>
    tidyr::nest(.by = time_horizon, .key = "probs_reals_times")

  real_labels
}

add_probs_distributions_to_reals_labels <- function(
  reals_labels,
  stratified_by,
  by,
  ...
) {
  # View(reals_labels)
  # saveRDS(reals_labels, "reals_labels_to_debug.rds")

  dynamic_dots <- rlang::list2(...)
  # print("dynamic_dots in add_probs_distributions_to_reals_labels")
  # print(dynamic_dots)
  #
  # print("by in add_probs_distributions_to_reals_labels")
  # print(by)
  #
  # print("stratified_by in add_probs_distributions_to_reals_labels")
  # print(stratified_by)

  # by <- 0.01 #DONE: fix
  by <- as.numeric(by) # TODO why is it not numeric?

  reals_labels |>
    dplyr::mutate(
      probs_distribution = purrr::map(
        probs_reals_times,
        function(x) {
          breaks <- create_breaks_values(
            x$probs,
            stratified_by,
            by
          )

          c(
            "real_censored",
            "real_positives",
            "real_competing",
            "real_negatives"
          ) |>
            purrr::map(
              function(y) {
                create_mids_and_counts_data_from_probs_times(
                  probs = x$probs[x$reals_labels == y],
                  breaks = breaks,
                  stratified_by = stratified_by,
                  weights = NA,
                  by = by
                ) |>
                  add_bounds_variables(
                    by = by,
                    stratified_by = stratified_by
                  ) |>
                  add_text_variables_to_probs_distribution_data(
                    stratified_by,
                    by = by
                  ) |>
                  dplyr::mutate(reference_group = "Train")
              }
            ) |>
            purrr::set_names(
              c(
                "real_censored",
                "real_positives",
                "real_competing",
                "real_negatives"
              )
            )
        }
      ),
      cumulative_probs_distribution_data = purrr::map(
        probs_distribution,
        ~ prepare_cumulative_probs_distribution_data(
          .x,
          stratified_by = stratified_by,
          by = by
        )
      ),
      performance_data = purrr::map(
        cumulative_probs_distribution_data,
        ~ turn_cumulative_probs_distribution_data_to_performance_data(
          .x,
          reference_group_column_name = "reference_group",
          by = by,
          stratified_by = stratified_by
        )
      ),
      confusion_matrix_for_display_data = purrr::map(
        performance_data,
        ~ create_confusion_matrix_for_display_data(
          .x,
          stratified_by = stratified_by
        )
      )
    )
}


# To do: create a long table of confusion_matrix_data

# TODO: fix time_horizon_slider

time_horizon_slider <- function(
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
  # TODO: fix inputId

  value <- if (!is.null(value)) value else min
  inputId <- "time_horizon_slider"
  valueId <- sprintf("filter_%s_%s__value", tableId, columnId)
  oninput <- paste(
    sprintf("document.getElementById('%s').textContent = this.value;", valueId),
    # sprintf("Reactable.setFilter('%s', '%s', this.value);", tableId2, columnId),
    sprintf("Reactable.setFilter('%s', '%s', this.value)", tableId, columnId)
  )

  # inputId <- "filter_confusion_matrix_for_display_probability_threshold_test"

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
        oninput = oninput,
        onchange = oninput, # For IE11 support
        style = "width: 100%;"
      ),
      htmltools::span(id = valueId, style = "margin-left: 8px;", value)
    )
  )
}

# TODO: every range filter must have explicit ID

rangeFilter <- function(
  tableId,
  tableId2,
  columnId,
  label,
  min,
  max,
  inputId = "fakeID",
  value = NULL,
  step = NULL,
  width = "200px"
) {
  value <- if (!is.null(value)) value else min
  # inputId <- sprintf("filter_%s_%s", tableId, columnId)
  valueId <- sprintf("filter_%s_%s__value", tableId, columnId)
  oninput <- paste(
    sprintf("document.getElementById('%s').textContent = this.value;", valueId),
    sprintf("Reactable.setFilter('%s', '%s', this.value)", tableId, columnId)
  )

  # TODO: fix inputId
  # inputId <- "filter_confusion_matrix_for_display_probability_threshold_test"

  # print("inputId in range filter scope")
  # print(inputId)

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
        oninput = oninput,
        onchange = oninput, # For IE11 support
        style = "width: 100%;"
      ),
      htmltools::span(id = valueId, style = "margin-left: 8px;", value)
    )
  )
}


create_tmp_rmd_files_for_probs_histogram <- function(
  reference_groups = NULL
) {
  dir.create(path = "temp_rmd")

  temp_rmd_list <- reference_groups |>
    purrr::map(
      function(x) {
        temp_rmd_path <- paste0(
          "temp_rmd/",
          paste0(x, ".rmd")
        )

        sink(file = temp_rmd_path)
        cat(
          "  \n",
          "#### ",
          x,
          "{.tabset}  \n",
          "  \n",
          "  \n",
          "##### ",
          "PPCR",
          "  \n",
          "```{r, echo=FALSE}",
          "  \n",
          'probs_histogram_list[["',
          x,
          '"]]$ppcr',
          "  \n",
          "```",
          "  \n",
          "  \n",
          "##### ",
          "Probability Threshold",
          "  \n",
          "```{r, echo=FALSE}",
          "  \n",
          'probs_histogram_list[["',
          x,
          '"]]$probability_threshold',
          "  \n",
          "```",
          "  \n",
          sep = ""
        )

        sink()

        temp_rmd_path
      }
    ) |>
    purrr::set_names(reference_groups)

  temp_rmd_list
}


create_probs_histogram_list <- function(
  reference_groups,
  by,
  all_time_horizons_confusion_matrix_for_display_data,
  fixed_probability_threshold,
  stratified_by = c("probability_threshold", "ppcr")
) {
  # print("by in create_probs_histogram list")
  # print(by)

  probs_histogram_list <- reference_groups |>
    purrr::map(function(reference_group) {
      stratified_by |>
        purrr::map(function(stratified_by) {
          create_stratified_by_sublist(
            stratified_by,
            by,
            reference_group,
            all_time_horizons_confusion_matrix_for_display_data[[
              reference_group
            ]][[stratified_by]],
            fixed_probability_threshold
          )
        }) |>
        purrr::set_names(stratified_by)
    }) |>
    purrr::set_names(reference_groups)
}

create_stratified_by_sublist <- function(
  stratified_by,
  by,
  reference_group,
  all_time_horizons_confusion_matrix_for_display_data,
  fixed_probability_threshold
) {
  # print("all_time_horizons_confusion_matrix_for_display_data")
  # print(all_time_horizons_confusion_matrix_for_display_data)

  script <- paste(
    "
    
    const ManipulateStratifiedBy = function(radioButton) {
    
      console.log('radioButton')
      console.log(radioButton)
      
      const rangeFilterlabel = document.querySelector(`label[for=\"cutoff_slider\"]`);
      
      console.log('rangeFilterlabel')
      console.log(rangeFilterlabel)
      
      console.log('rangeFilterlabel.textContent')
      console.log(rangeFilterlabel.textContent)
      
      if (radioButton.value === 'probability_threshold') {
        
        rangeFilterlabel.textContent = 'Probability Threshold'
      
      } else {
      
        rangeFilterlabel.textContent = 'PPCR (Predicted Positives Condition Rate)'
      
      }
      
      console.log('rangeFilterlabel')
      console.log(rangeFilterlabel)
      
      console.log('rangeFilterlabel.textContent')
      console.log(rangeFilterlabel.textContent)
    
      Reactable.setFilter(
      'confusion_matrix_for_display', 
      'stratified_by', radioButton.value)
    
    }
    
    const ManipulateConfusionMatrixInputTables = function(radiobutton) {
      
      console.log('Radio Button Input Confusion Matrix')
      console.log(radiobutton)
      
      const radioButtonValue = radiobutton.value;
      console.log('radioButtonValue')
      console.log(radioButtonValue)
      
      console.log('radiobutton.name')
      console.log(radiobutton.name)
      
      const inputTableID = 'confusionMatrixInput'
      
      console.log('inputTableID')
      console.log(inputTableID)
      
    const CompetingAssumptionValueExcluded = document.getElementById('competing_assumption_radiobutton_excluded').checked
    
    const CensoredAssumptionValueExcluded = document.getElementById('censored_assumption_radiobutton_excluded').checked

    
    console.log('CompetingAssumptionValueExcluded')
    console.log(CompetingAssumptionValueExcluded)


    console.log('CensoredAssumptionValueExcluded')
    console.log(CensoredAssumptionValueExcluded)
    
    let filtervalue;
    
    if ( CompetingAssumptionValueExcluded === true && CensoredAssumptionValueExcluded === true ) {
    
      filtervalue = 'all_excluded'
    
    } else if ( CompetingAssumptionValueExcluded === true && CensoredAssumptionValueExcluded === false ) {
    
      filtervalue = 'competing_excluded_censored_adjusted'
    
    } else if ( CompetingAssumptionValueExcluded === false && CensoredAssumptionValueExcluded === true ) {
    
      filtervalue = 'competing_nonevent_censored_excluded'
    
    } else if ( CompetingAssumptionValueExcluded === false && CensoredAssumptionValueExcluded === false ) {
    
      filtervalue = 'competing_nonevent_censored_adjusted'
    
    }
    
    console.log('filtervalue')
    console.log(filtervalue)
      
      
      const setMetaForInputTable = (tableID) => {
        Reactable.setMeta(tableID, (prevMeta) => {
        
        console.log('prevMeta')
        console.log(prevMeta.exclusion_assumptions)
        
            return prevMeta;
        });
    };
    
    
    //console.log('reactableInputTable')
    //console.log(reactableInputTable)
    

    // setMetaForInputTable(inputTableID);
    
    Reactable.setFilter(inputTableID, 'type', filtervalue) 
    
    }
    
    const ManipulateReactablesByRadioButtons = function(radiobutton) {
    const radioButtonValue = radiobutton.value;
    const inputTableID = radiobutton.name.replace('condition_checkbox-', '');
    const outputTableID = radiobutton.name.replace('condition_checkbox-confusionMatrixInput', 'confusion_matrix_for_display');
    
    const highlightedMetrics = {
        'pp': { TP: true, TN: false, FP: true, FN: false, N: false, 
                predicted_positives: true,
                predicted_negatives: false,
                real_positives: false,
                censored_predicted_positives: true,
                total_included_predicted_positives: true,
                real_negatives: false, total_predicted_positives: true, competing_predicted_negatives: false, competing_predicted_positives: true},
        'pn': { TP: false, TN: true, FP: false, FN: true, N: false, 
                predicted_positives: false,
                predicted_negatives: true,
                real_positives: false,
                censored_predicted_negatives: true,
                total_included_predicted_negatives: true,
                real_negatives: false, total_predicted_negatives: true, competing_predicted_negatives: true, competing_predicted_positives: false },
        'ao': { TP: true, TN: true, FP: true, FN: true, N: true, 
                predicted_positives: true, total_predicted: true,
                predicted_negatives: true,
                total_included_predicted_negatives: true,
                total_included_predicted_positives: true,
                total_included: true,
                censored_predicted_negatives: true,
                censored_predicted_positives: true,
                real_positives: true, real_competing: true,
                real_censored: true,
                real_negatives: true, competing_predicted_negatives: true, competing_predicted_positives: true, total_real_negatives: true, total_real_positives: true, total_real_competing: true, total_predicted_positives: true, total_predicted_negatives: true, total_obs: true },
        'rp': { TP: true, TN: false, FP: false, FN: true, N: false, real_positives: true, competing_predicted_negatives: false, competing_predicted_positives: false, total_real_positives: true },
        'rn': { TP: false, TN: true, FP: true, FN: false, N: false, real_negatives: true, competing_predicted_negatives: false, competing_predicted_positives: false, total_real_negatives: true }
    };

    const setMetaForTable = (tableID) => {
        Reactable.setMeta(tableID, (prevMeta) => {
            return { highlightedMetrics: highlightedMetrics[radioButtonValue] };
        });
    };

    setMetaForTable(inputTableID);
    setMetaForTable(outputTableID);
}
    
    
    function synchronizeCheckboxes(checkbox) {
    
    
  let tableID = checkbox.name.replace('_headerRadioGroup', '');
  
  const falseNegatives = d3.selectAll('.false-negatives');
  
  const parentDiv = d3.select('#hist-predicted_probability_threshold_model');
                      
  
  
  const svg = parentDiv.select('svg');
  
  svg.selectAll('.false-negatives').attr('fill', 'red')
  
  
  const NCheckbox = document.getElementById('header_checkbox_'+
tableID + '_type');
  const pnCheckbox = document.getElementById('header_checkbox_'+
tableID + '_predicted_negatives');
const ppCheckbox = document.getElementById('header_checkbox_'+
tableID + '_predicted_positives');
const rpCheckbox = document.getElementById('reals_checkbox_'+
tableID + '_0');
const rnCheckbox = document.getElementById('reals_checkbox_'+
tableID + '_1');
  
  var nCheckboxes = document.querySelectorAll('input[type=\"radio\"][value=\"type\"]');
  var rpRadioButtons = document.querySelectorAll('input[type=\"radio\"][value=\"real_positives\"]');
  var rnRadioButtons = document.querySelectorAll('input[type=\"radio\"][value=\"real_negatives\"]');
  

        
        

    var FNfillColor = 'blue';
    
    
    var falseNegativesBars = svg.selectAll('.false-negatives')
    
    falseNegativesBars.attr('fill', FNfillColor);
                      
                      
    var FPfillColor = (NCheckbox.checked || ppCheckbox.checked) ? '#FAC8CD' : '#FFF7F8';
                      svg.selectAll('.false-positives').attr('fill', FPfillColor);
                      
                      
                      var TNfillColor = (NCheckbox.checked || pnCheckbox.checked ) ? '#009e73' : '#F4FFF0';
                      svg.selectAll('.true-negatives').attr('fill', TNfillColor);
                      
                      var TPfillColor = (NCheckbox.checked || ppCheckbox.checked ) ? '#009e73' : '#F4FFF0';
                      svg.selectAll('.true-positives').attr('fill', TPfillColor);
                      
                      
  if (checkbox.value === 'real_positives') {
  
  rpRadioButtons.forEach(function(radioButton) {
    radioButton.addEventListener('change', function() {
    })
    
    radioButton.checked = true
  })
  
  trRadioButtons.forEach(function(radioButton) {
    radioButton.addEventListener('change', function() {
    })
    
    radioButton.checked = true
  })
  
  
  } else if (checkbox.value === 'real_negatives') {
  
  rnRadioButtons.forEach(function(radioButton) {
    radioButton.addEventListener('change', function() {
    })
    
    radioButton.checked = true
    })
    
    trRadioButtons.forEach(function(radioButton) {
    radioButton.addEventListener('change', function() {
    })
    
    radioButton.checked = true
  })
    
  }
  
  Reactable.setMeta(tableID, 
  prevMeta => {

    let updatedMetrics = {
      TP: prevMeta.highlightedMetrics.TP,
      TN: prevMeta.highlightedMetrics.TN,
      FP: prevMeta.highlightedMetrics.FP,
      FN: prevMeta.highlightedMetrics.FN,
      predicted_positives: false,
      predicted_negatives: false,
      real_positives: false,
      real_negatives: false,
      total_predicted: false,
      total_reals: false,
      N: true
    };
    
  if (checkbox.value === 'type') {
  
  updatedMetrics['TP'] = true
  updatedMetrics['FP'] = true
  updatedMetrics['TN'] = true
  updatedMetrics['FN'] = true
  updatedMetrics['real_positives'] = true
  updatedMetrics['real_negatives'] = true
  updatedMetrics['predicted_positives'] = true
  updatedMetrics['predicted_negatives'] = true
  updatedMetrics['N'] = true
  
  } else if (checkbox.value === 'real_positives') {
  
  updatedMetrics['TN'] = false
  updatedMetrics['FP'] = false
  updatedMetrics['TP'] = true
  updatedMetrics['FN'] = true
  updatedMetrics['real_positives'] = true
  updatedMetrics['real_negatives'] = false
  updatedMetrics['predicted_positives'] = false
  updatedMetrics['predicted_negatives'] = false
  updatedMetrics['N'] = false
  
  } else if (checkbox.value === 'real_negatives') {
  
  updatedMetrics['TP'] = false
  updatedMetrics['FN'] = false
  updatedMetrics['TN'] = true
  updatedMetrics['FP'] = true
  updatedMetrics['real_positives'] = false
  updatedMetrics['real_negatives'] = true
  updatedMetrics['predicted_positives'] = false
  updatedMetrics['predicted_negatives'] = false
  updatedMetrics['N'] = false
  
  } else if (checkbox.value === 'predicted_positives') {
  
  updatedMetrics['TN'] = false
  updatedMetrics['FN'] = false
  updatedMetrics['TP'] = true
  updatedMetrics['FP'] = true
  updatedMetrics['real_positives'] = false
  updatedMetrics['real_negatives'] = false
  updatedMetrics['predicted_positives'] = true
  updatedMetrics['predicted_negatives'] = false
  updatedMetrics['N'] = false
  
  } else if (checkbox.value === 'predicted_negatives') {
  
  updatedMetrics['TP'] = false
  updatedMetrics['FP'] = false
  updatedMetrics['FN'] = true
  updatedMetrics['TN'] = true
  updatedMetrics['real_positives'] = false
  updatedMetrics['real_negatives'] = false
  updatedMetrics['predicted_positives'] = false
  updatedMetrics['predicted_negatives'] = true
  updatedMetrics['N'] = false
  
  } 

    return { highlightedMetrics: updatedMetrics };
  });
}"
  )

  filteredperformancetableId <- gsub(
    "[,\\s]+",
    "_",
    paste(
      "filtered_performance_table",
      stratified_by,
      reference_group,
      sep = "_"
    ),
    perl = TRUE
  )

  cmdisplay <- gsub(
    "[,\\s]+",
    "_",
    paste(
      "confusion_matrix_for_display",
      stratified_by,
      reference_group,
      sep = "_"
    ),
    perl = TRUE
  )

  confusion_matrix_input_id <- gsub(
    "[,\\s]+",
    "_",
    paste("confusionMatrixInput", stratified_by, reference_group, sep = "_"),
    perl = TRUE
  )

  confusion_matrix_input <- create_confusion_matrix_as_input_element(
    confusion_matrix_input_id,
    cmdisplay,
    real_competing = TRUE,
    real_censored = TRUE
  )

  censored_assumption_radiobutton_input_id <- gsub(
    "[,\\s]+",
    "_",
    paste(
      "censored_assumption_radiobutton",
      stratified_by,
      reference_group,
      sep = "_"
    ),
    perl = TRUE
  )

  competing_assumption_radiobutton_input_id <- gsub(
    "[,\\s]+",
    "_",
    paste(
      "competing_assumption_radiobutton",
      stratified_by,
      reference_group,
      sep = "_"
    ),
    perl = TRUE
  )

  saveRDS(
    all_time_horizons_confusion_matrix_for_display_data,
    "all_time_horizons_confusion_matrix_for_display_data.rds"
  )

  confusion_matrix_for_display <- htmltools::tagList(
    render_confusion_matrix_for_probs_histogram(
      all_time_horizons_confusion_matrix_for_display_data,
      cmdisplay
    )
  )

  # TODO: move one_model_one_pop
  one_model_one_pop <- create_nested_data_frame_with_probs_distributions(
    probs = list("train" = as.vector(rdata$pred5)),
    reals = list("train" = rdata$status_num),
    times = rdata$time,
    fixed_horizon_times = c(1, 3, 5),
    stratified_by = stratified_by,
    by = by
  )

  reference_group <- "train" # TODO: move it

  # print("stratified_by")
  # print(stratified_by)

  # print("input to hist_predicted probs distribution")
  # print(
  #   one_model_one_pop[[reference_group]][[stratified_by]]$probs_distribution[[3]]
  # )

  # print("by entered to create_probs_hist_predicted_of_probs_distribution")
  # print(by)

  hist_predicted <- create_probs_hist_predicted_of_probs_distribution(
    probs_distribution = one_model_one_pop[[reference_group]][[
      stratified_by
    ]]$probs_distribution[[3]],
    stratified_by = stratified_by,
    population_name = reference_group,
    by = by,
    fixed_probability_threshold = fixed_probability_threshold
  )

  crosstalk_object <- crosstalk::bscols(
    widths = c(6, 6, 6, 6),
    htmltools::div(
      rangeFilter(
        cmdisplay,
        filteredperformancetableId,
        stratified_by,
        ifelse(
          stratified_by == "probability_threshold",
          "Probability Threshold: ",
          "Predicted Positive (Rate)"
        ),
        0,
        1,
        step = by
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
      htmltools::div(
        htmltools::HTML("<b>Competing Events:</b>"),
        htmltools::tags$label(
          `for` = competing_assumption_radiobutton_input_id,
          htmltools::tags$input(
            id = paste0(
              competing_assumption_radiobutton_input_id,
              "_excluded"
            ),
            name = competing_assumption_radiobutton_input_id,
            value = "excluded",
            checked = TRUE,
            type = "radio",
            onclick = "ManipulateConfusionMatrixInputTables(this)" #onClickFunction
          ),
          "Excluded"
        ),
        htmltools::tags$label(
          `for` = competing_assumption_radiobutton_input_id,
          htmltools::tags$input(
            id = paste0(
              competing_assumption_radiobutton_input_id,
              "_non_event"
            ),
            name = competing_assumption_radiobutton_input_id,
            value = "non_event",
            type = "radio",
            onclick = "ManipulateConfusionMatrixInputTables(this)"
          ),
          "As Non Event"
        )
      ),
      htmltools::div(
        htmltools::HTML("<b>Censored Events:</b>"),
        htmltools::tags$label(
          `for` = censored_assumption_radiobutton_input_id,
          htmltools::tags$input(
            id = paste0(
              censored_assumption_radiobutton_input_id,
              "_excluded"
            ),
            name = censored_assumption_radiobutton_input_id,
            value = "excluded",
            type = "radio",
            checked = TRUE,
            onclick = "ManipulateConfusionMatrixInputTables(this)"
          ),
          "Excluded"
        ),
        htmltools::tags$label(
          `for` = censored_assumption_radiobutton_input_id,
          htmltools::tags$input(
            id = paste0(
              censored_assumption_radiobutton_input_id,
              "_adjusted"
            ),
            name = censored_assumption_radiobutton_input_id,
            value = "adjusted",
            type = "radio",
            onclick = "ManipulateConfusionMatrixInputTables(this)"
          ),
          "Adjusted"
        )
      )
    ),
    htmltools::div(confusion_matrix_input),
    htmltools::div(confusion_matrix_for_display),
    hist_predicted,
    htmltools::tags$script(htmltools::HTML(script))
    # )
  )

  # saveRDS(crosstalk_object, paste0("crosstalk_object_", stratified_by, ".rds"))

  crosstalk_object
}

extract_all_time_horizons_confusion_matrix_for_display_data <- function(
  nested_data_frame_with_probs_distributions
) {
  nested_data_frame_with_probs_distributions |>
    purrr::map(
      \(reference_group) {
        reference_group |>
          purrr::map(
            \(stratified_by_data) {
              stratified_by_data |>
                dplyr::select(
                  time_horizon,
                  confusion_matrix_for_display_data
                ) |>
                tidyr::unnest(confusion_matrix_for_display_data)
            }
          )
      }
    )
}

create_full_hist_dat <- function(probs_distribution) {
  real_positives_hist_dat <- probs_distribution$real_positives |>
    dplyr::rename("real_positives" = counts, "cat" = "mids") |>
    select(cat, real_positives, text, text_range, lower_bound, upper_bound)

  real_negatives_hist_dat <- probs_distribution$real_negatives |>
    dplyr::rename("real_negatives" = counts, "cat" = "mids") |>
    select(cat, real_negatives, text, text_range, lower_bound, upper_bound)

  real_competing_hist_dat <- probs_distribution$real_competing |>
    dplyr::rename("real_competing" = counts, "cat" = "mids") |>
    select(cat, real_competing, text, text_range, lower_bound, upper_bound)

  real_censored_hist_dat <- probs_distribution$real_competing |>
    dplyr::rename("real_censored" = counts, "cat" = "mids") |>
    select(cat, real_censored, text, text_range, lower_bound, upper_bound)

  full_hist_dat <- dplyr::left_join(
    real_positives_hist_dat,
    real_negatives_hist_dat,
    by = c('cat', 'text_range', 'lower_bound', 'upper_bound'),
    suffix = c("_real_positives", "_real_negatives")
  ) |>
    dplyr::left_join(
      real_competing_hist_dat |>
        dplyr::rename("text_real_competing" = "text"),
      by = c('cat', 'text_range', 'lower_bound', 'upper_bound')
    ) |>
    dplyr::left_join(
      real_censored_hist_dat |>
        dplyr::rename("text_real_censored" = "text"),
      by = c('cat', 'text_range', 'lower_bound', 'upper_bound')
    )
}

# TODO: Create a function: create_probs_histogram_out_of_probs_distribution

create_probs_hist_predicted_of_probs_distribution <- function(
  probs_distribution,
  population_name,
  stratified_by,
  by,
  fixed_probability_threshold = fixed_probability_threshold
) {
  # View(names(probs_distribution))

  full_hist_dat <- create_full_hist_dat(probs_distribution)

  reference_group <- "train"

  saveRDS(full_hist_dat, "full_hist_dat.rds")

  hist_predicted <- r2d3::r2d3(
    data = full_hist_dat,
    script = "probs_hist_times.js",
    width = 350,
    height = 350,
    container = 'div',
    elementId = "histogramPredicted",
    options = list(
      by = by #,
      # initial_zoom = one_model_one_pop$train$probability_threshold$initial_zoom[[1]])
    )
  )

  hist_predicted
}


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


create_breaks_values <- function(probs_vec, stratified_by, by) {
  if (stratified_by != "probability_threshold") {
    breaks <- stats::quantile(probs_vec, probs = rev(seq(0, 1, by = by)))
  } else {
    breaks <- round(
      seq(0, 1, by = by),
      digits = nchar(format(by, scientific = FALSE))
    )
  }

  breaks
}

create_probs_distribution_list <- function(
  probs,
  reals,
  times,
  by,
  stratified_by,
  fixed_horizon_times
) {
  # View(probs)
  # View(reals)

  0:2 |>
    purrr::map(
      function(x) {
        breaks <- create_breaks_values(
          probs,
          stratified_by,
          by
        )

        create_mids_and_counts_data_from_probs_times(
          probs = probs[reals == x],
          breaks = breaks,
          stratified_by = stratified_by,
          weights = NA,
          by = by
        ) |>
          add_bounds_variables(by, stratified_by) |>
          add_text_variables_to_probs_distribution_data(
            stratified_by,
            by = by
          ) |>
          dplyr::mutate(reference_group = "Train")
      }
    ) |>
    purrr::set_names(
      "real_positives",
      "real_negatives",
      "real_competing"
    )
}




f <- ggplot(mydata %>% 
              filter(gender=="Female"),
            aes(x = avg_glucose_level)) +geom_histogram(aes(y = ..density..),
                 binwidth = 10)