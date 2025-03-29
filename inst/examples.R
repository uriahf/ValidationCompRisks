library(survival)

data(aidssi, package = "mstate") # AIDS data set
aidssi$event <- factor(aidssi$status, 0:2, c("censored", "AIDS", "SI"))

ajfit <- survfit(Surv(time, event) ~ 1, data = aidssi)

# AJ estimator

predictions <- summary(ajfit, times = 0:10)$pstate |>
  tibble::as_tibble() |>
  tibble::add_column(time_horizon = 0:10)

names(predictions)[1:length(ajfit$states)] <- c(ajfit$states)

predictions

# Regression coxph

library(magrittr)

cfit0 <- coxph(Surv(time, event) ~ ccr5, data = aidssi, id = patnr)


predictions <- summary(
  survfit(cfit0, newdata = aidssi),
  times = 0:1
) %$%
  pstate |>
  purrr::array_tree() |>
  purrr::set_names(
    paste("time_horizon", 0:1, sep = "_")
  ) |>
  purrr::map(
    \(time_horizon)
      time_horizon |>
        purrr::map(
          \(state)
            set_names(
              state,
              ajfit$states
            )
        )
  )

predictions |>
  View()

naniar::vis_miss(aidssi)

# extract only relevant predictionns

prob_aids <- predictions |>
  purrr::map(
    \(time_horizon) {
      time_horizon |>
        purrr::map(
          \(observation)
            observation %$%
              AIDS
        ) |>
        purrr::simplify()
    }
  )


# predictions 5 years

reals_aids <- aidssi$event[!is.na(aidssi$ccr5)]
levels(reals_aids) <- c(0, 1, 2)

length(prob_aids$time_horizon_1)
length(reals_aids)

render_dynamic_histogram_report(
  probs = list("binary_model" = prob_aids$time_horizon_5),
  reals = list("aids_population" = reals_aids),
  times = aidssi$time[!is.na(aidssi$ccr5)],
  fixed_horizon_times = c(1, 2, 3, 4, 5),
  by = 0.1,
  stratified_by = c("probability_threshold", "ppcr")
)
